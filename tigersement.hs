module TigerSement
       (
         transProg
       , SementError
       ) where

import qualified TigerSementTypes as TigSTy
import qualified TigerLexer as TLex
import qualified TigerAbsyn as TAbsyn
import qualified TigerSymbol as TSym
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad.Error

type SementError = (TLex.AlexPosn, String)
type ExpTy = TigSTy.Ty

data EnvEntry = VarEntry {varTy::TigSTy.Ty, varReadOnly::Bool}
              | FunEntry {funLevel::Int, funFormals::[TigSTy.Ty], funResult::TigSTy.Ty}

type Venv    = Map.Map TSym.Symbol EnvEntry
type Tenv    = Map.Map TSym.Symbol TigSTy.Ty
type SementStates = (Venv, Tenv, TigSTy.Uniq, Bool)

type SementState = StateT SementStates (Either SementError)

emptyState = (Map.empty, Map.empty, 0 :: TigSTy.Uniq, False)

binOpError              = "Binary operation on non-int types."
undefinedFuncError name = "Calling undefined function: " ++ name
undefinedRecError  name = "Creating undefined record type: " ++ name
recSymbolMisMatch       = "Symbol mismatch when creating record"
recTypeMisMatch         = "Type mismatch when creating record"
illegalRecType name     = "Attempting to create record with non-record type: " ++ name
assignmentTypeMismatch  = "Attempting to assign mismatching type to variable"

-- Generates a new value of Uniq type
genUnique :: SementState TigSTy.Uniq
genUnique = do (venv, tenv, a, inloop) <- get
               put (venv, tenv, a+1, inloop)
               return a

actualTy :: TigSTy.Ty -> TigSTy.Ty
actualTy (TigSTy.Name (_, Just ty)) = actualTy ty
actualTy (TigSTy.Name (_, Nothing)) = error "Impossible name type"
actualTy otherType = otherType

transVar :: TAbsyn.Var -> SementState ExpTy
transVar = undefined 

transExp :: TAbsyn.Exp -> SementState ExpTy
-- Binary Operation
transExp TAbsyn.OpExp{TAbsyn.opLeft=el, TAbsyn.opOper=op, TAbsyn.opRight=er, TAbsyn.opPos=pos}
  = do tl <- transExp el
       tr <- transExp er
       if (tl == tr) && (tl == TigSTy.INT)
         then return TigSTy.INT
         else throwError (pos, binOpError)
-- Variable
transExp (TAbsyn.VarExp var) = do tvar <- transVar var
                                  return tvar
-- String
transExp (TAbsyn.StringExp _) = return TigSTy.String
-- Sequence expression
transExp (TAbsyn.SeqExp ((e, _):[]))
  = do ty <- transExp e
       return ty
transExp (TAbsyn.SeqExp ((e, _):es))
  = do _ <- transExp e
       transExp (TAbsyn.SeqExp es)
transExp (TAbsyn.SeqExp []) = error "Impossible SeqExp case"
-- Function application
transExp TAbsyn.AppExp{TAbsyn.appFunc=funcsymbol, TAbsyn.appArgs=es, TAbsyn.appPos=pos}
  = do (venv, _, _, _) <- get
       if funcsymbol `Map.member` venv
          then do {tys <- mapM transExp es; return $ last tys}
          else throwError (pos, undefinedFuncError $ TSym.name funcsymbol) 
-- Record creation
transExp TAbsyn.RecordExp{TAbsyn.recordFields=efields, TAbsyn.recordTyp=recordtypesymbol, TAbsyn.recordPos=pos}
  = do (_, tenv, _, _) <- get
       case Map.lookup recordtypesymbol tenv of
         Just rty@(TigSTy.Record (symboltypair, _)) -> checkRecordInternal symboltypair rty
         Just nty@(TigSTy.Name (namesymbol, _)) -> do let aty = actualTy nty
                                                      if isRecord aty
                                                         then do let TigSTy.Record (symboltypair, _) = aty
                                                                 checkRecordInternal symboltypair aty
                                                         else throwError (pos, illegalRecType $ TSym.name namesymbol)
         _ -> throwError (pos, undefinedRecError $ TSym.name recordtypesymbol)
    where fst'  (a, _, _) = a
          snd'  (_, b, _) = b
          thrd' (_, _, c) = c
          isRecord (TigSTy.Record _) = True
          isRecord _ = False
          checkRecordInternal symboltypair rty = do let symbols = map fst' efields
                                                    let exprs   = map snd' efields
                                                    let recsymbols = map fst symboltypair
                                                    let rectys     = map snd symboltypair
                                                    tys <- mapM transExp exprs
                                                    if symbols==recsymbols
                                                       then if tys==rectys
                                                               then return rty
                                                               else throwError (pos, recTypeMisMatch)
                                                       else throwError (pos, recSymbolMisMatch)
transExp TAbsyn.AssignExp{TAbsyn.assignVar=var, TAbsyn.assignExp=exp, TAbsyn.assignPos=pos}
  = do tvar <- transVar var
       texp <- transExp exp
       if tvar == texp
          then return $ TigSTy.Unit
          else throwError (pos, assignmentTypeMismatch)


transDec :: TAbsyn.Dec -> SementState ()
transDec = undefined

transTy :: TAbsyn.Ty -> SementState TigSTy.Ty
transTy = undefined

transProg' :: SementStates -> TAbsyn.Program -> Either SementError ()
transProg' initialS (TAbsyn.Pexp e) = do _ <- evalStateT (transExp e) initialS
                                         return ()
transProg' initialS (TAbsyn.Pdecs (d:decs)) = do (_, s) <- runStateT (transDec d) initialS
                                                 transProg' s (TAbsyn.Pdecs decs)
transProg' _ (TAbsyn.Pdecs []) = return ()

transProg :: TAbsyn.Program -> Either SementError ()
transProg = transProg' emptyState
