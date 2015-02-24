module TigerSemant2
  ()
  where

import qualified TigerSemantTypes as TSty
import qualified TigerLexer       as TLex
import qualified TigerAbsyn       as TAbs
import qualified TigerSymbol2     as TSym
import qualified TigerTemp2       as TTmp
import qualified TigerTranslate2  as TTra
import qualified FrontEnd         as Frt
import qualified Data.Map         as Map
import qualified TigerParser2     as TPar
import Control.Monad.Except
import Data.IORef.MonadIO
import Data.List

type Frontend = Frt.Frontend
type Venv = Frt.Venv
type Tenv = Frt.Tenv

type GexpTy = (TTra.Gexp, TSty.Ty)

cyclicTypeError :: TLex.AlexPosn -> [String] -> Frt.SemantError
cyclicTypeError pos types = Frt.SE pos $ Frt.TypeLoop types

notCallableError :: TLex.AlexPosn -> String -> Frt.SemantError
notCallableError pos str = Frt.SE pos $ Frt.NotCallable str

typeMisMatchError :: TLex.AlexPosn -> String -> String -> Frt.SemantError
typeMisMatchError pos str1 str2 = Frt.SE pos $ Frt.TypeMismatch str1 str2

argumentNameError :: TLex.AlexPosn -> String -> String -> Frt.SemantError
argumentNameError pos str1 str2 = Frt.SE pos $ Frt.ArgumentName str1 str2

undefinedBinop :: TLex.AlexPosn -> String -> String -> Frt.SemantError
undefinedBinop pos str1 str2 = Frt.SE pos $ Frt.UndefinedBinop str1 str2

undefinedError :: TLex.AlexPosn -> String -> Frt.SemantError
undefinedError pos str = Frt.SE pos $ Frt.Undefined str

argumentCountError :: TLex.AlexPosn -> Int -> Int -> Frt.SemantError
argumentCountError pos c1 c2 = Frt.SE pos $ Frt.ArgumentCount c1 c2

enterLoop :: Frontend ()
enterLoop = do (v, t, l, frags) <- Frt.semantStGet
               Frt.semantStPut (v, t, l+1, frags)

exitLoop :: Frontend ()
exitLoop = do (v, t, l, frags) <- Frt.semantStGet
              Frt.semantStPut (v, t, l-1, frags)

actualTy' :: TLex.AlexPosn -> TSty.Ty -> [TSty.Ty] -> Frontend TSty.Ty
actualTy' pos a@(TSty.Name (_, iorefmaybety)) searched =
  do if a `elem` searched
        then throwError $ cyclicTypeError pos $ map show searched
        else do maybety <- liftIO $ readIORef iorefmaybety
                case maybety of
                  (Just ty) -> actualTy' pos ty (a:searched)
                  Nothing   -> error "Compiler error: fatal error in cyclic type detection."

actualTy :: TLex.AlexPosn -> TSty.Ty -> Frontend TSty.Ty
actualTy pos ty = actualTy' pos ty []

withBinding :: Venv -> Tenv -> Frontend a -> Frontend a
withBinding v t checker = do s@(_, _, l, fs) <- Frt.semantStGet
                             Frt.semantStPut (v, t, l, fs)
                             a <- checker
                             Frt.semantStPut s
                             return a

findFirstDiffInLists :: Eq a => [a] -> [a] -> Maybe Int
findFirstDiffInLists la lb | la == lb  = Nothing
                           | length la /= length lb = error "Compiler error: list a and list b must be the"
                                                            " same length in findFirstDiffInLists."
                           | otherwise = let equalities = zipWith (==) la lb
                                         in  (False) `elemIndex` equalities

transdec :: TTra.Level -> Maybe TTmp.Label -> TAbs.Dec -> Frontend (Venv, Tenv, [TTra.Gexp])
transdec = undefined

transexp :: TTra.Level -> Maybe TTmp.Label -> TAbs.Exp -> Frontend GexpTy
transexp lvl lab absexp = 
  let g (TAbs.VarExp v)   = transvar lvl lab v
      g (TAbs.NilExp _)   = return (TTra.nilGexp, TSty.Nil)
      g (TAbs.IntExp (v, _)) = do gexp <- TTra.intExp v
                                  return (gexp, TSty.INT)
      g (TAbs.StringExp (v, _)) = do gexp <- TTra.stringExp v
                                     return (gexp, TSty.String)
      g (TAbs.SeqExp []) = error "Compiler error: fatal error in sequence"
                                 " expression type checking"
      g (TAbs.SeqExp (epos:[])) = g $ fst epos
      g (TAbs.SeqExp (epos:eposes)) = do (ge, _) <- g $ fst epos
                                         (ge', ty) <- g $ TAbs.SeqExp eposes
                                         ge'' <- TTra.constructEseq ge ge'
                                         return (ge'', ty)
      g (TAbs.AppExp {TAbs.appFunc=funcsym, TAbs.appArgs=funcargs, TAbs.appPos=pos}) = 
        do (v, _, _, _) <- Frt.semantStGet
           case Map.lookup funcsym v of
             Just (Frt.FunEntry {Frt.funLevel=funlvl
                                ,Frt.funLabel=funlab
                                ,Frt.funFormals=funformals
                                ,Frt.funResult=funresult}) ->
               do (ges, tys) <- liftM unzip $ mapM g funcargs
                  let argposes = map TPar.extractPosition funcargs
                  argtys <- mapM (uncurry actualTy) (zip argposes tys)
                  formaltys <- mapM (actualTy pos) (map fst funformals)
                  case findFirstDiffInLists argtys formaltys of
                    Nothing -> do gexp <- TTra.callFunction funlab lvl funlvl ges
                                  return (gexp, funresult)
                    Just idx -> throwError $ typeMisMatchError (TPar.extractPosition $ funcargs !! idx) (show $ argtys !! idx) (show $ formaltys !! idx)
             Just _ -> throwError $ notCallableError pos $ TSym.name funcsym
             Nothing -> throwError $ undefinedError pos $ TSym.name funcsym
      g (TAbs.OpExp {TAbs.opLeft=lexp, TAbs.opOper=op, TAbs.opRight=rexp, TAbs.opPos=pos}) =
        do (lgexp, lty) <- g lexp
           (rgexp, rty) <- g rexp
           if (lty == rty)
              then case lty of
                     TSty.INT -> do { gexp <- op2fun op lgexp rgexp; return (gexp, TSty.INT) }
                     TSty.String -> do { gexp <- op2funstr op lgexp rgexp; return (gexp, TSty.INT) }
                     TSty.Nil -> throwError $ undefinedBinop pos (show TSty.Nil) (show rty)
                     TSty.Unit -> throwError $ undefinedBinop pos (show TSty.Unit) (show rty)
                     _ -> case op of
                            TAbs.EqOp -> do { gexp <- op2fun op lgexp rgexp; return (gexp, TSty.INT) }
                            TAbs.NeqOp -> do { gexp <- op2fun op lgexp rgexp; return (gexp, TSty.INT) }
                            _ -> throwError $ undefinedBinop pos (show lty) (show rty)
              else throwError $ typeMisMatchError pos (show lty) (show rty)
        where op2fun TAbs.EqOp = TTra.eqCmp
              op2fun TAbs.NeqOp = TTra.notEqCmp
              op2fun TAbs.LtOp  = TTra.lessThan
              op2fun TAbs.LeOp  = TTra.lessThanOrEq
              op2fun TAbs.GtOp  = flip TTra.lessThan
              op2fun TAbs.GeOp  = flip TTra.lessThanOrEq
              op2fun o          = TTra.arithmetic o
              op2funstr TAbs.EqOp = TTra.eqStr
              op2funstr TAbs.NeqOp = TTra.notEqStr
              op2funstr TAbs.LtOp = TTra.strLessThan
              op2funstr TAbs.LeOp = TTra.strLessThanOrEq
              op2funstr TAbs.GtOp = flip TTra.strLessThan
              op2funstr TAbs.GeOp = flip TTra.strLessThanOrEq
              op2funstr _         = error "Compiler error: impossible operator on string."
      g (TAbs.RecordExp {TAbs.recordFields=efields, TAbs.recordTyp=typsym, TAbs.recordPos=pos}) =
        do (_, t, _, _) <- Frt.semantStGet
           case Map.lookup typsym t of
             Nothing -> throwError $ undefinedError pos $ TSym.name typsym
             Just (TSty.Record ty) -> do let typefields = fst ty 
                                         let (fieldnames, fieldtys) = unzip typefields
                                         let (efieldnames, eexps, eposes) = unzip3 efields
                                         if length efieldnames == length fieldnames
                                            then case findFirstDiffInLists efieldnames fieldnames of
                                                   Nothing  -> do (gexps, exptys) <- liftM unzip $ mapM g eexps
                                                                  case findFirstDiffInLists exptys fieldtys of
                                                                    Nothing  -> do finalgexp <- TTra.createRecord gexps
                                                                                   return (finalgexp, TSty.Record ty)
                                                                    Just idx -> throwError $ typeMisMatchError
                                                                                             (TPar.extractPosition $ eexps !! idx)
                                                                                             (show $ exptys !! idx)
                                                                                             (show $ fieldtys !! idx)
                                                   Just idx -> throwError $ argumentNameError (TPar.extractPosition $ eexps !! idx)
                                                                                              (TSym.name $ efieldnames !! idx)
                                                                                              (TSym.name $ fieldnames !! idx)
                                            else throwError $ argumentCountError pos (length efieldnames) (length fieldnames)
             Just _ -> throwError $ typeMisMatchError pos ("Record") (TSym.name typsym)
  in  g absexp

transvar :: TTra.Level -> Maybe TTmp.Label -> TAbs.Var -> Frontend GexpTy
transvar = undefined

transty :: TAbs.Ty -> Frontend TSty.Ty
transty = undefined
