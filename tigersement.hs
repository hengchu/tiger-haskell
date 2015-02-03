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
import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Error
import Data.IORef
import Data.List

data SementError = SE (TLex.AlexPosn, String) deriving (Show, Eq)
type ExpTy = TigSTy.Ty

data EnvEntry = VarEntry {varTy::TigSTy.Ty, varReadOnly::Bool}
              | FunEntry {funFormals::[TigSTy.Ty], funResult::TigSTy.Ty}

type Venv    = Map.Map TSym.Symbol EnvEntry
type Tenv    = Map.Map TSym.Symbol TigSTy.Ty
type LoopLevel = Int
type SementStates = (Venv, Tenv, TigSTy.Uniq, LoopLevel)

type SementState = StateT SementStates (ErrorT SementError IO)

instance Error SementError where
  noMsg    = SE(TLex.AlexPn 0 0 0, "Unknown error")
  strMsg s = SE(TLex.AlexPn 0 0 0, s)

binOpNonMatchingError                   = "Binary operation on non-matching types"
binNilOp                                = "Binary operation on two nil types"
binUnitOp                               = "Binary operation on two unit types"
arrayRecOrderOp                         = "Order comparison operation on array or record type"
undefinedFuncError name                 = "Calling undefined function: " ++ name
undefinedRecError  name                 = "Creating undefined record type: " ++ name
recSymbolMisMatch                       = "Symbol mismatch when creating record"
recTypeMisMatch                         = "Type mismatch when creating record"
illegalRecType name                     = "Attempting to create record with non-record type: " ++ name
assignmentTypeMismatch                  = "Attempting to assign mismatching type to variable"
illegalIfTestType                       = "If expression has non-int test type"
ifthenTypeMismatch                      = "If expression then branch and else branch have different types"
illegalWhileTestType                    = "While expression has non-int test type"
whileLoopBodyUnit                       = "While expression should have unit typed body"
forLoopBoundaryType                     = "For loop has non-int low or high type"
forLoopBodyUnit                         = "For loop should have unit typed body"
breakExpNotInLoop                       = "Break expression not in any loop"
undefinedArrError name                  = "Creating array for undefined type: " ++ name
nonarrayCreation  name                  = "Attempting to create array for non-array type: " ++ name
arrayInitMismatch                       = "Array initialization expression does not match actual type"
arraySizeNotInt                         = "Array size expression is not int-typed"
undefinedFuncRetType name               = "Undefined function return type: " ++ name
undefinedFuncParamType                  = "Undefined function parameter type(s)"
procedureHasNonUnitRetType name         = "Procedure: " ++ name ++ " has non-unit return type"
functionRetBodyTypeMismatch name        = "Function: " ++ name ++ " return type does not match body expression type"
undefinedVarDecType name                = "Declared variable type: " ++ name ++ " is not defined"
variableInitDeclaredTypeMismatch name   = "Variable: " ++ name ++ " init expression type does not match declared type"
undefinedTypeInNameTy name              = "Undefined type: " ++ name ++ " in name type declaration"
undefinedTypeInArrayTy name             = "Undefined type: " ++ name ++ " in array type declaration"
undefinedTypeInRecordTy name            = "Undefined type: " ++ name ++ " in record type declaration"
variableUndefined name                  = "Variable is not in scope: " ++ name
nameIsNotVariable name                  = name ++ " is not a variable"
fieldVarOnNonRecordType name            = "Trying to access field: " ++ name ++ " on non record type"
recordTypeHasNoField name               = "Record type does not have field: " ++ name
subscriptVarOnNonArrayType              = "Trying to subscript non-array type"
subscriptIsNotIntType                   = "Subscript is not int-typed"
circularType                            = "Circular type discovered!"
redefiningType name                     = "Redefining type: " ++ name
variableIsNotFunction name              = "Variable is not function: " ++ name
parameterTypesDoesNotMatchFunctionTypes = "Parameter types does not match function declared types"

-- Generates a new value of Uniq type
genUnique :: SementState TigSTy.Uniq
genUnique = do (venv, tenv, a, inloop) <- get
               put (venv, tenv, a+1, inloop)
               return a

actualTy' :: TLex.AlexPosn -> TigSTy.Ty -> [TigSTy.Ty] -> SementState TigSTy.Ty
actualTy' pos a@(TigSTy.Name (_, iorefmaybety)) searched = do if a `elem` searched
                                                                 then throwError $ SE(pos, circularType ++ ": "  ++ (show a))
                                                                 else do maybety <- liftIO $ readIORef iorefmaybety
                                                                         case maybety of
                                                                           (Just ty) -> actualTy' pos ty (a:searched)
                                                                           Nothing -> error "Impossible name type"
actualTy' pos otherType searched = if (otherType `elem` searched) 
                                      then throwError $ SE(pos, circularType ++ ": " ++ (show otherType))
                                      else return otherType

actualTy :: TLex.AlexPosn -> TigSTy.Ty -> SementState TigSTy.Ty
actualTy pos a = actualTy' pos a []

isRecord (TigSTy.Record _) = True
isRecord _ = False

isArray (TigSTy.Array _) = True
isArray _ = False

transVar :: TAbsyn.Var -> SementState ExpTy
transVar (TAbsyn.SimpleVar(s, pos)) = do (v, _, _, _) <- get
                                         case Map.lookup s v of
                                           Nothing -> throwError $ SE(pos, variableUndefined $ TSym.name s)
                                           Just VarEntry{varTy=ty, varReadOnly=_} -> return ty
                                           _ -> throwError $ SE(pos, nameIsNotVariable $ TSym.name s)
transVar (TAbsyn.FieldVar(var, s, pos)) = do varty <- transVar var
                                             aty <- actualTy pos varty
                                             if (isRecord aty)
                                                then do let TigSTy.Record(symboltypairs, _) = aty
                                                        case find (\(sym, _) -> sym == s) symboltypairs of
                                                          Just (_, ty) -> return ty
                                                          Nothing -> throwError $ SE(pos, recordTypeHasNoField $ TSym.name s)
                                                else throwError $ SE(pos, fieldVarOnNonRecordType $ TSym.name s)
transVar (TAbsyn.SubscriptVar(var, exp, pos)) = do varty <- transVar var
                                                   aty <- actualTy pos varty
                                                   if (isArray aty)
                                                      then do let TigSTy.Array(ty, _) = aty
                                                              expty <- transExp exp
                                                              if (expty == TigSTy.INT)
                                                                  then return ty
                                                                  else throwError $ SE(pos, subscriptIsNotIntType)
                                                      else throwError $ SE(pos, subscriptVarOnNonArrayType)

enterLoop :: SementState ()
enterLoop = do (venv, tenv, uniq, looplevel) <- get
               put (venv, tenv, uniq, looplevel+1)

exitLoop :: SementState ()
exitLoop = do (venv, tenv, uniq, looplevel) <- get
              put (venv, tenv, uniq, looplevel-1)

withBinding :: Venv -> Tenv -> SementState a -> SementState a
withBinding venv tenv checker = do s@(_, _, uniq, looplevel) <- get
                                   put (venv, tenv, uniq, looplevel)
                                   a <- checker
                                   put s
                                   return a

transExp :: TAbsyn.Exp -> SementState ExpTy
-- Binary Operation
transExp TAbsyn.OpExp{TAbsyn.opLeft=el, TAbsyn.opOper=op, TAbsyn.opRight=er, TAbsyn.opPos=pos}
  = do tl <- transExp el
       tr <- transExp er
       if (tl == tr)
          then case tl of
                 TigSTy.INT    -> return TigSTy.INT
                 TigSTy.String -> return TigSTy.INT
                 TigSTy.Nil    -> throwError $ SE(pos, binNilOp)
                 TigSTy.Unit   -> throwError $ SE(pos, binUnitOp)
                 _             -> case op of
                                    TAbsyn.EqOp  -> return TigSTy.INT
                                    TAbsyn.NeqOp -> return TigSTy.INT
                                    _            -> throwError $ SE(pos, arrayRecOrderOp)
          else throwError $ SE(pos, binOpNonMatchingError)
-- Variable
transExp (TAbsyn.VarExp var) = do tvar <- transVar var
                                  return tvar
-- Nil expression
transExp TAbsyn.NilExp = return TigSTy.Nil
-- Int expression
transExp (TAbsyn.IntExp _) = return TigSTy.INT
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
       case Map.lookup funcsymbol venv of
          Just (FunEntry{funFormals=formaltys, funResult=resty}) -> do tys <- mapM transExp es
                                                                       if tys==formaltys
                                                                          then return resty
                                                                          else throwError $ SE(pos, parameterTypesDoesNotMatchFunctionTypes)
          Just _ -> throwError $ SE(pos, variableIsNotFunction $ TSym.name funcsymbol)
          Nothing -> throwError $ SE(pos, undefinedFuncError $ TSym.name funcsymbol) 
-- Assignment
transExp TAbsyn.AssignExp{TAbsyn.assignVar=var, TAbsyn.assignExp=exp, TAbsyn.assignPos=pos}
  = do tvar <- transVar var
       texp <- transExp exp
       if tvar == texp
          then return $ TigSTy.Unit
          else throwError $ SE(pos, assignmentTypeMismatch)
-- If expression
transExp TAbsyn.IfExp{TAbsyn.ifTest=testexp, TAbsyn.ifThen=thenexp, TAbsyn.ifElse=maybeElse, TAbsyn.ifPos=pos}
  = do testty <- transExp testexp
       thenty <- transExp thenexp
       if testty == TigSTy.INT
          then if isJust maybeElse
               then do let elseexp = fromJust maybeElse
                       elsety <- transExp elseexp
                       if thenty==elsety
                          then return elsety
                          else throwError $ SE(pos, ifthenTypeMismatch)
               else return thenty
          else throwError $ SE(pos, illegalIfTestType)
-- While expression
transExp TAbsyn.WhileExp{TAbsyn.whileTest=testexp, TAbsyn.whileBody=bodyexp, TAbsyn.whilePos=pos}
  = do testty <- transExp testexp
       if testty == TigSTy.INT
          then do bodyty <- checkWhileInternal
                  if bodyty == TigSTy.Unit
                     then return TigSTy.Unit
                     else throwError $ SE(pos, whileLoopBodyUnit)
          else throwError $ SE(pos, illegalWhileTestType)
  where checkWhileInternal = do enterLoop
                                bodyty <- transExp bodyexp
                                exitLoop
                                return bodyty
-- For expression
transExp TAbsyn.ForExp{TAbsyn.forVar=vardec, TAbsyn.forLo=lowexp, TAbsyn.forHi=highexp, TAbsyn.forBody=bodyexp, TAbsyn.forPos=pos}
  = do (venv, tenv, _, _) <- get
       let venv' = Map.insert (TAbsyn.vardecName vardec) VarEntry{varTy=TigSTy.INT, varReadOnly=False} venv
       lowty <- transExp lowexp
       highty <- transExp highexp
       if (lowty == highty) && (lowty == TigSTy.INT)
          then withBinding venv' tenv checkForInternal
          else throwError $ SE(pos, forLoopBoundaryType)
    where checkForInternal = do enterLoop
                                bodyty <- transExp bodyexp
                                exitLoop
                                if (bodyty == TigSTy.Unit)
                                   then return TigSTy.Unit
                                   else throwError $ SE(pos, forLoopBodyUnit)
-- Break expression
transExp TAbsyn.BreakExp{TAbsyn.breakPos=pos}
  = do (_, _, _, inloop) <- get
       if (inloop > 0)
          then return TigSTy.Unit
          else throwError $ SE(pos, breakExpNotInLoop)
-- Let expression
transExp TAbsyn.LetExp{TAbsyn.letDecs=decs, TAbsyn.letBody=bodyexp, TAbsyn.letPos=_}
  = do (venv', tenv') <- transDecs decs
       withBinding venv' tenv' checkLetExpInternal
  where transDecs (d:[])   = do (v, t) <- transDec d
                                return (v, t)
        transDecs (d:ds)   = do (v, t) <- transDec d
                                withBinding v t (transDecs ds)
        transDecs []       = do (v, t, _, _) <- get
                                return (v, t)
        checkLetExpInternal = transExp bodyexp
-- Array expression
transExp TAbsyn.ArrayExp{TAbsyn.arrayTyp=arraytypesymbol, TAbsyn.arraySize=szexp, TAbsyn.arrayInit=initexp, TAbsyn.arrayPos=pos}
  = do (_, tenv, _, _) <- get
       case Map.lookup arraytypesymbol tenv of
          Just ty -> checkArrayExpInternal ty
          Nothing -> throwError $ SE(pos, undefinedArrError $ TSym.name arraytypesymbol)
    where checkArrayExpInternal ty = do aty <- actualTy pos ty
                                        szty <- transExp szexp
                                        initty <- transExp initexp
                                        if (isArray aty)
                                           then do let TigSTy.Array (internalTy, _) = aty
                                                   ainternalTy <- actualTy pos internalTy
                                                   if (ainternalTy == initty)
                                                      then if (szty == TigSTy.INT)
                                                           then return aty
                                                           else throwError $ SE(pos, arraySizeNotInt)
                                                      else throwError $ SE(pos, arrayInitMismatch)
                                           else throwError $ SE(pos, nonarrayCreation $ TSym.name arraytypesymbol)
          isArray (TigSTy.Array _) = True
          isArray _                = False
-- Record creation
transExp TAbsyn.RecordExp{TAbsyn.recordFields=efields, TAbsyn.recordTyp=recordtypesymbol, TAbsyn.recordPos=pos}
  = do (_, tenv, _, _) <- get
       case Map.lookup recordtypesymbol tenv of
         Just rty@(TigSTy.Record (symboltypair, _)) -> checkRecordInternal symboltypair rty
         Just nty@(TigSTy.Name (namesymbol, _))     -> do aty <- actualTy pos nty
                                                          if isRecord aty
                                                             then do let TigSTy.Record (symboltypair, _) = aty
                                                                     checkRecordInternal symboltypair aty
                                                             else throwError $ SE(pos, illegalRecType $ TSym.name namesymbol)
         _ -> throwError $ SE(pos, undefinedRecError $ TSym.name recordtypesymbol)
    where fst'  (a, _, _) = a
          snd'  (_, b, _) = b
          thrd' (_, _, c) = c
          checkRecordInternal symboltypair rty = do let symbols = map fst' efields
                                                    let exprs   = map snd' efields
                                                    let recsymbols = map fst symboltypair
                                                    let rectys     = map snd symboltypair
                                                    tys <- mapM transExp exprs
                                                    actualRecTys <- mapM (actualTy pos) rectys
                                                    if symbols==recsymbols
                                                       then if tys==actualRecTys
                                                               then return rty
                                                               else throwError $ SE(pos, recTypeMisMatch)
                                                       else throwError $ SE(pos, recSymbolMisMatch)
                                        

transDec :: TAbsyn.Dec -> SementState (Venv, Tenv)
-- Function declaration
transDec (TAbsyn.FunctionDec fs) = do let namesymbols = map TAbsyn.fundecName fs
                                      let paramfields = map TAbsyn.fundecParams fs
                                      let resultmaybesymbols = map TAbsyn.fundecResult fs
                                      (venv, tenv, _, _) <- get
                                      resultTys <- mapM (maybeResSymToTy tenv) resultmaybesymbols
                                      paramTyss <- mapM (mapM $ tfieldToTy tenv) paramfields
                                      let fundecs = zipWith funDecZipper paramTyss resultTys
                                      let funsymboldecpair = zip namesymbols fundecs
                                      let venv' = foldr insert' venv funsymboldecpair
                                      withBinding venv' tenv (mapM_ checkBody fs)
                                      return (venv', tenv)
                                   where maybeResSymToTy _    Nothing  = return TigSTy.Unit
                                         maybeResSymToTy tenv (Just (s, pos)) = do if s `Map.member` tenv
                                                                                      then return $ fromJust $ Map.lookup s tenv
                                                                                      else throwError $ SE(pos, undefinedFuncRetType $ TSym.name s)
                                         funDecZipper paramTys resultTy = FunEntry{funFormals=paramTys, funResult=resultTy}
                                         insert' (k, v) = Map.insert k v
                                         checkBody TAbsyn.Fundec{TAbsyn.fundecName=namesym
                                                                  ,TAbsyn.fundecParams=tfields
                                                                  ,TAbsyn.fundecResult=mayberes
                                                                  ,TAbsyn.fundecBody=bexp
                                                                  ,TAbsyn.fundecPos=pos}
                                           = do (v, t, _, _) <- get
                                                let paramnamesymbols = map TAbsyn.tfieldName tfields
                                                paramTys <- mapM (tfieldToTy t) tfields
                                                let varentries = map (\ty -> VarEntry{varTy=ty, varReadOnly=False}) paramTys
                                                let varNameEntryPairs = zip paramnamesymbols varentries
                                                let v' = foldr insert' v varNameEntryPairs
                                                bodyTy <- withBinding v' t $ transExp bexp
                                                case mayberes of
                                                  Nothing -> if (bodyTy == TigSTy.Unit) 
                                                                then return () 
                                                                else throwError $ SE(pos, procedureHasNonUnitRetType $ TSym.name namesym)
                                                  Just (rettysym, rettypos) -> case Map.lookup rettysym t of
                                                                                  Just retty -> do actualretty <- actualTy pos retty
                                                                                                   if actualretty == bodyTy 
                                                                                                      then return ()
                                                                                                      else throwError $ SE(pos, functionRetBodyTypeMismatch $ TSym.name namesym)
                                                                                  Nothing -> throwError $ SE(rettypos, undefinedFuncRetType $ TSym.name namesym)
                                         tfieldToTy tenv (TAbsyn.Tfield {TAbsyn.tfieldName=_
                                                                        ,TAbsyn.tfieldTyp=s
                                                                        ,TAbsyn.tfieldPos=pos}) = do if s `Map.member` tenv
                                                                                                        then return $ fromJust $ Map.lookup s tenv
                                                                                                        else throwError $ SE(pos, undefinedFuncParamType)
-- Variable declaration
transDec TAbsyn.VarDec{TAbsyn.varDecVar=vardec
                      ,TAbsyn.varDecTyp=maybetype
                      ,TAbsyn.varDecInit=initexp
                      ,TAbsyn.varDecPos=pos} = do initty <- transExp initexp
                                                  let variablenamesym = TAbsyn.vardecName vardec
                                                  (v, t, _, _) <- get
                                                  case maybetype of
                                                    Nothing -> do let v' = Map.insert variablenamesym VarEntry{varTy=initty, varReadOnly=False} v
                                                                  return (v', t)
                                                    Just (varty, varpos) -> case Map.lookup varty t of
                                                                              Nothing -> throwError $ SE(varpos, undefinedVarDecType $ TSym.name varty)
                                                                              Just ty -> do actualty <- actualTy pos ty
                                                                                            if (actualty == initty)
                                                                                               then do let v' = Map.insert variablenamesym VarEntry{varTy=initty, varReadOnly=False} v
                                                                                                       return (v', t)
                                                                                               else throwError $ SE(pos, variableInitDeclaredTypeMismatch $ TSym.name variablenamesym)
-- Type declaration
transDec (TAbsyn.TypeDec decs) = do (v, t, _, _) <- get
                                    let typenames = map TAbsyn.typedecName decs
                                    let typeposes = map TAbsyn.typedecPos decs
                                    mapM_ checkTypeName $ zip typenames typeposes
                                    refNothings <- mapM (\_ -> liftIO $ newIORef Nothing) decs
                                    let tempNameTypes = zipWith (\namesym -> \refnothing -> TigSTy.Name(namesym, refnothing)) typenames refNothings
                                    let t' = foldr (insert') t (zip typenames tempNameTypes)
                                    let absyntys = map TAbsyn.typedecTy decs
                                    tys <- mapM (withBinding v t' . transTy) absyntys
                                    mapM_ rewriteRefNothing $ zip refNothings tys
                                    return (v, t')
                                 where insert' (k, v)= Map.insert k v
                                       rewriteRefNothing (refNothing, ty) = liftIO $ writeIORef refNothing $ Just ty
                                       checkTypeName (typename, pos) = do (_, t, _, _) <- get
                                                                          if typename `Map.member` t
                                                                             then throwError $ SE(pos, redefiningType $ TSym.name typename)
                                                                             else return ()

transTy :: TAbsyn.Ty -> SementState TigSTy.Ty
transTy (TAbsyn.NameTy(namesymbol, pos)) = do (_, t, _, _) <- get
                                              case Map.lookup namesymbol t of
                                                Nothing -> throwError $ SE(pos, undefinedTypeInNameTy $ TSym.name namesymbol)
                                                Just ty -> return ty
transTy (TAbsyn.ArrayTy(namesymbol, pos)) = do (_, t, _, _) <- get
                                               case Map.lookup namesymbol t of
                                                 Nothing -> throwError $ SE(pos, undefinedTypeInArrayTy $ TSym.name namesymbol)
                                                 Just ty -> do u <- genUnique
                                                               return $ TigSTy.Array(ty, u)
transTy (TAbsyn.RecordTy tfields) = do (_, t, _, _) <- get
                                       let names = map TAbsyn.tfieldName tfields
                                       let types = map TAbsyn.tfieldTyp  tfields
                                       let positions = map TAbsyn.tfieldPos tfields
                                       if all (checkExistence t) types
                                          then do let sementtys = map fromJust $ map (flip (Map.lookup) t) types
                                                  u <- genUnique
                                                  return $ TigSTy.Record(zip names sementtys, u)
                                          else do let (Just nonexistentty) = find (not . checkExistence t) types
                                                  let (Just idx) = nonexistentty `elemIndex` types
                                                  throwError $ SE(positions!!idx, undefinedTypeInRecordTy $ TSym.name (types!!idx))
                                    where checkExistence tenv name = if name `Map.member` tenv then True else False



transProg' :: SementStates -> TAbsyn.Program -> IO(Either SementError ())
transProg' initialS (TAbsyn.Pexp e) = do errorOrTy <- runErrorT $ evalStateT (transExp e) initialS
                                         case errorOrTy of
                                           Left err -> return $ Left err
                                           _        -> return $ Right ()
transProg' initialS (TAbsyn.Pdecs (d:decs)) = do errorOrResSt <- runErrorT $ runStateT (transDec d) initialS
                                                 case errorOrResSt of
                                                   Left err -> return $ Left err
                                                   Right (_, s) -> transProg' s (TAbsyn.Pdecs decs)
transProg' _ (TAbsyn.Pdecs []) = return $ Right ()

transProg :: TSym.SymbolMap -> TAbsyn.Program -> IO (Either SementError ())
transProg symmap prog = let 
                          emptyState = (initialVenv, initialTenv, 0 :: TigSTy.Uniq, 0)
                          intsym = TSym.symbol "int" symmap
                          stringsym = TSym.symbol "string" symmap
                          insert' (sym, ty) = Map.insert sym ty
                          refineSyms ((maybesym, ty):rest) = case maybesym of
                                                               Nothing  -> refineSyms rest
                                                               Just sym -> (sym, ty):(refineSyms rest)
                          refineSyms [] = []
                          initialVenv = Map.empty
                          initialTenv = foldr insert' Map.empty (refineSyms [(intsym, TigSTy.INT), (stringsym, TigSTy.String)])
                        in
                          transProg' emptyState prog
