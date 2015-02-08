module TigerSement
       (
         transProg
       , SementError
       ) where

import qualified TigerSementTypes as TSTy
import qualified TigerLexer       as TLex
import qualified TigerAbsyn       as TAbsyn
import qualified TigerSymbol      as TSym
import qualified TigerTemp        as TTmp
import qualified TigerTranslate   as TTran
import qualified Data.Map         as Map
import Data.Maybe
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.IORef
import Data.List

data SementError = SE (TLex.AlexPosn, String) deriving (Show, Eq)
type ExpTy = (TTran.Gexp, TSTy.Ty)

data EnvEntry = VarEntry {varAccess :: TTran.Access, varTy::TSTy.Ty, varReadOnly::Bool}
              | FunEntry {funLevel :: TTran.Level, funLabel :: TTmp.Label, funFormals::[(TSTy.Ty, TTran.Access)], funResult::TSTy.Ty}

type Venv    = Map.Map TSym.Symbol EnvEntry
type Tenv    = Map.Map TSym.Symbol TSTy.Ty
type LoopLevel = Int
type SementStates = (Venv, Tenv, TSTy.Uniq, LoopLevel)

type SementState = StateT SementStates (ExceptT SementError IO)

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
nonOrdCmpOnStr                          = "Non order comparison operator on string"

-- Generates a new value of Uniq type
genUnique :: SementState TSTy.Uniq
genUnique = do (venv, tenv, a, inloop) <- get
               put (venv, tenv, a+1, inloop)
               return a

actualTy' :: TLex.AlexPosn -> TSTy.Ty -> [TSTy.Ty] -> SementState TSTy.Ty
actualTy' pos a@(TSTy.Name (_, iorefmaybety)) searched = do if a `elem` searched
                                                                 then throwError $ SE(pos, circularType ++ ": "  ++ (show a))
                                                                 else do maybety <- liftIO $ readIORef iorefmaybety
                                                                         case maybety of
                                                                           (Just ty) -> actualTy' pos ty (a:searched)
                                                                           Nothing -> error "Impossible name type"
actualTy' pos otherType searched = if (otherType `elem` searched) 
                                      then throwError $ SE(pos, circularType ++ ": " ++ (show otherType))
                                      else return otherType

actualTy :: TLex.AlexPosn -> TSTy.Ty -> SementState TSTy.Ty
actualTy pos a = actualTy' pos a []

isRecord (TSTy.Record _) = True
isRecord _ = False

isArray (TSTy.Array _) = True
isArray _ = False

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

transVar :: TTran.Level -> Maybe TTmp.Label -> TAbsyn.Var -> SementState ExpTy
transVar level _ (TAbsyn.SimpleVar(s, pos)) = do (v, _, _, _) <- get
                                                 case Map.lookup s v of
                                                   Nothing                                -> throwError $ SE(pos, variableUndefined $ TSym.name s)
                                                   Just VarEntry{varAccess=acc, varTy=ty} -> do g <- liftIO $ TTran.simpleVar acc level
                                                                                                return (g, ty)
                                                   _                                      -> throwError $ SE(pos, nameIsNotVariable $ TSym.name s)

transVar level breakLab (TAbsyn.FieldVar(var, s, pos)) = do (ge, varty) <- transVar level breakLab var
                                                            aty <- actualTy pos varty
                                                            if (isRecord aty)
                                                               then do let TSTy.Record(symboltypairs, _) = aty
                                                                       case findIndex (\(sym, _) -> sym == s) symboltypairs of
                                                                         Just idx -> do g <- liftIO $ TTran.field ge idx
                                                                                        return (g, snd $ symboltypairs !! idx)
                                                                         Nothing  -> throwError $ SE(pos, recordTypeHasNoField $ TSym.name s)
                                                               else throwError $ SE(pos, fieldVarOnNonRecordType $ TSym.name s)

transVar level breakLab (TAbsyn.SubscriptVar(var, idxexp, pos)) = do (ge, varty) <- transVar level breakLab var
                                                                     aty <- actualTy pos varty
                                                                     if (isArray aty)
                                                                        then do let TSTy.Array(ty, _) = aty
                                                                                (idxge, idxty) <- transExp level breakLab idxexp
                                                                                if (idxty == TSTy.INT)
                                                                                    then do g <- liftIO $ TTran.subscript ge idxge
                                                                                            return (g, ty)
                                                                                    else throwError $ SE(pos, subscriptIsNotIntType)
                                                                        else throwError $ SE(pos, subscriptVarOnNonArrayType)


transExp :: TTran.Level -> Maybe TTmp.Label -> TAbsyn.Exp -> SementState ExpTy
-- Binary Operation
transExp level breakLab TAbsyn.OpExp{TAbsyn.opLeft=el, TAbsyn.opOper=op, TAbsyn.opRight=er, TAbsyn.opPos=pos}
  = do (gel, tl) <- transExp level breakLab el
       (ger, tr) <- transExp level breakLab er
       atl <- actualTy pos tl
       atr <- actualTy pos tr
       if (atl == atr)
          then case atl of
                 TSTy.INT    -> do {g <- liftIO $ selectFunNonStr op gel ger; return (g, TSTy.INT)}
                 TSTy.String -> do case op of
                                     TAbsyn.EqOp  -> do { g <- liftIO $ selectFunStr op gel ger; return (g, TSTy.INT)}
                                     TAbsyn.NeqOp -> do { g <- liftIO $ selectFunStr op gel ger; return (g, TSTy.INT)}
                                     _            -> throwError $ SE(pos, nonOrdCmpOnStr)
                 TSTy.Nil    -> throwError $ SE(pos, binNilOp)
                 TSTy.Unit   -> throwError $ SE(pos, binUnitOp)
                 _           -> case op of
                                  TAbsyn.EqOp  -> do { g <- liftIO $ selectFunNonStr op gel ger; return (g, TSTy.INT)}
                                  TAbsyn.NeqOp -> do { g <- liftIO $ selectFunNonStr op gel ger; return (g, TSTy.INT)}
                                  _            -> throwError $ SE(pos, arrayRecOrderOp)
          else throwError $ SE(pos, binOpNonMatchingError)
  where selectFunNonStr TAbsyn.EqOp  = TTran.eqCmp
        selectFunNonStr TAbsyn.NeqOp = TTran.notEqCmp
        selectFunNonStr TAbsyn.LtOp  = TTran.lessThan
        selectFunNonStr TAbsyn.LeOp  = TTran.lessThanOrEq
        selectFunNonStr TAbsyn.GtOp  = flip TTran.lessThanOrEq
        selectFunNonStr TAbsyn.GeOp  = flip TTran.lessThan
        selectFunNonStr o            = TTran.arithmetic o
        selectFunStr    TAbsyn.EqOp  = TTran.eqStr
        selectFunStr    TAbsyn.NeqOp = TTran.notEqStr
        selectFunStr    _            = error "Compiler error: non order operator on string"


-- Variable
transExp level breakLab (TAbsyn.VarExp var) = transVar level breakLab var
-- Nil expression
transExp _     _ TAbsyn.NilExp = return (TTran.nilGexp, TSTy.Nil)
-- Int expression
transExp _     _ (TAbsyn.IntExp val) = do g <- liftIO $ TTran.intExp val 
                                          return (g, TSTy.INT)
-- String
transExp _     _ (TAbsyn.StringExp (str, _)) = do g <- liftIO $ TTran.stringExp str
                                                  return (g, TSTy.String)
-- Sequence expression
transExp level breakLab (TAbsyn.SeqExp ((e, _):[]))
  = transExp level breakLab e
transExp level breakLab (TAbsyn.SeqExp ((e, _):es))
  = do (ge, _) <- transExp level breakLab e
       (greste, ty) <- transExp level breakLab (TAbsyn.SeqExp es)
       g <- liftIO $ TTran.constructEseq ge greste
       return (g, ty)
transExp _ _ (TAbsyn.SeqExp []) = error "Impossible SeqExp case"
-- Function application
transExp level breakLab TAbsyn.AppExp{TAbsyn.appFunc=funcsymbol, TAbsyn.appArgs=es, TAbsyn.appPos=pos}
  = do (venv, _, _, _) <- get
       case Map.lookup funcsymbol venv of
          Just (FunEntry{ funLevel=funlvl
                        , funLabel=funlab
                        , funFormals=formaltys
                        , funResult=resty }) -> do (ges, tys) <- liftM unzip $ mapM (transExp level breakLab) es
                                                   actualTys  <- mapM (actualTy pos) tys
                                                   actualFormalTys <- mapM (actualTy pos . fst) formaltys
                                                   if actualTys==actualFormalTys
                                                      then do g <- liftIO $ TTran.callFunction funlab level funlvl ges
                                                              return (g, resty)
                                                      else throwError $ SE(pos, parameterTypesDoesNotMatchFunctionTypes)
          Just _ -> throwError $ SE(pos, variableIsNotFunction $ TSym.name funcsymbol)
          Nothing -> throwError $ SE(pos, undefinedFuncError $ TSym.name funcsymbol) 
-- Assignment
transExp level breakLab TAbsyn.AssignExp{TAbsyn.assignVar=var, TAbsyn.assignExp=exp, TAbsyn.assignPos=pos}
  = do (gevar, tvar) <- transVar level breakLab var
       (geexp, texp) <- transExp level breakLab exp
       if tvar == texp
          then do g <- liftIO $ TTran.assign gevar geexp
                  return (g, TSTy.Unit)
          else throwError $ SE(pos, assignmentTypeMismatch)
-- If expression
transExp level breakLab TAbsyn.IfExp{TAbsyn.ifTest=testexp, TAbsyn.ifThen=thenexp, TAbsyn.ifElse=maybeElse, TAbsyn.ifPos=pos}
  = do (testge, testty) <- transExp level breakLab testexp
       (thenge, thenty) <- transExp level breakLab thenexp
       if testty == TSTy.INT
          then if isJust maybeElse
               then do let elseexp = fromJust maybeElse
                       (elsege, elsety) <- transExp level breakLab elseexp
                       actualThenTy <- actualTy pos thenty
                       actualElseTy <- actualTy pos elsety
                       if actualThenTy==actualElseTy
                          then do g <- liftIO $ TTran.ifThenElse testge thenge elsege
                                  return (g, elsety)
                          else throwError $ SE(pos, ifthenTypeMismatch)
               else do g <- liftIO $ TTran.ifThen testge thenge
                       return (g, thenty)
          else throwError $ SE(pos, illegalIfTestType)
-- While expression
transExp level breakLab TAbsyn.WhileExp{TAbsyn.whileTest=testexp, TAbsyn.whileBody=bodyexp, TAbsyn.whilePos=pos}
  = do (testge, testty) <- transExp level breakLab testexp
       donelab <- liftIO TTmp.newLabel
       if testty == TSTy.INT
          then do (bodyge, bodyty) <- checkWhileInternal donelab
                  if bodyty == TSTy.Unit
                     then do g <- liftIO $ TTran.whileLoop testge bodyge donelab
                             return (g, TSTy.Unit)
                     else throwError $ SE(pos, whileLoopBodyUnit)
          else throwError $ SE(pos, illegalWhileTestType)
  where checkWhileInternal donelab = do enterLoop
                                        bodyres <- transExp level (Just donelab) bodyexp
                                        exitLoop
                                        return bodyres
-- For expression
transExp level breakLab TAbsyn.ForExp{TAbsyn.forVar=vardec, TAbsyn.forLo=lowexp, TAbsyn.forHi=highexp, TAbsyn.forBody=bodyexp, TAbsyn.forPos=pos}
  = do (venv, tenv, _, _) <- get
       counteraccess <- liftIO $ TTran.allocInFrame level
       countergexp <- liftIO $ TTran.simpleVar counteraccess level
       let venv' = Map.insert (TAbsyn.vardecName vardec) VarEntry{varAccess=counteraccess, varTy=TSTy.INT, varReadOnly=False} venv
       donelab <- liftIO TTmp.newLabel
       (lowge, lowty) <- transExp level breakLab lowexp
       (highge, highty) <- transExp level breakLab highexp
       if (lowty == highty) && (lowty == TSTy.INT)
          then withBinding venv' tenv $ checkForInternal donelab lowge highge countergexp
          else throwError $ SE(pos, forLoopBoundaryType)
    where checkForInternal donelab logexp higexp counterge = do enterLoop
                                                                (bodyge, bodyty) <- transExp level (Just donelab) bodyexp
                                                                exitLoop
                                                                if (bodyty == TSTy.Unit)
                                                                   then do g <- liftIO $ TTran.forLoop logexp higexp bodyge donelab counterge
                                                                           return (g, TSTy.Unit)
                                                                   else throwError $ SE(pos, forLoopBodyUnit)
-- Break expression
transExp _ (Just breakLab) TAbsyn.BreakExp{TAbsyn.breakPos=pos}
  = do (_, _, _, inloop) <- get
       if (inloop > 0)
          then do g <- liftIO $ TTran.break breakLab
                  return (g, TSTy.Unit)
          else throwError $ SE(pos, breakExpNotInLoop)
transExp _ _ TAbsyn.BreakExp{TAbsyn.breakPos=pos} = throwError $ SE(pos, breakExpNotInLoop)
-- Let expression
transExp level breakLab TAbsyn.LetExp{TAbsyn.letDecs=decs, TAbsyn.letBody=bodyexp, TAbsyn.letPos=_}
  = do (venv', tenv', ges) <- transDecs decs
       (bodyge, bodyty) <- withBinding venv' tenv' checkLetExpInternal
       g <- liftIO $ TTran.letExpression ges bodyge
       return (g, bodyty)
  where transDecs (d:[])   = do (v, t, ges) <- transDec level breakLab d
                                return (v, t, ges)
        transDecs (d:ds)   = do (v, t, ges) <- transDec level breakLab d
                                (v', t', gess) <- withBinding v t (transDecs ds)
                                return (v', t', ges++gess)
        transDecs []       = do (v, t, _, _) <- get
                                return (v, t, [])
        checkLetExpInternal = transExp level breakLab bodyexp
-- Array expression
transExp level breakLab TAbsyn.ArrayExp{TAbsyn.arrayTyp=arraytypesymbol, TAbsyn.arraySize=szexp, TAbsyn.arrayInit=initexp, TAbsyn.arrayPos=pos}
  = do (_, tenv, _, _) <- get
       case Map.lookup arraytypesymbol tenv of
          Just ty -> checkArrayExpInternal ty
          Nothing -> throwError $ SE(pos, undefinedArrError $ TSym.name arraytypesymbol)
    where checkArrayExpInternal ty = do aty <- actualTy pos ty
                                        (szge, szty) <- transExp level breakLab szexp
                                        (initge, initty) <- transExp level breakLab initexp
                                        if (isArray aty)
                                           then do let TSTy.Array (internalTy, _) = aty
                                                   ainternalTy <- actualTy pos internalTy
                                                   if (ainternalTy == initty)
                                                      then if (szty == TSTy.INT)
                                                           then do g <- liftIO $ TTran.createArray szge initge
                                                                   return (g, aty)
                                                           else throwError $ SE(pos, arraySizeNotInt)
                                                      else throwError $ SE(pos, arrayInitMismatch)
                                           else throwError $ SE(pos, nonarrayCreation $ TSym.name arraytypesymbol)
-- Record creation
transExp level breakLab TAbsyn.RecordExp{TAbsyn.recordFields=efields, TAbsyn.recordTyp=recordtypesymbol, TAbsyn.recordPos=pos}
  = do (_, tenv, _, _) <- get
       case Map.lookup recordtypesymbol tenv of
         Just rty@(TSTy.Record (symboltypair, _)) -> checkRecordInternal symboltypair rty
         Just nty@(TSTy.Name (namesymbol, _))     -> do aty <- actualTy pos nty
                                                        if isRecord aty
                                                           then do let TSTy.Record (symboltypair, _) = aty
                                                                   checkRecordInternal symboltypair aty
                                                           else throwError $ SE(pos, illegalRecType $ TSym.name namesymbol)
         _ -> throwError $ SE(pos, undefinedRecError $ TSym.name recordtypesymbol)
    where fst'  (a, _, _) = a
          snd'  (_, b, _) = b
          checkRecordInternal symboltypair rty = do let symbols = map fst' efields
                                                    let exprs   = map snd' efields
                                                    let recsymbols = map fst symboltypair
                                                    let rectys     = map snd symboltypair
                                                    (ges, tys) <- liftM unzip $ mapM (transExp level breakLab) exprs
                                                    actualTys <- mapM (actualTy pos) tys
                                                    actualRecTys <- mapM (actualTy pos) rectys
                                                    if symbols==recsymbols
                                                       then if actualTys==actualRecTys
                                                               then do g <- liftIO $ TTran.createRecord ges
                                                                       return (g, rty)
                                                               else throwError $ SE(pos, recTypeMisMatch)
                                                       else throwError $ SE(pos, recSymbolMisMatch)
                                        

transDec :: TTran.Level -> Maybe TTmp.Label -> TAbsyn.Dec -> SementState (Venv, Tenv, [TTran.Gexp])
-- Function declaration
transDec level breakLab (TAbsyn.FunctionDec fs) = do let namesymbols = map TAbsyn.fundecName fs
                                                     let paramfields = map TAbsyn.fundecParams fs
                                                     let resultmaybesymbols = map TAbsyn.fundecResult fs
                                                     (venv, tenv, _, _) <- get
                                                     resultTys <- mapM (maybeResSymToTy tenv) resultmaybesymbols
                                                     paramTyss <- mapM (mapM $ tfieldToTy tenv) paramfields
                                                     funlabels <- mapM (\_ -> liftIO TTmp.newLabel) fs
                                                     (funlvls,formalsWithOffsetss) <- liftM unzip $ mapM (\paramTys -> liftIO $ TTran.newLevel level paramTys) paramTyss
                                                     let fundecs = zipWith4 funDecZipper funlvls funlabels formalsWithOffsetss resultTys
                                                     let funsymboldecpair = zip namesymbols fundecs
                                                     let venv' = foldr insert' venv funsymboldecpair
                                                     withBinding venv' tenv (mapM_ (uncurry4 checkBody) (zip4 fs formalsWithOffsetss funlvls funlabels))
                                                     return (venv', tenv, [])
                                                  where uncurry4 fun (a, b, c, d) = fun a b c d
                                                        maybeResSymToTy _    Nothing  = return TSTy.Unit
                                                        maybeResSymToTy tenv (Just (s, pos)) = do if s `Map.member` tenv
                                                                                                     then return $ fromJust $ Map.lookup s tenv
                                                                                                     else throwError $ SE(pos, undefinedFuncRetType $ TSym.name s)
                                                        funDecZipper funlvl funlab paramTys resultTy = FunEntry{funLevel=funlvl
                                                                                                               ,funLabel=funlab
                                                                                                               ,funFormals=paramTys
                                                                                                               ,funResult=resultTy}
                                                        insert' (k, v) = Map.insert k v
                                                        varEntryZipper ty (_, access) = VarEntry{varAccess=access, varTy=ty, varReadOnly=False}
                                                        checkBody TAbsyn.Fundec{TAbsyn.fundecName=namesym
                                                                               ,TAbsyn.fundecParams=tfields
                                                                               ,TAbsyn.fundecResult=mayberes
                                                                               ,TAbsyn.fundecBody=bexp
                                                                               ,TAbsyn.fundecPos=pos}
                                                                  formalsWithOffsets
                                                                  funlvl
                                                                  funlab
                                                          = do (v, t, _, _) <- get
                                                               let paramnamesymbols = map TAbsyn.tfieldName tfields
                                                               paramTys <- mapM (tfieldToTy t) tfields
                                                               let varentries = zipWith varEntryZipper paramTys formalsWithOffsets
                                                               let varNameEntryPairs = zip paramnamesymbols varentries
                                                               let v' = foldr insert' v varNameEntryPairs
                                                               (bodyge, bodyTy) <- withBinding v' t $ transExp funlvl breakLab bexp
                                                               case mayberes of
                                                                 Nothing -> if (bodyTy == TSTy.Unit) 
                                                                               then liftIO $ TTran.createProcFrag funlab funlvl bodyge
                                                                               else throwError $ SE(pos, procedureHasNonUnitRetType $ TSym.name namesym)
                                                                 Just (rettysym, rettypos) -> case Map.lookup rettysym t of
                                                                                                 Just retty -> do actualretty <- actualTy pos retty
                                                                                                                  if actualretty == bodyTy 
                                                                                                                     then liftIO $ TTran.createProcFrag funlab funlvl bodyge
                                                                                                                     else throwError $ SE(pos, functionRetBodyTypeMismatch $ TSym.name namesym)
                                                                                                 Nothing -> throwError $ SE(rettypos, undefinedFuncRetType $ TSym.name namesym)
                                                        tfieldToTy tenv (TAbsyn.Tfield {TAbsyn.tfieldName=_
                                                                                       ,TAbsyn.tfieldTyp=s
                                                                                       ,TAbsyn.tfieldPos=pos}) = do if s `Map.member` tenv
                                                                                                                       then return $ fromJust $ Map.lookup s tenv
                                                                                                                       else throwError $ SE(pos, undefinedFuncParamType)
-- Variable declaration
transDec level breakLab TAbsyn.VarDec{TAbsyn.varDecVar=vardec
                                     ,TAbsyn.varDecTyp=maybetype
                                     ,TAbsyn.varDecInit=initexp
                                     ,TAbsyn.varDecPos=pos} = 
  do (initge, initty) <- transExp level breakLab initexp
     let variablenamesym = TAbsyn.vardecName vardec
     (v, t, _, _) <- get
     access <- liftIO $ TTran.allocInFrame level
     let varentry = VarEntry{varAccess=access, varTy=initty, varReadOnly=False}
     simplevar <- liftIO $ TTran.simpleVar access level
     assigngexp <- liftIO $ TTran.assign simplevar initge
     case maybetype of
       Nothing -> do let v' = Map.insert variablenamesym varentry v
                     return (v', t, [assigngexp])
       Just (varty, varpos) -> case Map.lookup varty t of
                                 Nothing -> throwError $ SE(varpos, undefinedVarDecType $ TSym.name varty)
                                 Just ty -> do actualty <- actualTy pos ty
                                               if (actualty == initty)
                                                  then do let v' = Map.insert variablenamesym varentry v
                                                          return (v', t, [assigngexp])
                                                  else throwError $ SE(pos, variableInitDeclaredTypeMismatch $ TSym.name variablenamesym)
-- Type declaration
transDec _ _ (TAbsyn.TypeDec decs) = 
  do (v, t, _, _) <- get
     let typenames = map TAbsyn.typedecName decs
     let typeposes = map TAbsyn.typedecPos decs
     mapM_ checkTypeName $ zip typenames typeposes
     refNothings <- mapM (\_ -> liftIO $ newIORef Nothing) decs
     let tempNameTypes = zipWith (\namesym -> \refnothing -> TSTy.Name(namesym, refnothing)) typenames refNothings
     let t' = foldr (insert') t (zip typenames tempNameTypes)
     let absyntys = map TAbsyn.typedecTy decs
     tys <- mapM (withBinding v t' . transTy) absyntys
     mapM_ rewriteRefNothing $ zip refNothings tys
     return (v, t', [])
   where insert' (k, v)= Map.insert k v
         rewriteRefNothing (refNothing, ty) = liftIO $ writeIORef refNothing $ Just ty
         checkTypeName (typename, pos) = do (_, t, _, _) <- get
                                            if typename `Map.member` t
                                               then throwError $ SE(pos, redefiningType $ TSym.name typename)
                                               else return ()

transTy :: TAbsyn.Ty -> SementState TSTy.Ty
transTy (TAbsyn.NameTy(namesymbol, pos)) = do (_, t, _, _) <- get
                                              case Map.lookup namesymbol t of
                                                Nothing -> throwError $ SE(pos, undefinedTypeInNameTy $ TSym.name namesymbol)
                                                Just ty -> return ty
transTy (TAbsyn.ArrayTy(namesymbol, pos)) = do (_, t, _, _) <- get
                                               case Map.lookup namesymbol t of
                                                 Nothing -> throwError $ SE(pos, undefinedTypeInArrayTy $ TSym.name namesymbol)
                                                 Just ty -> do u <- genUnique
                                                               return $ TSTy.Array(ty, u)
transTy (TAbsyn.RecordTy tfields) = do (_, t, _, _) <- get
                                       let names = map TAbsyn.tfieldName tfields
                                       let types = map TAbsyn.tfieldTyp  tfields
                                       let positions = map TAbsyn.tfieldPos tfields
                                       if all (checkExistence t) types
                                          then do let sementtys = map fromJust $ map (flip (Map.lookup) t) types
                                                  u <- genUnique
                                                  return $ TSTy.Record(zip names sementtys, u)
                                          else do let (Just nonexistentty) = find (not . checkExistence t) types
                                                  let (Just idx) = nonexistentty `elemIndex` types
                                                  throwError $ SE(positions!!idx, undefinedTypeInRecordTy $ TSym.name (types!!idx))
                                    where checkExistence tenv name = if name `Map.member` tenv then True else False



transProg' :: SementStates -> TAbsyn.Program -> IO(Either SementError [TTran.Frag])
transProg' initialS (TAbsyn.Pexp e) = do (tigerMainLevel,_) <- liftIO $ TTran.newLevel TTran.outerMost []
                                         errorOrTy <- runExceptT $ evalStateT (transExp tigerMainLevel Nothing e) initialS
                                         case errorOrTy of
                                           Left err -> return $ Left err
                                           Right (ge, _) -> do liftIO $ TTran.createMainFrag tigerMainLevel ge
                                                               frags <- liftIO $ TTran.getResult
                                                               return $ Right frags
transProg' initialS (TAbsyn.Pdecs (d:decs)) = do (tigerMainLevel,_) <- liftIO $ TTran.newLevel TTran.outerMost []
                                                 errorOrResSt <- runExceptT $ runStateT (transDec tigerMainLevel Nothing d) initialS
                                                 case errorOrResSt of
                                                   Left err -> return $ Left err
                                                   Right (_, s) -> transProg' s (TAbsyn.Pdecs decs)
transProg' _ (TAbsyn.Pdecs []) = do frags <- liftIO $ TTran.getResult
                                    return $ Right frags

transProg :: TAbsyn.Program -> IO (Either SementError [TTran.Frag])
transProg prog = do intsym       <- TSym.symbol "int"
                    stringsym    <- TSym.symbol "string"
                    printsym     <- TSym.symbol "print"
                    flushsym     <- TSym.symbol "flush"
                    getcharsym   <- TSym.symbol "getchar"
                    ordsym       <- TSym.symbol "ord"
                    chrsym       <- TSym.symbol "chr"
                    sizesym      <- TSym.symbol "size"
                    substringsym <- TSym.symbol "substring"
                    concatsym    <- TSym.symbol "concat"
                    notsym       <- TSym.symbol "not"
                    exitsym      <- TSym.symbol "exit"

                    printlab     <- TTmp.namedLabel "print"
                    flushlab     <- TTmp.namedLabel "flush"
                    getcharlab   <- TTmp.namedLabel "getchar"
                    ordlab       <- TTmp.namedLabel "ord"
                    chrlab       <- TTmp.namedLabel "chr"
                    sizelab      <- TTmp.namedLabel "size"
                    substringlab <- TTmp.namedLabel "substring"
                    concatlab    <- TTmp.namedLabel "concat"
                    notlab       <- TTmp.namedLabel "not"
                    exitlab      <- TTmp.namedLabel "exit"
                    let insert' (sym, ty) = Map.insert sym ty
                    let initialVenv = foldr insert' Map.empty [(printsym,FunEntry { funLabel=printlab
                                                                                  , funLevel=TTran.outerMost
                                                                                  , funFormals=map addDummyAccess [TSTy.String], funResult=TSTy.Unit } )
                                                              ,(flushsym,FunEntry { funLabel=flushlab
                                                                                  , funLevel=TTran.outerMost
                                                                                  , funFormals =[], funResult=TSTy.Unit   } )
                                                              ,(getcharsym,FunEntry { funLabel=getcharlab
                                                                                    , funLevel=TTran.outerMost
                                                                                    , funFormals =[], funResult=TSTy.String } )
                                                              ,(ordsym,FunEntry { funLabel=ordlab
                                                                                , funLevel=TTran.outerMost
                                                                                , funFormals =map addDummyAccess [TSTy.String], funResult=TSTy.INT    } )
                                                              ,(chrsym,FunEntry { funLabel=chrlab
                                                                                , funLevel=TTran.outerMost
                                                                                , funFormals =map addDummyAccess [TSTy.INT], funResult=TSTy.String } )
                                                              ,(sizesym,FunEntry { funLabel=sizelab
                                                                                 , funLevel=TTran.outerMost
                                                                                 , funFormals =map addDummyAccess [TSTy.String], funResult=TSTy.INT} )
                                                              ,(substringsym,FunEntry { funLabel=substringlab
                                                                                      , funLevel=TTran.outerMost
                                                                                      , funFormals =map addDummyAccess [TSTy.String, TSTy.INT, TSTy.INT], funResult=TSTy.String } )
                                                              ,(concatsym,FunEntry { funLabel=concatlab
                                                                                   , funLevel=TTran.outerMost
                                                                                   , funFormals =map addDummyAccess [TSTy.String, TSTy.String], funResult=TSTy.String } )
                                                              ,(notsym,FunEntry { funLabel=notlab
                                                                                , funLevel=TTran.outerMost
                                                                                , funFormals =map addDummyAccess [TSTy.INT], funResult=TSTy.INT    } )
                                                              ,(exitsym,FunEntry { funLabel=exitlab
                                                                                 , funLevel=TTran.outerMost
                                                                                 , funFormals =map addDummyAccess [TSTy.INT], funResult=TSTy.Unit   } ) ]
                    let initialTenv = foldr insert' Map.empty [(intsym, TSTy.INT), 
                                                               (stringsym, TSTy.String)]
                    let emptyState = (initialVenv, initialTenv, 0 :: TSTy.Uniq, 0)
                    transProg' emptyState prog
  where addDummyAccess ty = (ty, (TTran.outerMost, 0))
