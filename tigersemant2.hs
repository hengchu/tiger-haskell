module TigerSemant2
  (
    transprog
  )
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
import Data.Maybe

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

breakOutOfLoop :: TLex.AlexPosn -> Frt.SemantError
breakOutOfLoop pos = Frt.SE pos $ Frt.BreakOutsideOfLoop

duplicateDefinition :: TLex.AlexPosn -> String -> Frt.SemantError
duplicateDefinition pos str = Frt.SE pos $ Frt.DuplicateDefinition str

notVariable :: TLex.AlexPosn -> String -> Frt.SemantError
notVariable pos str = Frt.SE pos $ Frt.NotVariable str

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
actualTy' pos a searched = do if a `elem` searched 
                                 then throwError $ cyclicTypeError pos $ map show searched
                                 else return a

actualTy :: TLex.AlexPosn -> TSty.Ty -> Frontend TSty.Ty
actualTy pos ty = actualTy' pos ty []

withBinding :: Venv -> Tenv -> Frontend a -> Frontend a
withBinding v t checker = do (origv, origt, l, fs) <- Frt.semantStGet
                             Frt.semantStPut (v, t, l, fs)
                             a <- checker
                             (_, _, l', fs') <- Frt.semantStGet
                             Frt.semantStPut (origv, origt, l', fs')
                             return a

findFirstDiffInLists :: Eq a => [a] -> [a] -> Maybe Int
findFirstDiffInLists la lb | la == lb  = Nothing
                           | length la /= length lb = error "Compiler error: list a and list b must be the"
                                                            " same length in findFirstDiffInLists."
                           | otherwise = let equalities = zipWith (==) la lb
                                         in  (False) `elemIndex` equalities

sym2ty :: TSym.Symbol -> TLex.AlexPosn -> Frontend TSty.Ty
sym2ty sym pos = do (_, t, _, _) <- Frt.semantStGet
                    case Map.lookup sym t of
                      Nothing -> throwError $ undefinedError pos $ TSym.name sym
                      Just ty -> return ty

addtypetobinding :: TSym.Symbol -> TSty.Ty -> Frontend ()
addtypetobinding sym ty = do (v, t, l, f) <- Frt.semantStGet
                             let t' = Map.insert sym ty t
                             Frt.semantStPut (v, t', l, f)

addfunctiontobinding :: TSym.Symbol -> TTra.Level -> TTmp.Label -> [(TSty.Ty, Frt.Access)] -> TSty.Ty -> Frontend ()
addfunctiontobinding sym lvl lab params result = do (v, t, l, f) <- Frt.semantStGet
                                                    let fentry = Frt.FunEntry lvl lab params result
                                                    let v' = Map.insert sym fentry v
                                                    Frt.semantStPut (v', t, l, f)

isarraytyp :: TSty.Ty -> Bool
isarraytyp (Frt.Array _) = True
isarraytyp _             = False

isrecordtyp :: TSty.Ty -> Bool
isrecordtyp (Frt.Record _) = True
isrecordtyp _              = False

zipWithM4 :: Monad m => (a -> b -> c -> d -> m e) -> [a] -> [b] -> [c] -> [d] -> m [e]
zipWithM4 f (a:args1) (b:args2) (c:args3) (d:args4) = do e <- f a b c d
                                                         es <- zipWithM4 f args1 args2 args3 args4
                                                         return $ e:es
zipWithM4 f [] [] [] [] = return []
zipWithM4 _ _ _ _ _ = error "zipWithM4: Args 1..4 must have the same length."

zipWithM5 :: (Show a, Show b, Show c, Show d, Show e, Monad m) => (a -> b -> c -> d -> e -> m g) -> [a] -> [b] -> [c] -> [d] -> [e] -> m [g]
zipWithM5 f (a:args1) (b:args2) (c:args3) (d:args4) (e:args5)= do g <- f a b c d e
                                                                  gs <- zipWithM5 f args1 args2 args3 args4 args5
                                                                  return $ g:gs
zipWithM5 f [] [] [] [] [] = return []
zipWithM5 _ a b c d e = error $ "zipWithM5: Args 1..5 must have the same length." {- ++ show a ++ "\n" 
                                                                                    ++ show b ++ "\n"
                                                                                    ++ show c ++ "\n" 
                                                                                    ++ show d ++ "\n"
                                                                                    ++ show e -}

zipTypePos :: [[TSym.Symbol]] -> [[TLex.AlexPosn]] -> [[(TSym.Symbol, TLex.AlexPosn)]]
zipTypePos typsyms poses = zipWith zipTyppos' typsyms poses
  where zipTyppos' syms poses = zip syms poses


transdec :: TTra.Level -> Maybe TTmp.Label -> TAbs.Dec -> Frontend (Venv, Tenv, [TTra.Gexp])
transdec lvl lab dec =
  let g (TAbs.FunctionDec fdecs) = let fnamesyms    = map TAbs.fundecName fdecs
                                       ffieldnamess = map (map TAbs.tfieldName) $ map TAbs.fundecParams fdecs
                                       ffieldtypess = map (map TAbs.tfieldTyp)  $ map TAbs.fundecParams fdecs
                                       ffieldposess = map (map TAbs.tfieldPos) $ map TAbs.fundecParams fdecs
                                       fbodyexps = map TAbs.fundecBody fdecs
                                       ftypes = map TAbs.fundecResult fdecs
                                   in  do fparamtyss <- mapM (mapM (uncurry sym2ty)) (zipTypePos ffieldtypess ffieldposess)
                                          (flevels, formalsWithOffsetss) <- liftM unzip $ mapM (TTra.newLevel lvl) fparamtyss
                                          fresulttys <- mapM ftype2ty ftypes
                                          flabs <- mapM (\_ -> TTmp.newLabel) fdecs
                                          let fentries = zipWith4 Frt.FunEntry flevels flabs formalsWithOffsetss fresulttys
                                          (v, t, _, _) <- Frt.semantStGet
                                          let v' = foldr (uncurry Map.insert) v (zip fnamesyms fentries)
                                          _ <- withBinding v' t $ zipWithM5 (checkbody lab) flevels flabs fresulttys 
                                                                            (zip3 ffieldnamess fparamtyss $ liftM (map snd) formalsWithOffsetss)
                                                                            fbodyexps
                                          return (v', t, [])
        where ftype2ty (Just (s, pos)) = sym2ty s pos
              ftype2ty Nothing         = return TSty.Unit
              checkbody newlab newlvl funlab decty (paramsyms, paramtys, paramaccesses) bodyexp = 
                do let varentries = zipWith3 Frt.VarEntry paramaccesses paramtys $ take (length paramsyms) $ repeat False
                   (v, t, _, _) <- Frt.semantStGet
                   let v' = foldr (uncurry Map.insert) v (zip paramsyms varentries)
                   (gexp, bodyty) <- withBinding v' t $ transexp newlvl newlab bodyexp
                   bodyty' <- actualTy (TPar.extractPosition bodyexp) bodyty
                   decty' <- actualTy (TPar.extractPosition bodyexp) decty
                   if bodyty' == decty'
                      then TTra.createProcFrag funlab newlvl gexp
                      else throwError $ typeMisMatchError (TPar.extractPosition bodyexp) (show bodyty) (show decty)
      g (TAbs.VarDec {TAbs.varDecVar=vardec, TAbs.varDecTyp=typandpos, TAbs.varDecInit=initexp, TAbs.varDecPos=pos}) =
        do (initgexp, initty) <- transexp lvl lab initexp
           let varnamesym = TAbs.vardecName vardec
           varaccess <- liftIO $ TTra.allocInFrame lvl
           var <- TTra.simpleVar varaccess lvl
           assigngexp <- TTra.assign var initgexp
           case typandpos of
             Just (typsym, typpos) -> do typty <- sym2ty typsym typpos
                                         initty' <- actualTy (TPar.extractPosition initexp) initty
                                         typty' <- actualTy typpos typty
                                         if initty' == typty'
                                            then do let varentry = Frt.VarEntry varaccess initty' False
                                                    (v, t, _, _) <- Frt.semantStGet
                                                    let v' = Map.insert varnamesym varentry v
                                                    return (v', t, [assigngexp])
                                            else throwError $ typeMisMatchError pos (show initty) (show typty)
             Nothing -> do (v, t, _, _) <- Frt.semantStGet
                           let varentry = Frt.VarEntry varaccess initty False
                           let v' = Map.insert varnamesym varentry v
                           return (v', t, [assigngexp])
      g (TAbs.TypeDec decs) =
        do (v, t, _, _) <- Frt.semantStGet
           let names = map TAbs.typedecName decs
           let poses = map TAbs.typedecPos decs
           mapM_ (uncurry (checkname t)) (zip poses names)
           refNothings <- mapM (\_ -> liftIO $ newIORef Nothing) decs
           let nametypes = zipWith (\sym ref -> TSty.Name (sym, ref)) names refNothings
           let t' = foldr (uncurry Map.insert) t (zip names nametypes)
           let absyntys = map TAbs.typedecTy decs
           tys <- mapM (withBinding v t' . transty) absyntys
           mapM_ (\(ref, ty) -> liftIO $ writeIORef ref $ Just ty) (zip refNothings tys)
           return (v, t', [])
        where checkname t pos sym = do case Map.lookup sym t of
                                         Nothing -> return ()
                                         Just _  -> throwError $ duplicateDefinition pos $ TSym.name sym
  in g dec

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
           lty' <- actualTy (TPar.extractPosition lexp) lty
           rty' <- actualTy (TPar.extractPosition rexp) rty
           if (lty' == rty')
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
        let checkrecord ty = do let typefields = fst ty 
                                let (fieldnames, fieldtys) = unzip typefields
                                let (efieldnames, eexps, eposes) = unzip3 efields
                                if length efieldnames == length fieldnames
                                   then case findFirstDiffInLists efieldnames fieldnames of
                                          Nothing  -> do (gexps, exptys) <- liftM unzip $ mapM g eexps
                                                         exptys' <- mapM (uncurry actualTy) (zip eposes exptys)
                                                         fieldtys' <- mapM (actualTy pos) fieldtys
                                                         case findFirstDiffInLists exptys' fieldtys' of
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
        in do (_, t, _, _) <- Frt.semantStGet
              case Map.lookup typsym t of
                Nothing -> throwError $ undefinedError pos $ TSym.name typsym
                Just (TSty.Record ty) -> checkrecord ty
                Just typ@(TSty.Name _) -> do aty <- actualTy pos typ
                                             case aty of
                                               TSty.Record ty -> checkrecord ty
                                               _ -> throwError $ typeMisMatchError pos ("Record") (TSym.name typsym)
                Just _ -> throwError $ typeMisMatchError pos ("Record") (TSym.name typsym)
      g (TAbs.AssignExp {TAbs.assignVar=var, TAbs.assignExp=aexp, TAbs.assignPos=pos}) =
        do (vgexp, vty) <- transvar lvl lab var
           (agexp, aty) <- g aexp
           vty' <- actualTy pos vty
           aty' <- actualTy (TPar.extractPosition aexp) aty
           if vty' == aty'
              then do gexp <- TTra.assign vgexp agexp
                      return (gexp, TSty.Unit)
              else throwError $ typeMisMatchError pos (show vty) (show aty)
      g (TAbs.IfExp {TAbs.ifTest=testexp, TAbs.ifThen=thenexp, TAbs.ifElse=elseexp, TAbs.ifPos=pos}) =
        do (testgexp, testty) <- g testexp
           (thengexp, thenty) <- g thenexp
           thenty' <- actualTy (TPar.extractPosition thenexp) thenty
           if testty == TSty.INT
              then case elseexp of
                     Just elseexp' -> do (elsegexp, elsety) <- g elseexp'
                                         elsety' <- actualTy (TPar.extractPosition elseexp') elsety
                                         if (thenty' == elsety')
                                            then do gexp <- TTra.ifThenElse testgexp thengexp elsegexp
                                                    return (gexp, elsety)
                                            else throwError $ typeMisMatchError pos (show thenty) (show elsety)
                     Nothing       -> if (thenty' == TSty.Unit)
                                         then do gexp <- TTra.ifThen testgexp thengexp
                                                 return (gexp, TSty.Unit)
                                         else throwError $ typeMisMatchError pos (show TSty.Unit) (show thenty)
              else throwError $ typeMisMatchError pos (show TSty.INT) (show testty)
      g (TAbs.WhileExp {TAbs.whileTest=testexp, TAbs.whileBody=bodyexp, TAbs.whilePos=pos}) =
        do (testgexp, testty) <- g testexp
           if (testty == TSty.INT)
              then do donelab <- TTmp.newLabel
                      enterLoop
                      (bodygexp, bodyty) <- transexp lvl (Just donelab) bodyexp
                      exitLoop
                      bodyty' <- actualTy (TPar.extractPosition bodyexp) bodyty
                      if (bodyty' == TSty.Unit)
                         then do gexp <- TTra.whileLoop testgexp bodygexp donelab
                                 return (gexp, TSty.Unit)
                         else throwError $ typeMisMatchError pos (show TSty.Unit) (show bodyty)
              else throwError $ typeMisMatchError pos (show TSty.INT) (show testty)
      g (TAbs.ForExp {TAbs.forVar=vardec, TAbs.forLo=lowexp, TAbs.forHi=highexp, TAbs.forBody=bodyexp, TAbs.forPos=pos}) =
        do let itername = TAbs.vardecName vardec
           (lowgexp, lowty) <- g lowexp
           (highgexp, highty) <- g highexp
           if (lowty == highty)
              then if (lowty == TSty.INT)
                      then do (v, t, _, _) <- Frt.semantStGet
                              iteraccess <- liftIO $ TTra.allocInFrame lvl
                              let v' = Map.insert itername (Frt.VarEntry iteraccess TSty.INT True) v
                              itergexp   <- TTra.simpleVar iteraccess lvl
                              donelab <- TTmp.newLabel
                              enterLoop
                              (bodygexp, bodyty) <- withBinding v' t (transexp lvl (Just donelab) bodyexp)
                              exitLoop
                              bodyty' <- actualTy (TPar.extractPosition bodyexp) bodyty
                              if bodyty' == TSty.Unit
                                 then do gexp <- TTra.forLoop lowgexp highgexp bodygexp donelab itergexp
                                         return (gexp, TSty.Unit)
                                 else throwError $ typeMisMatchError pos (show TSty.Unit) (show bodyty)
                      else throwError $ typeMisMatchError pos (show TSty.INT) (show lowty)
              else throwError $ typeMisMatchError pos (show lowty) (show highty)
      g (TAbs.BreakExp pos) =
        do (_, _, l, _) <- Frt.semantStGet
           if (l > 0)
              then case lab of
                     Just lab' -> do gexp <- TTra.break lab'
                                     return (gexp, TSty.Unit)
                     Nothing -> throwError $ breakOutOfLoop pos
              else throwError $ breakOutOfLoop pos
      g (TAbs.LetExp {TAbs.letDecs=decs, TAbs.letBody=bodyexp, TAbs.letPos=pos}) =
        let transdecs (d:[]) = transdec lvl lab d
            transdecs (d:ds) = do (v, t, ges) <- transdec lvl lab d
                                  (v', t', gess) <- withBinding v t $ transdecs ds
                                  return (v', t', ges++gess)
            transdecs [] = do (v, t, _, _) <- Frt.semantStGet
                              return (v, t, [])
        in  do (v', t', ges) <- transdecs decs
               (bodygexp, bodyty) <- withBinding v' t' $ g bodyexp
               gexp <- TTra.letExpression ges bodygexp
               return (gexp, bodyty)
      g (TAbs.ArrayExp {TAbs.arrayTyp=typsym, TAbs.arraySize=sizexp, TAbs.arrayInit=initexp, TAbs.arrayPos=pos}) =
        let checkarray typ ty =  do (sizegexp, sizety) <- g sizexp
                                    if (sizety == TSty.INT)
                                       then do (initgexp, initty) <- g initexp
                                               initty' <- actualTy (TPar.extractPosition initexp) initty
                                               ty' <- actualTy pos ty
                                               if (initty' == ty')
                                                  then do gexp <- TTra.createArray sizegexp initgexp
                                                          return (gexp, typ)
                                                  else throwError $ typeMisMatchError pos (show ty) (show initty)
                                       else throwError $ typeMisMatchError pos (show TSty.INT) (show sizety)
        in do (_, t, _, _) <- Frt.semantStGet
              case Map.lookup typsym t of
                Just typ@(Frt.Array (ty, _)) -> checkarray typ ty
                Just typ@(Frt.Name _) -> do aty <- actualTy pos typ
                                            case aty of
                                              Frt.Array (ty, _) -> checkarray aty ty
                                              _ -> throwError $ typeMisMatchError pos ("Array") (show typsym)
                Just typ -> throwError $ typeMisMatchError pos ("Array") (show typ)
                Nothing -> throwError $ undefinedError pos (TSym.name typsym)
           
  in  g absexp

transvar :: TTra.Level -> Maybe TTmp.Label -> TAbs.Var -> Frontend GexpTy
transvar lvl lab (TAbs.SimpleVar(s, pos)) = do (v, _, _, _) <- Frt.semantStGet
                                               case Map.lookup s v of
                                                 Nothing -> throwError $ undefinedError pos $ TSym.name s
                                                 Just (Frt.VarEntry acc ty _) -> do gexp <- TTra.simpleVar acc lvl
                                                                                    return (gexp, ty)
                                                 Just _ -> throwError $ notVariable pos $ TSym.name s
transvar lvl lab (TAbs.FieldVar(v, s, pos)) = do (vgexp, vty) <- transvar lvl lab v
                                                 vty' <- actualTy (TPar.extractPosition v) vty
                                                 case vty' of
                                                   TSty.Record(symtypairs, _) ->
                                                     case findIndex (\(sym, _) -> sym == s) symtypairs of
                                                       Nothing  -> throwError $ undefinedError pos $ TSym.name s
                                                       Just idx -> do let ty = snd $ symtypairs !! idx
                                                                      gexp <- TTra.field vgexp idx
                                                                      return (gexp, ty)
                                                   _ -> throwError $ typeMisMatchError pos ("Record") (show vty')
transvar lvl lab (TAbs.SubscriptVar(v, idxexp, pos)) = do (vgexp, vty) <- transvar lvl lab v
                                                          (idxgexp, idxty) <- transexp lvl lab idxexp
                                                          idxty' <- actualTy pos idxty
                                                          if idxty' == TSty.INT 
                                                             then do vty' <- actualTy (TPar.extractPosition v) vty
                                                                     case vty' of
                                                                       TSty.Array (innerty, _) -> do gexp <- TTra.subscript vgexp idxgexp
                                                                                                     return (gexp, innerty)
                                                                       _ -> throwError $ typeMisMatchError pos ("Array") (show vty)
                                                             else throwError $ typeMisMatchError pos (show TSty.INT) (show idxty)
                                                 

transty :: TAbs.Ty -> Frontend TSty.Ty
transty (TAbs.NameTy(sym, pos)) =
  do (_, t, _, _) <- Frt.semantStGet
     case Map.lookup sym t of
       Nothing -> throwError $ undefinedError pos $ TSym.name sym 
       Just ty -> return ty

transty (TAbs.ArrayTy(sym, pos)) =
  do (_, t, _, _) <- Frt.semantStGet
     case Map.lookup sym t of
       Nothing -> throwError $ undefinedError pos $ TSym.name sym
       Just ty -> do u <- Frt.genUniq
                     return $ TSty.Array(ty, u)

transty (TAbs.RecordTy tfields) =
  do (_, t, _, _) <- Frt.semantStGet
     let names = map TAbs.tfieldName tfields
     let types = map TAbs.tfieldTyp  tfields
     let poses = map TAbs.tfieldPos  tfields
     case findIndex (not . (flip exists t)) types of
       Nothing -> do let tys = map (fromJust . (flip Map.lookup t)) types
                     u <- Frt.genUniq
                     return $ TSty.Record (zip names tys, u)
       Just idx -> throwError $ undefinedError (poses !! idx) (TSym.name $ names !! idx)
  where exists typ t = typ `Map.member` t


transprog :: Frontend [Frt.Frag]
transprog = let builtintypes = [("string", TSty.String), ("int", TSty.INT)]
                builtinfuncnames = ["chr", "concat", "exit", "flush", "getchar"
                                   ,"not", "ord", "print", "size", "substring"]
                builtinfunctys = [TSty.String, TSty.String, TSty.Unit, TSty.Unit,
                                  TSty.String, TSty.INT, TSty.INT, TSty.Unit,
                                  TSty.INT, TSty.String]
                builtinparamtys = [[TSty.INT], [TSty.String, TSty.String]
                                  ,[TSty.INT], [], [], [TSty.INT], [TSty.String]
                                  ,[TSty.String], [TSty.String]
                                  ,[TSty.String, TSty.INT, TSty.INT]]
                
            in
              do builtintypsym <- mapM TSym.symbol $ map fst builtintypes
                 mapM_ (uncurry addtypetobinding) (zip builtintypsym $ map snd builtintypes)

                 builtinfuncsyms <- mapM TSym.symbol builtinfuncnames
                 builtinfunclabs <- mapM TTmp.namedLabel builtinfuncnames
                 let builtinfunclvls = take (length builtinfuncsyms) $ repeat TTra.outerMost
                 let paramtyswithdummyaccess = liftM (map (\ty -> (ty, (TTra.outerMost, 0)))) builtinparamtys
                 _ <- zipWithM5 addfunctiontobinding builtinfuncsyms builtinfunclvls builtinfunclabs paramtyswithdummyaccess builtinfunctys

                 absyn <- TPar.parser
                 case absyn of
                   TAbs.Pexp e -> do (mainlvl,_) <- TTra.newLevel TTra.outerMost []
                                     (gexp, _) <- transexp mainlvl Nothing e
                                     TTra.createMainFrag mainlvl gexp
                                     frags <- TTra.getResult
                                     return frags
                   TAbs.Pdecs (ds) -> do (mainlvl,_) <- TTra.newLevel TTra.outerMost []
                                         transprog' mainlvl ds
                                         frags <- TTra.getResult
                                         return frags
  where transprog' mainlvl (d:decs) = do (v, t, _) <- transdec mainlvl Nothing d
                                         withBinding v t $ transprog' mainlvl decs
                                         return ()
        transprog' _ _ = return ()
