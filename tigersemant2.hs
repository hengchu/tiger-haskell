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

undefinedError :: TLex.AlexPosn -> String -> Frt.SemantError
undefinedError pos str = Frt.SE pos $ Frt.Undefined str

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
                  let argformaltys = zip argtys formaltys
                  if all (\(a, b) -> a == b) argformaltys then
                     do gexp <- TTra.callFunction funlab lvl funlvl ges
                        return (gexp, funresult)
                  else do let violators = filter (\(a, b) -> a /= b) argformaltys
                          let maybeidx  = (head violators) `elemIndex` argformaltys
                          case maybeidx of
                            Just idx -> throwError $ typeMisMatchError 
                                                     (TPar.extractPosition $ funcargs!!idx) 
                                                     (show $ fst $ head violators) (show $ snd $ head violators)
                            Nothing  -> error "Compiler error: fatal error when checking function application arguments."
             Just _ -> throwError $ notCallableError pos $ TSym.name funcsym
             Nothing -> throwError $ undefinedError pos $ TSym.name funcsym
                          
                                  
                                  
  in  g absexp

transvar :: TTra.Level -> Maybe TTmp.Label -> TAbs.Var -> Frontend GexpTy
transvar = undefined

transty :: TAbs.Ty -> Frontend TSty.Ty
transty = undefined
