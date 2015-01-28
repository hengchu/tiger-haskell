module TigerSement
       (
         transProg
       , SementError
       ) where

import qualified TigerSementTypes as TigSTy
import qualified TigerLexer as TLex
import qualified TigerAbsyn as TAbsyn
import qualified TigerSymbol as TSym
import Data.Map
import Control.Monad.State
import Data.Either

type SementError = (TLex.AlexPosn, String)
type ExpTy = TigSTy.Ty

data EnvEntry = VarEntry {varTy::TigSTy.Ty, varReadOnly::Bool}
              | FunEntry {funLevel::Int, funFormals::[TigSTy.Ty], funResult::TigSTy.Ty}

type Venv    = Map TSym.Symbol EnvEntry
type Tenv    = Map TSym.Symbol TigSTy.Ty
type SementStates = (Venv, Tenv, TigSTy.Uniq)

type SementState a = State SementStates a

emptyState = (empty, empty, 0 :: TigSTy.Uniq)

-- Generates a new value of Uniq type
genUnique :: SementState TigSTy.Uniq
genUnique = do (venv, tenv, a) <- get
               put (venv, tenv, a+1)
               return a

transVar :: TAbsyn.Var -> SementState (Either SementError ExpTy)
transVar = undefined

transExp :: TAbsyn.Exp -> SementState (Either SementError ExpTy)
transExp = undefined

transDec :: TAbsyn.Dec -> SementState (Either SementError ())
transDec = undefined

transTy :: TAbsyn.Ty -> SementState (Either SementError TigSTy.Ty)
transTy = undefined

transProg' :: SementStates -> TAbsyn.Program -> Either SementError ()
transProg' initialS (TAbsyn.Pexp e) = do _ <- evalState (transExp e) initialS
                                         return ()

transProg' initialS (TAbsyn.Pdecs (d:decs)) = let (a, s) = runState (transDec d) initialS
                                              in
                                                case (isRight a) of 
                                                  True  -> transProg' s (TAbsyn.Pdecs decs)
                                                  False -> a
transProg' _ (TAbsyn.Pdecs []) = return ()

transProg :: TAbsyn.Program -> Either SementError ()
transProg = transProg' emptyState
