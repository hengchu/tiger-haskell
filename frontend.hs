module FrontEnd
  (
    Semant
  , SemantError(..)
  , SemantState
  , PToken(..)
  , Frontend
  , SymbolTempState
  , semantStGet
  , semantStPut
  )
  where

-- Label/Symbol > Parser > SemanticAnalysis

import Text.Parsec
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

import qualified TigerLexer as TLex

type SemantState = (Bool, Int) 
data SemantError = SE TLex.AlexPosn String
  deriving(Show, Eq)
type Semant = StateT SemantState (ExceptT SemantError IO)

data PToken = PToken TLex.AlexPosn TLex.TokenClass
  deriving(Show, Eq)
type SymbolTempState = (Map.Map String Int, Int)
type Frontend = ParsecT [PToken] SymbolTempState Semant

semantStGet :: Frontend SemantState
semantStGet = lift get

semantStPut :: SemantState -> Frontend ()
semantStPut = lift . put
