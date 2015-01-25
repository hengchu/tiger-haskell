module TigerSymbol
  (
    SymbolMap
  , Symbol
  , symbolMapFromStrings
  , symbol
  , name
  ) where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Maybe (fromJust)

type Symbol = (String, Int)
type SymbolMap = Map.Map String Int

addString :: String -> State (SymbolMap, Int) Symbol
addString str = do
  (m, c) <- get
  (m', sym, hasStr) <- return (addString_ str m c)
  unless hasStr (put (m', c+1))
  return sym

addString_ :: String -> SymbolMap -> Int -> (SymbolMap, Symbol, Bool)
addString_ str m c = if str `Map.member` m
                        then (m, (str, fromJust $ Map.lookup str m), True)
                        else (m', (str, c+1), False)
                     where m' = Map.insert str (c+1) m

symbolMapFromStrings :: [String] -> SymbolMap
symbolMapFromStrings strs = fst $ execState (mapM addString strs) (Map.empty, 0)

symbol :: String -> SymbolMap -> Maybe Symbol
symbol str m = if str `Map.member` m
                  then Just (str, fromJust (Map.lookup str m))
                  else Nothing

name :: Symbol -> String
name (str, _) = str
