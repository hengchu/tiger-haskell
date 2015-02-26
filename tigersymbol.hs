module TigerSymbol
  (
    symbol
  , name
  , Symbol
  )
  where

import Text.Parsec
import qualified Data.Map as Map

import FrontEnd

symbol :: String -> Frontend Symbol
symbol str = do (m, c, tc, lc, u) <- getState
                case Map.lookup str m of
                  Just c' -> return $ (str, c')
                  Nothing -> do let m' = Map.insert str c m
                                putState (m', c+1, tc, lc, u)
                                return $ (str, c)

name :: Symbol -> String
name (str, _) = str
