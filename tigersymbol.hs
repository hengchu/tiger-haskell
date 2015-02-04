module TigerSymbol
  (
    SymbolMap
  , Symbol
  , symbol
  , name
  ) where

import qualified Data.Map.Strict as Map
import Data.IORef

type Symbol = (String, Int)
type SymbolMap = Map.Map String Int

symbolState :: IO (IORef (SymbolMap, Int))
symbolState = newIORef (Map.empty, 0)

symbol :: String -> IO Symbol
symbol str = do state <- symbolState
                (m, c) <- readIORef state
                case Map.lookup str m of
                  Nothing -> do let m' = Map.insert str c m
                                writeIORef state (m', c+1)
                                return (str, c)
                  Just c' -> return (str, c')

name :: Symbol -> String
name (str, _) = str
