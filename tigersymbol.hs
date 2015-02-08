module TigerSymbol
  (
    SymbolMap
  , Symbol
  , symbol
  , name
  ) where

import qualified Data.Map.Strict as Map
import System.IO.Unsafe
import Data.IORef

type Symbol = (String, Int)
type SymbolMap = Map.Map String Int

symbolState :: IORef (SymbolMap, Int)
{-# NOINLINE symbolState #-}
symbolState = unsafePerformIO $ newIORef (Map.empty, 0)

symbol :: String -> IO Symbol
symbol str = do (m, c) <- readIORef symbolState
                case Map.lookup str m of
                  Nothing -> do let m' = Map.insert str c m
                                writeIORef symbolState (m', c+1)
                                return (str, c)
                  Just c' -> return (str, c')

name :: Symbol -> String
name (str, _) = str
