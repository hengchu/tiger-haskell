module TigerTemp
       ( 
         Temp(..)
       , Label
       , newTemp
       , newLabel
       , namedLabel
       )
       where

import Data.IORef
import TigerSymbol
import TigerRegisters
import Distribution.Simple.Utils (lowercase)
import System.IO.Unsafe

data Temp  = Temp Int
           | Named Register
             deriving (Eq)
type Label = Symbol

instance Show Temp where
  show (Temp num) = "T" ++ show num
  show (Named reg) = lowercase $ show reg

tempCount :: IORef Int
{-# NOINLINE tempCount #-}
tempCount  = unsafePerformIO $ newIORef 0

labCount :: IORef Int
{-# NOINLINE labCount #-}
labCount = unsafePerformIO $ newIORef 0

newTemp :: IO Temp
newTemp = do c <- readIORef tempCount
             writeIORef tempCount (c+1)
             return $ Temp c

newLabel :: IO Label
newLabel = do c <- readIORef labCount
              writeIORef labCount (c+1)
              symbol $ "L" ++ show c

namedLabel :: String -> IO Label
namedLabel = symbol
