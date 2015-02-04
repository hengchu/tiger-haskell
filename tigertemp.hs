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

data Temp  = Temp Int
type Label = Symbol

instance Show Temp where
  show (Temp num) = "T" ++ show num

tempCount :: IO (IORef Int)
tempCount  = newIORef 0

labCount :: IO (IORef Int)
labCount = newIORef 0

newTemp :: IO Temp
newTemp = do tempref <- tempCount
             c <- readIORef tempref
             writeIORef tempref (c+1)
             return $ Temp c

newLabel :: IO Label
newLabel = do labref <- labCount
              c <- readIORef labref
              writeIORef labref (c+1)
              symbol $ "L" ++ show c

namedLabel :: String -> IO Label
namedLabel = symbol
