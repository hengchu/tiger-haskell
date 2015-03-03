module TigerGenSymLabTmp
  (
    symbol
  , name
  , newLabel
  , newTemp
  , namedLabel
  , GenSymLabTmp
  , GenSymLabTmpState
  , runGSLT
  , initialGSLTState
  )
  where

import TigerSymbol
import TigerTemp
import qualified Data.Map as Map
import Control.Monad.State

type GenSymLabTmpState = ( Int
                         , Int -- Temp count
                         , Int -- Lab count
                         , Map.Map String Int
                         )
type GenSymLabTmp m  = StateT GenSymLabTmpState m

initialGSLTState :: GenSymLabTmpState
initialGSLTState = (0, 0, 0, Map.empty)

getSymbolState :: (Monad m) => GenSymLabTmp m Int
getSymbolState = do (s, _, _, _) <- get
                    return s

putSymbolState :: (Monad m) => Int -> GenSymLabTmp m ()
putSymbolState s = do (_, t, l, m) <- get
                      put (s, t, l, m)

getTempState :: (Monad m) => GenSymLabTmp m Int
getTempState = do (_, t, _, _) <- get
                  return t

putTempState :: (Monad m) => Int -> GenSymLabTmp m ()
putTempState t = do (s, _, l, m) <- get
                    put (s, t, l, m)

getLabelState :: (Monad m) => GenSymLabTmp m Int
getLabelState = do (_, _, l, _) <- get
                   return l

putLabelState :: (Monad m) => Int -> GenSymLabTmp m ()
putLabelState l = do (s, t, _, m) <- get
                     put (s, t, l, m)

getSymbolState2 :: (Monad m) => GenSymLabTmp m (Map.Map String Int)
getSymbolState2 = do (_, _, _, m) <- get
                     return m

putSymbolState2 :: (Monad m) => Map.Map String Int -> GenSymLabTmp m ()
putSymbolState2 m = do (s, t, l, _) <- get
                       put (s, t, l, m)

symbol :: (Monad m) => String -> GenSymLabTmp m Symbol
symbol str = do m <- getSymbolState2
                s <- getSymbolState
                case Map.lookup str m of
                  Just c -> return (str, c)
                  Nothing -> do let map' = Map.insert str s m
                                putSymbolState $ s+1
                                putSymbolState2 map'
                                return (str, s)

namedLabel :: (Monad m) => String -> GenSymLabTmp m Label
namedLabel = symbol

name :: Symbol -> String
name sym = fst sym

newLabel :: (Monad m) => GenSymLabTmp m Label
newLabel = do l <- getLabelState
              lab <- symbol ("L"++show l)
              putLabelState $ l+1
              return lab

newTemp :: (Monad m) => GenSymLabTmp m Temp
newTemp = do t <- getTempState
             putTempState $ t+1
             return $ TEMP t

runGSLT :: GenSymLabTmpState -> (GenSymLabTmp m a) -> m (a, GenSymLabTmpState)
runGSLT st m = runStateT m st
