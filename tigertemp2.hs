module TigerTemp2
  (
    Label
  , Temp (..)
  , newTemp
  , newLabel
  , namedLabel
  )
  where

import TigerSymbol2
import FrontEnd
import Text.Parsec


newTemp :: Frontend Temp
newTemp = do (m, c, tc, lc, u) <- getState
             putState (m, c, tc+1, lc, u)
             return $ Temp tc

newLabel :: Frontend Label
newLabel = do (m, c, tc, lc, u) <- getState
              putState (m, c, tc, lc+1, u)
              symbol $ "L"++show lc

namedLabel :: String -> Frontend Label
namedLabel = TigerSymbol2.symbol
