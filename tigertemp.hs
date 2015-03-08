module TigerTemp
  (
    Label
  , Temp(..)
  )
  where

import TigerRegisters
import TigerSymbol

type Label = Symbol

data Temp  = TEMP Int
           | Named Register
          deriving (Eq)

instance Show Temp where
  show (TEMP d) = "T"++show d
  show (Named r) = show r
