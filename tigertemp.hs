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
           | SRC  Int
           | DST  Int
           | Named Register
          deriving (Ord, Eq)

instance Show Temp where
  show (TEMP d) = "T"++show d
  show (Named r) = show r
  show (SRC d) = "`S"++show d
  show (DST d) = "`D"++show d
