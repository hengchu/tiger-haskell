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
          deriving (Show, Eq)
