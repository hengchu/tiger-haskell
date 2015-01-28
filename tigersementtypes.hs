module TigerSementTypes
       (
         Ty(..)
       , Uniq
       ) where

import qualified TigerSymbol as S

type Uniq = Integer
data Ty = Record ([(S.Symbol, Ty)], Uniq)
        | Nil
        | INT
        | String
        | Array (Ty, Uniq)
        | Name (S.Symbol, Maybe Ty)
        | Unit
        deriving (Show, Eq)
