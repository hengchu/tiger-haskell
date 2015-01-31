module TigerSementTypes
       (
         Ty(..)
       , Uniq
       ) where

import qualified TigerSymbol as S
import Data.IORef

type Uniq = Integer
data Ty = Record ([(S.Symbol, Ty)], Uniq)
        | Nil
        | INT
        | String
        | Array (Ty, Uniq)
        | Name (S.Symbol, IORef (Maybe Ty))
        | Unit
        deriving (Eq)

instance Show Ty where
  show (Record (xs, u)) = "Record: (" ++ show xs ++ " )"
  show Nil = "Nil"
  show INT = "INT"
  show String = "String"
  show (Array(t, u)) = "Array: (" ++ show t ++ "[])"
  show (Name(s, ref)) = "Name: (" ++ show s ++ ", <ioref>)"
  show Unit = "Unit"
