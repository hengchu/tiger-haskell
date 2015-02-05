module TigerITree
       ( Stm(..)
       , Exp(..)
       , Test(..)
       , Binop(..)
       , Relop(..)
       , Cvtop(..)
       )
       where

import qualified TigerTemp as Temp

type Size = Int

data Stm = SEQ   (Stm, Stm)
         | LABEL (Temp.Label)
         | JUMP  (Exp, [Temp.Label])
         | CJUMP (Test, Temp.Label, Temp.Label)
         | MOVE  (Exp, Exp)
         | EXP   Exp
         deriving (Show, Eq)

data Exp = BINOP  (Binop, Exp, Exp)
         | CVTOP  (Cvtop, Exp, Size, Size)
         | MEM    (Exp, Size)
         | TEMP   Temp.Temp
         | ESEQ   (Stm, Exp)
         | NAME   Temp.Label
         | CONST  Int
         | CONSTF Float
         | CALL   (Exp, [Exp])
         deriving (Show, Eq)

data Test = TEST (Relop, Exp, Exp)
         deriving (Show, Eq)

data Binop = FPLUS | FMINUS | FDIV | FMUL
           | PLUS  | MINUS  | MUL  | DIV
           | AND   | OR     | LSHIFT | RSHIFT | ARSHIFT | XOR
         deriving (Show, Eq)

data Relop = EQ | NE | LT | GT | LE | GE
           | ULT | ULE | UGT | UGE
           | FEQ | FNE | FLT | FLE | FGT | FGE
         deriving (Show, Eq)

data Cvtop = CVTSU | CVTSS | CVTSF | CVTUU
           | CVTUS | CVTFS | CVTFF
         deriving (Show, Eq)
