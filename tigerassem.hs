module TigerAssem
  (
    Assem(..)
  , Instr(..)
  , Lab
  , Offset
  , mkaddr
  )
  where

import TigerTemp

type Addr   = (Temp, Offset)
type Lab    = String
type Offset = Int

mkaddr :: Temp -> Offset -> Addr
mkaddr reg offset = (reg, offset)

data Assem = MOVRR     Temp     Temp
           | MOVRM     Temp     Addr
           | MOVMR     Addr     Temp
           | MOVCR     Int      Temp
           | MOVCM     Int      Addr
           | PUSH      Temp
           | PUSHC     Int
           | PUSHM     Addr
           | POP       Temp
           | LEAL      Lab      Temp
           | LEAM      Addr     Temp
           | ADDRR     Temp     Temp
           | ADDRM     Temp     Addr
           | ADDMR     Addr     Temp
           | ADDCR     Int      Temp
           | ADDCM     Int      Addr
           | SUBRR     Temp     Temp -- subtract rl  from rr
           | SUBRM     Temp     Addr -- subtract reg from mem
           | SUBMR     Addr     Temp -- subtract mem from reg
           | SUBCR     Int      Temp
           | SUBCM     Int      Addr
           | INCR      Temp
           | INCM      Addr
           | DECR      Temp
           | DECM      Addr
           | IMULRR    Temp     Temp
           | IMULRM    Temp     Addr
           | IMULRR2   Temp     Temp    Int
           | IMULRM2   Temp     Addr    Int
           | IDIVR     Temp
           | IDIVM     Addr
           | CDQ
           | ANDRR     Temp     Temp
           | ANDRM     Temp     Addr
           | ANDMR     Addr     Temp
           | ANDCRC    Int      Temp
           | ANDCMC    Int      Addr
           | ORRR      Temp     Temp
           | ORRM      Temp     Addr
           | ORMR      Addr     Temp
           | ORCR      Int      Temp
           | ORCM      Int      Addr
           | XORRR     Temp     Temp
           | XORRM     Temp     Addr
           | XORMR     Addr     Temp
           | XORCR     Int      Temp
           | XORCM     Int      Addr
           | NOTR      Temp
           | NOTM      Addr
           | NEGR      Temp
           | NEGM      Addr
           | JMP       Lab
           | JE        Lab
           | JNE       Lab
           | JZ        Lab
           | JG        Lab
           | JGE       Lab
           | JL        Lab
           | JLE       Lab
           | CMPRR     Temp     Temp
           | CMPRM     Temp     Addr
           | CMPMR     Addr     Temp
           | CMPCR     Int      Temp
           | CALLR     Temp
           | CALLL     Lab
           | RET
           | COMMENT   String
          deriving (Show, Eq)


data Instr = OPER  { opAssem::Assem, opSrc::[Temp], opDst::[Temp], opJump::Maybe [Lab] }
           | LABEL { labLab::Lab }
           | MOV   { movAssem::Assem, movSrc::Temp, movDst::Temp }
           | CMT Assem
          deriving (Show, Eq)
