module TigerAssem
  (
    Assem(..)
  , Instr(..)
  , Lab
  , Offset
  , mkaddr
  , defs
  , uses
  )
  where

import TigerTemp
import Data.List

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
           | ANDCR     Int      Temp
           | ANDCM     Int      Addr
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
          deriving (Eq)


data Instr = OPER  { opAssem::Assem, opSrc::[Temp], opDst::[Temp], opJump::Maybe [Lab] }
           | LABEL { labLab::Lab }
           | MOV   { movAssem::Assem, movSrc::Temp, movDst::Temp }
           | CMT Assem
          deriving (Eq)

defs :: Instr -> [Temp]
defs instr = case instr of
               OPER _ _ ds _ -> ds
               LABEL _ -> []
               MOV _ _ d -> [d]
               CMT _ -> []

uses :: Instr -> [Temp]
uses instr = case instr of
                  OPER _ us _ _ -> us
                  LABEL _ -> []
                  MOV _ u _ -> [u]
                  CMT _ -> []


showaddr :: Addr -> String
showaddr (t, off) = show off++"(%"++show t++")"

instance Show Assem where
  show (MOVRR t1 t2) = "movl %"++show t1++", %"++show t2
  show (MOVRM t1 addr) = "movl %"++show t1++", "++showaddr addr
  show (MOVMR addr t2) = "movl "++show addr++", %"++show t2
  show (MOVCR d t) = "movl $"++show d++", %"++show t
  show (MOVCM d addr) = "movl $"++show d++", "++show addr
  show (PUSH t) = "pushl %"++show t
  show (PUSHC d) = "pushl $"++show d
  show (PUSHM addr) = "pushl "++showaddr addr
  show (POP t) = "popl %"++show t
  show (LEAL lab t) = "leal $"++lab++", %"++show t
  show (LEAM addr t) = "leal "++showaddr addr++", %"++show t
  show (ADDRR t1 t2) = "addl %"++show t1++", %"++show t2
  show (ADDRM t addr) = "addl %"++show t++", "++showaddr addr
  show (ADDMR addr t) = "addl "++showaddr addr++", %"++show t
  show (ADDCR d t) = "addl $"++show d++", %"++show t
  show (ADDCM d addr) = "addl $"++show d++", "++showaddr addr
  show (SUBRR t1 t2) = "subl %"++show t1++", %"++show t2
  show (SUBRM t addr) = "subl %"++show t++", "++showaddr addr
  show (SUBMR addr t) = "subl "++showaddr addr++", %"++show t
  show (SUBCR d t) = "subl $"++show d++", %"++show t
  show (SUBCM d addr) = "subl $"++show d++", "++showaddr addr
  show (INCR t) = "incl %"++show t
  show (INCM addr) = "incl "++showaddr addr
  show (DECR t) = "decl %"++show t
  show (DECM addr) = "decl "++showaddr addr
  show (IMULRR t1 t2) = "imul %"++show t1++", %"++show t2
  show (IMULRM t addr) = "imul %"++show t++", "++showaddr addr
  show (IMULRR2 t1 t2 d) = "imul %"++show t1++", %"++show t2++", $"++show d
  show (IMULRM2 t addr d) = "imul %"++show t++", "++showaddr addr++", $"++show d
  show (IDIVR t) = "idivl %"++show t
  show (IDIVM addr) = "idivl "++showaddr addr
  show CDQ = "cdq"
  show (ANDRR t1 t2) = "andl %"++show t1++", %"++show t2
  show (ANDRM t addr) = "andl %"++show t++", "++showaddr addr
  show (ANDMR addr t) = "andl "++showaddr addr++", %"++show t
  show (ANDCR d t) = "andl $"++show d++", %"++show t
  show (ANDCM d addr) = "andl $"++show d++", "++showaddr addr
  show (ORRR t1 t2) = "orl %"++show t1++", %"++show t2
  show (ORRM t addr) = "orl %"++show t++", "++showaddr addr
  show (ORMR addr t) = "orl "++showaddr addr++", %"++show t
  show (ORCR d t) = "orl $"++show d++", %"++show t
  show (ORCM d addr) = "orl $"++show d++", "++showaddr addr
  show (XORRR t1 t2) = "xorl %"++show t1++", %"++show t2
  show (XORRM t addr) = "xorl %"++show t++", "++showaddr addr
  show (XORMR addr t) = "xorl "++showaddr addr++", %"++show t
  show (XORCR d t) = "xorl $"++show d++", %"++show t
  show (XORCM d addr) = "xorl $"++show d++", "++showaddr addr
  show (NOTR t) = "notl %"++show t
  show (NOTM addr) = "notl "++showaddr addr
  show (NEGR t) = "negl %"++show t
  show (NEGM addr) = "negl "++showaddr addr
  show (JMP lab) = "jmp $"++lab
  show (JE lab) = "je $"++lab
  show (JNE lab) = "jne $"++lab
  show (JZ lab) = "jz $"++lab
  show (JG lab) = "jg $"++lab
  show (JGE lab) = "jge $"++lab
  show (JL lab) = "jl $"++lab
  show (JLE lab) = "jle $"++lab
  show (CMPRR t1 t2) = "cmpl %"++show t1++", %"++show t2
  show (CMPRM t addr) = "cmpl %"++show t++", "++showaddr addr
  show (CMPMR addr t) = "cmpl "++showaddr addr++", %"++show t
  show (CMPCR d t) = "cmpl $"++show d++", %"++show t
  show (CALLR t) = "call %"++show t
  show (CALLL lab) = "call $"++lab
  show RET = "ret"
  show (COMMENT str) = "# "++str

instance Show Instr where
  show (OPER assem _ _ Nothing) = show assem
  show (OPER assem _ _ (Just labs)) = show assem++", labs=["++(concat.intersperse ",") labs++"]"
  show (LABEL lab) = lab++":"
  show (MOV assem _ _) = show assem
  show (CMT assem) = show assem
