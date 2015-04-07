module TigerAssem
  (
    Assem(..)
  , Instr(..)
  , Lab
  , Offset
  , Addr
  , mkaddr
  , defs
  , uses
  , instrfmt
  )
  where

import TigerTemp
import TigerRegisters
import qualified Data.Map as Map
import Data.Maybe (fromJust)

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
          deriving (Show, Eq)


data Instr = OPER  { opAssem::Assem, opSrc::[Temp], opDst::[Temp], opJump::Maybe [Lab] }
           | LABEL { labLab::Lab }
           | MOV   { movAssem::Assem, movSrc::Temp, movDst::Temp }
           | CMT Assem
           | DIRECTIVE String
          deriving (Show, Eq)

defs :: Instr -> [Temp]
defs instr = case instr of
               OPER _ _ ds _ -> ds
               LABEL _ -> []
               MOV _ _ d -> [d]
               CMT _ -> []
               DIRECTIVE _ -> []

uses :: Instr -> [Temp]
uses instr = case instr of
                  OPER _ us _ _ -> us
                  LABEL _ -> []
                  MOV _ u _ -> [u]
                  CMT _ -> []
                  DIRECTIVE _ -> []


instrfmt :: Instr -> Map.Map Temp Register -> String
instrfmt instr allocation =
  let
     showtemp (SRC d) srcs dsts = showtemp (srcs !! d) srcs dsts
     showtemp (DST d) srcs dsts = showtemp (dsts !! d) srcs dsts
     showtemp t@(TEMP _) _ _ = show $ fromJust $ Map.lookup t allocation
     showtemp t _ _ = show t

     showaddr1 :: Addr -> [Temp] -> [Temp] -> String
     showaddr1 (t, off) srcs dsts = show off++"("++showtemp t srcs dsts++")"

     ispseudo (PSEUDO _) = True
     ispseudo _ = False

     map2pseudo (SRC d) srcs dsts = (ispseudo $ fromJust $ Map.lookup (srcs!!d) allocation)
     map2pseudo (DST d) srcs dsts = (ispseudo $ fromJust $ Map.lookup (dsts!!d) allocation)
     map2pseudo t _ _ = ispseudo $ fromJust $ Map.lookup t allocation

     showas as srcs dsts =
       let showtmp = \t -> showtemp t srcs dsts
           showadr = \adr -> showaddr1 adr srcs dsts
       in case as of
            MOVRR t1 t2 -> if map2pseudo t1 srcs dsts && map2pseudo t2 srcs dsts
                              then error $ "Compiler error: " ++ show t1 ++ ", " ++ show t2 ++ ", " ++ show srcs ++ ", " ++ show dsts
                              else "movl "++showtmp t1++", "++showtmp t2
            MOVRM t1 addr -> if map2pseudo t1 srcs dsts
                                then error $ "Compiler error: " ++ show t1 ++ show srcs ++ ", " ++ show dsts
                                else "movl "++showtmp t1++", "++showadr addr
            MOVMR addr t2 -> if map2pseudo t2 srcs dsts
                                then error $ "Compiler error: " ++ show t2 ++ show srcs ++ ", " ++ show dsts
                                else "movl "++showadr addr++", "++showtmp t2
            MOVCR d t -> "movl $"++show d++", "++showtmp t
            MOVCM d addr -> "movl $"++show d++", "++showadr addr
            PUSH t -> "pushl "++showtmp t
            PUSHC d -> "pushl $"++show d
            PUSHM addr -> "pushl "++showadr addr
            POP t -> "popl "++showtmp t
            LEAL lab t -> "leal "++lab++", "++showtmp t
            LEAM addr t -> "leal "++showadr addr++", "++showtmp t
            ADDRR t1 t2 -> "addl "++showtmp t1++", "++showtmp t2
            ADDRM t addr -> "addl "++showtmp t++", "++showadr addr
            ADDMR addr t -> "addl "++showadr addr++", "++showtmp t
            ADDCR d t -> "addl $"++show d++", "++showtmp t
            ADDCM d addr -> "addl $"++show d++", "++showadr addr
            SUBRR t1 t2 -> "subl "++showtmp t1++", "++showtmp t2
            SUBRM t addr -> "subl "++showtmp t++", "++showadr addr
            SUBMR addr t -> "subl "++showadr addr++", "++showtmp t
            SUBCR d t -> "subl $"++show d++", "++showtmp t
            SUBCM d addr -> "subl $"++show d++", "++showadr addr
            INCR t -> "incl "++showtmp t
            INCM addr -> "incl "++showadr addr
            DECR t -> "decl "++showtmp t
            DECM addr -> "decl "++showadr addr
            IMULRR t1 t2 -> "imul "++showtmp t1++", "++showtmp t2
            IMULRM t addr -> "imul "++showtmp t++", "++showadr addr
            IMULRR2 t1 t2 d -> "imul "++showtmp t1++", "++showtmp t2++", $"++show d
            IMULRM2 t addr d -> "imul "++showtmp t++", "++showadr addr++", $"++show d
            IDIVR t -> "idivl "++showtmp t
            IDIVM addr -> "idivl "++showadr addr
            CDQ -> "cdq"
            ANDRR t1 t2 -> "andl "++showtmp t1++", "++showtmp t2
            ANDRM t addr -> "andl "++showtmp t++", "++showadr addr
            ANDMR addr t -> "andl "++showadr addr++", "++showtmp t
            ANDCR d t -> "andl $"++show d++", "++showtmp t
            ANDCM d addr -> "andl $"++show d++", "++showadr addr
            ORRR t1 t2 -> "orl "++showtmp t1++", "++showtmp t2
            ORRM t addr -> "orl "++showtmp t++", "++showadr addr
            ORMR addr t -> "orl "++showadr addr++", "++showtmp t
            ORCR d t -> "orl $"++show d++", "++showtmp t
            ORCM d addr -> "orl $"++show d++", "++showadr addr
            XORRR t1 t2 -> "xorl "++showtmp t1++", "++showtmp t2
            XORRM t addr -> "xorl "++showtmp t++", "++showadr addr
            XORMR addr t -> "xorl "++showadr addr++", "++showtmp t
            XORCR d t -> "xorl $"++show d++", "++showtmp t
            XORCM d addr -> "xorl $"++show d++", "++showadr addr
            NOTR t -> "notl "++showtmp t
            NOTM addr -> "notl "++showadr addr
            NEGR t -> "negl "++showtmp t
            NEGM addr -> "negl "++showadr addr
            JMP lab -> "jmp "++lab
            JE lab -> "je "++lab
            JNE lab -> "jne "++lab
            JZ lab -> "jz "++lab
            JG lab -> "jg "++lab
            JGE lab -> "jge "++lab
            JL lab -> "jl "++lab
            JLE lab -> "jle "++lab
            CMPRR t1 t2 -> "cmpl "++showtmp t1++", "++showtmp t2
            CMPRM t addr -> "cmpl "++showtmp t++", "++showadr addr
            CMPMR addr t -> "cmpl "++showadr addr++", "++showtmp t
            CMPCR d t -> "cmpl $"++show d++", "++showtmp t
            CALLR t -> "call "++showtmp t
            CALLL lab -> "call "++lab
            RET -> "ret"
            COMMENT str -> "# "++str

  in case instr of
       LABEL lab -> lab++":"
       CMT (COMMENT cmt) -> "# "++cmt
       CMT _ -> error "Compiler error: Illegal CMT pattern."
       OPER as srcs dsts _ -> showas as srcs dsts
       MOV  as src dst -> showas as [src] [dst]
       DIRECTIVE str -> str

{-
showaddr :: Addr -> String
showaddr (t, off) = show off++"(%"++show t++")"

instance Show Assem where
  show (MOVRR t1 t2) = "movl %"++show t1++", %"++show t2
  show (MOVRM t1 addr) = "movl %"++show t1++", "++showaddr addr
  show (MOVMR addr t2) = "movl "++showaddr addr++", %"++show t2
  show (MOVCR d t) = "movl $"++show d++", %"++show t
  show (MOVCM d addr) = "movl $"++show d++", "++showaddr addr
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
  show (JMP lab) = "jmp "++lab
  show (JE lab) = "je "++lab
  show (JNE lab) = "jne "++lab
  show (JZ lab) = "jz "++lab
  show (JG lab) = "jg "++lab
  show (JGE lab) = "jge "++lab
  show (JL lab) = "jl "++lab
  show (JLE lab) = "jle "++lab
  show (CMPRR t1 t2) = "cmpl %"++show t1++", %"++show t2
  show (CMPRM t addr) = "cmpl %"++show t++", "++showaddr addr
  show (CMPMR addr t) = "cmpl "++showaddr addr++", %"++show t
  show (CMPCR d t) = "cmpl $"++show d++", %"++show t
  show (CALLR t) = "call %"++show t
  show (CALLL lab) = "call $"++lab
  show RET = "ret"
  show (COMMENT str) = "# "++str
-}

{-
instance Show Instr where
  show (OPER assem _ _ Nothing) = show assem
  show (OPER assem _ _ (Just labs)) = show assem++", labs=["++(concat.intersperse ",") labs++"]"
  show (LABEL lab) = lab++":"
  show (MOV assem _ _) = show assem
  show (CMT assem) = show assem
  show (DIRECTIVE str) = str
-}