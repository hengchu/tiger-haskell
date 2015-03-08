module TigerCodeGen
  (
    codegen
  )
  where

import TigerAssem
import qualified TigerTemp as Tmp
import TigerRegisters
import TigerITree
import qualified TigerGenSymLabTmp as TGSLT
import Prelude hiding (EQ, LT, GT)
import Control.Monad.State
import Control.Monad.Identity

named :: Register -> Tmp.Temp
named = Tmp.Named

codegen :: Stm -> TGSLT.GenSymLabTmpState -> ([Instr], TGSLT.GenSymLabTmpState)
codegen stm st = let monad = codegen' stm
                     gsltmonad = execStateT monad []
                     (instrs, st') = (runIdentity . TGSLT.runGSLT st) gsltmonad
                 in  (reverse instrs, st')

stringdata :: Tmp.Label -> String -> String
stringdata = undefined

-- Codegen Monad
type Codegen = StateT [Instr] (TGSLT.GenSymLabTmp Identity)

newTemp :: Codegen Tmp.Temp
newTemp = lift $ TGSLT.newTemp

emit :: Instr -> Codegen ()
emit instr = do instrs <- get
                put $ instr:instrs

codegen' :: Stm -> Codegen ()
codegen' s0 =
  let

    comment :: String -> Instr
    comment str = CMT $ COMMENT str

    op2jmp :: Relop -> String -> Assem
    op2jmp EQ = JE
    op2jmp NE = JNE
    op2jmp LT = JL
    op2jmp GT = JG
    op2jmp LE = JLE
    op2jmp GE = JGE
    op2jmp op = error $ "Compiler error: Jump operator " ++ show op ++ " not yet implemented."

    genCall :: Tmp.Label -> [Exp] -> Bool -> Codegen ()
    genCall f args shouldsavecaller =
      do when (shouldsavecaller) saveCallerSaves
         emit $ comment "Pushing arguments on stack in reverse order"
         mapM_ munchArg $ reverse args
         emit $ comment "Done pushing arguments"
         emit $ OPER (CALLL $ TGSLT.name f) [named ESP] (map named [ESP, EBP]) Nothing
         when (length args > 0) $
           do emit $ comment "Free arguments from stack"
              emit $ OPER (ADDCR (length args * 4) (named ESP))
                          [named ESP] (map named [ESP]) Nothing
              emit $ comment "Done freeing arguments from stack"
         when (shouldsavecaller) restoreCallerSaves

    saveCallerSaves :: Codegen ()
    saveCallerSaves =
      do emit $ comment "Pushing caller save registers on stack"
         emit $ OPER (PUSH $ named EAX) (map named [EAX, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named ECX) (map named [ECX, ESP]) [named ESP] Nothing
         emit $ OPER (PUSH $ named EDX) (map named [EDX, ESP]) [named ESP] Nothing
         emit $ comment "Done pushing caller save registers on stack"
         

    restoreCallerSaves :: Codegen ()
    restoreCallerSaves =
      do emit $ comment "Restoring caller save registers"
         emit $ OPER (POP $ named EDX) [named ESP] (map named [EDX, ESP]) Nothing
         emit $ OPER (POP $ named ECX) [named ESP] (map named [ECX, ESP]) Nothing
         emit $ OPER (POP $ named EAX) [named ESP] (map named [EAX, ESP]) Nothing
         emit $ comment "Done restoring caller save registers"

    munchStm :: Stm -> Codegen ()
    munchStm (SEQ(s1, s2)) = error "Compiler error: ITree is not canonicalized."
    munchStm (MOVE(TEMP t, CALL(NAME f, args))) =
      do saveCallerSaves
         genCall f args False
         emit $ MOV (MOVRR (named EAX) t) (named EAX) t
         restoreCallerSaves

    munchStm (MOVE(TEMP t, e)) =
      do src <- munchExp e
         emit $ MOV (MOVRR src t) src t

    munchStm (MOVE(MEM(TEMP tmp, sz), e)) =
      do src <- munchExp e
         emit $ OPER (MOVRM src $ mkaddr tmp 0) [tmp, src] [] Nothing

    munchStm (MOVE(MEM(BINOP(PLUS, e1, CONST i), sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM src2 $ mkaddr src1 i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(BINOP(PLUS, CONST i, e1), sz), e2)) =
      munchStm (MOVE(MEM(BINOP(PLUS, e1, CONST i), sz), e2))

    munchStm (MOVE(MEM(BINOP(MINUS, e1, CONST i), sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM src2 $ mkaddr src1 $ -i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(BINOP(MINUS, CONST i, e1), sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (NEGR src1) [src1] [] Nothing
         emit $ OPER (MOVRM src2 $ mkaddr src1 i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(e1, sz1), MEM(e2, sz2))) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         t <- newTemp
         emit $ OPER (MOVMR (mkaddr src2 0) t) [src2] [t] Nothing
         emit $ OPER (MOVRM t (mkaddr src1 0)) [src1, t] [] Nothing

    munchStm (MOVE(MEM(e1, sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM src2 (mkaddr src1 0)) [src1, src2] [] Nothing

    munchStm (MOVE(e1, e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRR src2 src1) [src2] [src1] Nothing

    munchStm (JUMP(NAME lab, lablist)) =
      do emit $ OPER (JMP $ TGSLT.name lab) [] [] (Just $ map TGSLT.name lablist)

    munchStm (CJUMP(TEST(relop, e1, e2), lab1, lab2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         let jmp = op2jmp relop
         emit $ OPER (CMPRR src1 src2) [src1, src2] [] Nothing
         emit $ OPER (jmp $ TGSLT.name lab1) [] [] (Just $ map TGSLT.name [lab1, lab2])

    munchStm (TigerITree.LABEL lab) =
      emit $ TigerAssem.LABEL (TGSLT.name lab)

    munchStm (EXP(CALL(NAME f, args))) =
      genCall f args True

    munchStm (EXP(e)) =
      munchExp e >> return ()

    munchExp :: Exp -> Codegen Tmp.Temp
    munchExp (ESEQ(s, e)) = error "Compiler error: ITree is not canonicalized."

    munchExp (TEMP t) = return t

    munchExp (MEM(CONST i, sz)) =
      do r <- newTemp
         emit $ OPER (MOVMR (mkaddr (named ZERO) i) r) [] [r] Nothing
         return r

    munchExp (MEM(BINOP(PLUS, e, CONST i), sz)) =
      do t <- munchExp e
         r <- newTemp
         emit $ OPER (MOVMR (mkaddr t i) r) [t] [r] Nothing
         return r

    munchExp (MEM(BINOP(PLUS, CONST i, e), sz)) =
      munchExp (MEM(BINOP(PLUS, e, CONST i), sz))

    munchExp (MEM(BINOP(MINUS, e, CONST i), sz)) =
      do t <- munchExp e
         r <- newTemp
         emit $ OPER (MOVMR (mkaddr t (-i)) r) [t] [r] Nothing
         return r

    munchExp (MEM(e, sz)) =
      do t <- munchExp e
         r <- newTemp
         emit $ OPER (MOVMR (mkaddr t 0) r) [t] [r] Nothing
         return r

    munchExp (BINOP(MUL, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         r <- newTemp
         emit $ OPER (MOVRR t1 r) [t1] [r] Nothing
         emit $ OPER (IMULRR t2 r) [t2, r] [r, named EDX] Nothing
         return r

    munchExp (BINOP(DIV, e1, e2)) =
      do r <- newTemp

         t1 <- munchExp e1
         emit $ OPER (MOVRR t1 (named EAX)) [t1] [named EAX] Nothing

         emit $ OPER (PUSH $ named EAX) (map named [ESP, EAX]) [named ESP] Nothing

         t2 <- munchExp e2
         emit $ OPER (MOVRR t2 r) [t2] [r] Nothing

         emit $ OPER (POP $ named EAX) [named ESP] [named EAX, named ESP] Nothing
         emit $ OPER CDQ [] [] Nothing
         emit $ OPER (IDIVR r) [r, named EAX, named EDX] (map named [EAX, EDX]) Nothing
         emit $ OPER (MOVRR (named EAX) r) [named EAX] [r] Nothing
         return r

    munchExp (BINOP(PLUS, CONST i, e)) =
      do t <- munchExp e
         emit $ OPER (ADDCR i t) [t] [t] Nothing
         return t

    munchExp (BINOP(PLUS, e, CONST i)) =
      munchExp (BINOP(PLUS, CONST i, e))

    munchExp (BINOP(PLUS, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ADDRR t1 t2) [t1, t2] [t2] Nothing
         return t2

    munchExp (BINOP(MINUS, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (SUBRR t2 t1) [t1, t2] [t1] Nothing
         return t1

    munchExp (BINOP(AND, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ANDRR t1 t2) [t1, t2] [t2] Nothing
         return t2

    munchExp (BINOP(OR, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ORRR t1 t2) [t1, t2] [t2] Nothing
         return t2

    munchExp (CONST i) =
      do r <- newTemp
         emit $ OPER (MOVCR i r) [] [r] Nothing
         return r

    munchExp (NAME lab) =
      do r <- newTemp
         emit $ OPER (LEAL (TGSLT.name lab) r) [] [r] Nothing
         return r

    munchExp e = error $ "Compiler error: Impossible pattern: " ++ show e ++ "."

    munchArg :: Exp -> Codegen ()
    munchArg (CONST i) =
      emit $ OPER (PUSHC i) [named ESP] [named ESP] Nothing

    munchArg (MEM(BINOP(PLUS, e, CONST i), sz)) =
      do t <- munchExp e
         emit $ OPER (PUSHM (mkaddr t i)) [t, named ESP] [named ESP] Nothing

    munchArg (MEM(BINOP(PLUS, CONST i, e), sz)) =
      munchArg (MEM(BINOP(PLUS, e, CONST i), sz))

    munchArg (MEM(BINOP(MINUS, e, CONST i), sz)) =
      do t <- munchExp e
         emit $ OPER (PUSHM (mkaddr t (-i))) [t, named ESP] [named ESP] Nothing

    munchArg (MEM(e, sz)) =
      do t <- munchExp e
         emit $ OPER (PUSHM $ mkaddr t 0) [t, named ESP] [named ESP] Nothing

    munchArg e =
      do t <- munchExp e
         emit $ OPER (PUSH t) [t, named ESP] [named ESP] Nothing
      
  in
    munchStm s0
