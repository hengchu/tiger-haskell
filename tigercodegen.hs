module TigerCodeGen
  (
    codegen
  , stringdata
  , procEntryExit
  )
  where

import TigerAssem
import qualified TigerTemp as Tmp
import TigerFrame
import TigerRegisters
import TigerITree
import qualified TigerGenSymLabTmp as TGSLT
import Prelude hiding (EQ, LT, GT)
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IORef


named :: Register -> Tmp.Temp
named = Tmp.Named

codegen :: Stm -> TGSLT.GenSymLabTmpState -> ([Instr], TGSLT.GenSymLabTmpState)
codegen stm st = let monad = codegen' stm
                     gsltmonad = execStateT monad []
                     (instrs, st') = (runIdentity . TGSLT.runGSLT st) gsltmonad
                 in  (reverse instrs, st')

stringdata :: Tmp.Label -> String -> [Instr]
stringdata lab str = [
                       DIRECTIVE ".data"
                     , TigerAssem.LABEL (TGSLT.name lab)
                     , DIRECTIVE $ ".4byte " ++ (show $ length str)
                     , DIRECTIVE $ ".string \"" ++ str ++ "\""
                     ]

-- Codegen Monad
type Codegen = StateT [Instr] (TGSLT.GenSymLabTmp Identity)

newTemp :: Codegen Tmp.Temp
newTemp = lift $ TGSLT.newTemp

emit :: Instr -> Codegen ()
emit instr = do instrs <- get
                put $ instr:instrs

comment :: String -> Instr
comment str = CMT $ COMMENT str

codegen' :: Stm -> Codegen ()
codegen' s0 =
  let
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
      do let calldefs = map Tmp.Named [EAX, ECX, EDX]
         when (shouldsavecaller) saveCallerSaves
         emit $ comment "Pushing arguments on stack in reverse order"
         mapM_ munchArg $ reverse args
         emit $ comment "Done pushing arguments"
         emit $ OPER (CALLL $ TGSLT.name f) [named ESP] (calldefs ++ map named [ESP, EBP])
                Nothing
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
         emit $ MOV (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) src t

    munchStm (MOVE(MEM(TEMP tmp, sz), e)) =
      do src <- munchExp e
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) 0) [tmp, src] [] Nothing

    munchStm (MOVE(MEM(BINOP(PLUS, e1, CONST i), sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(BINOP(PLUS, CONST i, e1), sz), e2)) =
      munchStm (MOVE(MEM(BINOP(PLUS, e1, CONST i), sz), e2))

    munchStm (MOVE(MEM(BINOP(MINUS, e1, CONST i), sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) $ -i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(BINOP(MINUS, CONST i, e1), sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (NEGR (Tmp.SRC 0)) [src1] [] Nothing
         emit $ OPER (MOVRM (Tmp.SRC 1) $ mkaddr (Tmp.SRC 0) i) [src1, src2] [] Nothing

    munchStm (MOVE(MEM(e1, sz1), MEM(e2, sz2))) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         t <- newTemp
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) 0) (Tmp.DST 0)) [src2] [t] Nothing
         emit $ OPER (MOVRM (Tmp.SRC 1) (mkaddr (Tmp.SRC 0) 0)) [src1, t] [] Nothing

    munchStm (MOVE(MEM(e1, sz), e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRM (Tmp.SRC 1) (mkaddr (Tmp.SRC 0) 0)) [src1, src2] [] Nothing

    munchStm (MOVE(e1, e2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         emit $ OPER (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) [src2] [src1] Nothing

    munchStm (JUMP(NAME lab, lablist)) =
      do emit $ OPER (JMP $ TGSLT.name lab) [] [] (Just $ map TGSLT.name lablist)

    munchStm (CJUMP(TEST(relop, e1, e2), lab1, lab2)) =
      do src1 <- munchExp e1
         src2 <- munchExp e2
         let jmp = op2jmp relop
         emit $ OPER (CMPRR (Tmp.SRC 0) (Tmp.SRC 1)) [src1, src2] [] Nothing
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
         emit $ OPER (MOVMR (mkaddr (named ZERO) i) (Tmp.DST 0)) [] [r] Nothing
         return r

    munchExp (MEM(BINOP(PLUS, e, CONST i), sz)) =
      do t <- munchExp e
         r <- newTemp
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) i) (Tmp.DST 0)) [t] [r] Nothing
         return r

    munchExp (MEM(BINOP(PLUS, CONST i, e), sz)) =
      munchExp (MEM(BINOP(PLUS, e, CONST i), sz))

    munchExp (MEM(BINOP(MINUS, e, CONST i), sz)) =
      do t <- munchExp e
         r <- newTemp
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) (-i)) (Tmp.DST 0)) [t] [r] Nothing
         return r

    munchExp (MEM(e, sz)) =
      do t <- munchExp e
         r <- newTemp
         emit $ OPER (MOVMR (mkaddr (Tmp.SRC 0) 0) (Tmp.DST 0)) [t] [r] Nothing
         return r

    munchExp (BINOP(MUL, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         r <- newTemp
         emit $ OPER (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) [t1] [r] Nothing
         emit $ OPER (IMULRR (Tmp.SRC 0) (Tmp.DST 0)) [t2, r] [r, named EDX] Nothing
         return r

    munchExp (BINOP(DIV, e1, e2)) =
      do r <- newTemp

         t1 <- munchExp e1
         emit $ OPER (MOVRR (Tmp.SRC 0) (named EAX)) [t1] [named EAX] Nothing

         emit $ OPER (PUSH $ named EAX) (map named [ESP, EAX]) [named ESP] Nothing

         t2 <- munchExp e2
         emit $ OPER (MOVRR (Tmp.SRC 0) (Tmp.DST 0)) [t2] [r] Nothing

         emit $ OPER (POP $ named EAX) [named ESP] [named EAX, named ESP] Nothing
         emit $ OPER CDQ [] [] Nothing
         emit $ OPER (IDIVR (Tmp.SRC 0)) [r, named EAX, named EDX] (map named [EAX, EDX])
                     Nothing
         emit $ OPER (MOVRR (named EAX) (Tmp.DST 0)) [named EAX] [r] Nothing
         return r

    munchExp (BINOP(PLUS, CONST i, e)) =
      do t <- munchExp e
         r <- newTemp
         emit $ MOV (MOVRR t r) t r
         emit $ OPER (ADDCR i (Tmp.SRC 0)) [r] [r] Nothing
         return r

    munchExp (BINOP(PLUS, e, CONST i)) =
      munchExp (BINOP(PLUS, CONST i, e))

    munchExp (BINOP(PLUS, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ADDRR (Tmp.SRC 0) (Tmp.DST 0)) [t1, t2] [t2] Nothing
         return t2

    munchExp (BINOP(MINUS, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (SUBRR (Tmp.SRC 1) (Tmp.DST 0)) [t1, t2] [t1] Nothing
         return t1

    munchExp (BINOP(AND, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ANDRR (Tmp.SRC 0) (Tmp.DST 0)) [t1, t2] [t2] Nothing
         return t2

    munchExp (BINOP(OR, e1, e2)) =
      do t1 <- munchExp e1
         t2 <- munchExp e2
         emit $ OPER (ORRR (Tmp.SRC 0) (Tmp.DST 0)) [t1, t2] [t2] Nothing
         return t2

    munchExp (CONST i) =
      do r <- newTemp
         emit $ OPER (MOVCR i (Tmp.DST 0)) [] [r] Nothing
         return r

    munchExp (NAME lab) =
      do r <- newTemp
         emit $ OPER (LEAL (TGSLT.name lab) (Tmp.DST 0)) [] [r] Nothing
         return r

    munchExp e = error $ "Compiler error: Impossible pattern: " ++ show e ++ "."

    munchArg :: Exp -> Codegen ()
    munchArg (CONST i) =
      emit $ OPER (PUSHC i) [named ESP] [named ESP] Nothing

    munchArg (MEM(BINOP(PLUS, e, CONST i), sz)) =
      do t <- munchExp e
         emit $ OPER (PUSHM (mkaddr (Tmp.SRC 0) i)) [t, named ESP] [named ESP] Nothing

    munchArg (MEM(BINOP(PLUS, CONST i, e), sz)) =
      munchArg (MEM(BINOP(PLUS, e, CONST i), sz))

    munchArg (MEM(BINOP(MINUS, e, CONST i), sz)) =
      do t <- munchExp e
         emit $ OPER (PUSHM (mkaddr (Tmp.SRC 0) (-i))) [t, named ESP] [named ESP] Nothing

    munchArg (MEM(e, sz)) =
      do t <- munchExp e
         emit $ OPER (PUSHM $ mkaddr (Tmp.SRC 0) 0) [t, named ESP] [named ESP] Nothing

    munchArg e =
      do t <- munchExp e
         emit $ OPER (PUSH (Tmp.SRC 0)) [t, named ESP] [named ESP] Nothing
      
  in
    munchStm s0


procEntryExit :: Tmp.Label -> [(Instr, [Tmp.Temp])]
                           -> Map.Map Tmp.Temp Register
                           -> [Tmp.Temp]
                           -> Frame
                           -> IO [Instr]
procEntryExit name
              instrAndLiveTemps
              alloc
              formals
              frame =
  do let instrs = map fst instrAndLiveTemps
     prologue <- newIORef []
     epilogue <- newIORef []
     let emitPro i = do pros <- readIORef prologue
                        writeIORef prologue (i:pros)
     let emitEpi i = do epi <- readIORef epilogue
                        writeIORef epilogue (i:epi)
     let labname = TGSLT.name name
     numlocals <- readIORef $ frameLocalCount frame
     
     emitPro $ DIRECTIVE $ ".text\n.global "++labname
     emitPro $ TigerAssem.LABEL labname
     emitPro $ comment "prolog begins"
     emitPro $ OPER (PUSH $ Tmp.Named EBP) [Tmp.Named EBP] [] Nothing
     emitPro $ MOV (MOVRR (Tmp.Named ESP) (Tmp.Named EBP)) (Tmp.Named ESP) (Tmp.Named EBP)
     emitPro $ comment "allocating space for locals"
     emitPro $ OPER (SUBCR (numlocals*4+npseudoregs*4) (Tmp.Named ESP)) [Tmp.Named ESP] [] Nothing
     emitPro $ comment "saving callee saves"
     emitPro $ OPER (PUSH (Tmp.Named EBX)) [Tmp.Named EBX] [] Nothing
     emitPro $ OPER (PUSH (Tmp.Named EDI)) [Tmp.Named EDI] [] Nothing
     emitPro $ OPER (PUSH (Tmp.Named ESI)) [Tmp.Named ESI] [] Nothing
     emitPro $ comment "prologue ends here"

     emitEpi $ comment "epilogue begins here"
     emitEpi $ OPER (POP (Tmp.Named ESI)) [] [Tmp.Named ESI] Nothing
     emitEpi $ OPER (POP (Tmp.Named EDI)) [] [Tmp.Named EDI] Nothing
     emitEpi $ OPER (POP (Tmp.Named EBX)) [] [Tmp.Named EBX] Nothing
     emitEpi $ MOV (MOVRR (Tmp.Named EBP) (Tmp.Named ESP)) (Tmp.Named EBP) (Tmp.Named ESP)
     emitEpi $ OPER (POP (Tmp.Named EBP)) [] [Tmp.Named EBP] Nothing
     emitEpi $ OPER RET [] [] Nothing

     let spilledInstrs = concatMap (genSpill alloc) instrs
     proInstrs <- readIORef prologue
     epiInstrs <- readIORef epilogue
     return $ (reverse proInstrs) ++ spilledInstrs ++ (reverse epiInstrs)


pseudoreg2addr :: Register -> Addr
pseudoreg2addr (PSEUDO d) = mkaddr (Tmp.Named EBP) $ (-4)*(d+1)
pseudoreg2addr _ = error "Compiler error: pseudoreg2addr called with machine register."

genSpill :: Map.Map Tmp.Temp Register -> Instr -> [Instr]
genSpill alloc instr =
  let

    availmregs = [ECX, EDX]

    ispseudo (PSEUDO _) = True
    ispseudo _ = False

    loadfreg src@(PSEUDO _) mreg =
      ([MOVMR (pseudoreg2addr src) (Tmp.Named mreg)], mreg)
    loadfreg src _ = ([], src)

    mkmov p@(PSEUDO _) r = MOVMR (pseudoreg2addr p) (Tmp.Named r)
    mkmov r p@(PSEUDO _) = MOVRM (Tmp.Named r) (pseudoreg2addr p)
    mkmov r1 r2 = MOVRR (Tmp.Named r1) (Tmp.Named r2)

    mapsrcs [] _ = ([], [])
    mapsrcs (src:srcs) (mreg:mregs) =
      let (loadinstrs, src') = loadfreg src mreg
          (loadrest, srcs') = mapsrcs srcs mregs
      in (loadinstrs++loadrest, src':srcs')
    mapsrcs srcs [] =
      if any ispseudo srcs
        then error "Compiler error: not enough machine register to map pseudo sources."
        else ([], srcs)

    stripnamed (Tmp.Named r) = r
    stripnamed t = case Map.lookup t alloc of
                     Just r -> r
                     Nothing -> error $ "Compiler error: genSpill encountered non-colored temp: "++show t++"."

    mapdsts [] _ _ = ([], [])
    mapdsts ds@(dst:dsts) srcs newsrcs =
      if any ispseudo ds
        then let found = List.findIndex (dst==) srcs
             in  case found of
                   Just idx -> let src = srcs !! idx
                                   mreg = newsrcs !! idx
                               in  if src /= mreg
                                      then ([mkmov mreg dst], mreg:dsts)
                                      else ([], dst:dsts)
                   Nothing -> case dst of
                                PSEUDO _ -> let mreg = head availmregs
                                            in  ([mkmov mreg dst], mreg:dsts)
                                _ -> ([], dst:dsts)
        else ([], dst:dsts)

    mksimpleop i = OPER i [] [] Nothing

  in case instr of
       OPER i srcs dsts jmp ->
         let (loadinstrs, newsrcs) = mapsrcs (map stripnamed srcs) availmregs
             (storeinstrs, newdsts) = mapdsts (map stripnamed dsts) (map stripnamed srcs) newsrcs
         in map mksimpleop loadinstrs ++ [OPER i (map Tmp.Named newsrcs) (map Tmp.Named newdsts) jmp] ++ map mksimpleop storeinstrs
       MOV i src dst ->
          let (loadinstrs, newsrcs) = mapsrcs [stripnamed src] availmregs
              (storeinstrs, newdsts) = mapdsts [stripnamed dst] [stripnamed src] newsrcs
          in map mksimpleop loadinstrs ++ [OPER i (map Tmp.Named newsrcs) (map Tmp.Named newdsts) Nothing] ++ map mksimpleop storeinstrs
       TigerAssem.LABEL _ -> [instr]
       CMT _ -> [instr]
       DIRECTIVE _ -> [instr]
