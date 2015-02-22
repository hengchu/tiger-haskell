module TigerTranslate2
  (
    Level
  , Access
  , Frag
  , Gexp
  , newLevel
  , outerMost
  , allocInFrame
  , eqStr
  , notEqStr
  , strLessThan
  , strLessThanOrEq
  , eqCmp
  , notEqCmp
  , lessThan
  , lessThanOrEq
  , arithmetic
  , intExp
  , stringExp
  , constructEseq
  , letExpression
  , assign
  , createRecord
  , createArray
  , field
  , subscript
  , simpleVar
  , ifThen
  , ifThenElse
  , whileLoop
  , forLoop
  , TigerTranslate2.break
  , callFunction
  , createProcFrag
  , createMainFrag
  , reset
  , getResult
  , nilGexp
  ) where

import qualified TigerFrame2 as Frame
import qualified TigerTemp2 as Tmp
import qualified TigerAbsyn as Absyn
import qualified TigerRegisters as Reg
import Control.Monad.IO.Class
import FrontEnd

data Gexp = Ex Exp
          | Nx Stm
          | Cx (Label -> Label -> Stm)
             -- True label   False label

outerMost :: Level
outerMost = TOP

nilGexp :: Gexp
nilGexp = Ex $ CONST(0)

newLevel :: Level -> [a] -> Frontend (Level, [(a, Access)])
newLevel parent formals =
  do let numformals = length formals
     (frame, (slOffset:offsets)) <- liftIO $ Frame.newFrame (numformals+1)
     lvlUniq <- genUniq
     let lvl = LEVEL { levelFrame=frame
                     , staticLinkOffset=slOffset
                     , levelParent=parent
                     , levelUniq=lvlUniq }
     let offsets' = zip (replicate numformals lvl) offsets
     let formalsAndOffsets = zip formals offsets'
     return (lvl, formalsAndOffsets)

allocInFrame :: Level -> IO Access
allocInFrame lvl@(LEVEL { levelFrame=lvlframe }) =
  do offset <- Frame.allocLocalInFrame lvlframe
     return (lvl, offset) 
allocInFrame TOP = error "Compiler error: cannot alloc local in TOP Level frame"

seqcon :: [Stm] -> Stm
seqcon (x:[]) = x
seqcon (x:xs) = SEQ(x, seqcon xs)
seqcon []     = error "Compiler error: Impossible usage of seqcon"

unEx :: Gexp -> Frontend (Exp)
unEx (Ex e)      = return e
unEx (Cx genstm) = do r <- Tmp.newTemp
                      t <- Tmp.newLabel
                      f <- Tmp.newLabel
                      return $ ESEQ(seqcon
                                       [ MOVE(TEMP r, CONST 1)
                                       , genstm t f
                                       , LABEL f
                                       , MOVE(TEMP r, CONST 0)
                                       , LABEL t
                                       ], TEMP r)
unEx (Nx s)      = return $ ESEQ(s, CONST 0)

unCx :: Gexp -> Frontend (Tmp.Label -> Tmp.Label -> Stm)
unCx (Ex (CONST(1))) = return $ (\t -> \_ -> JUMP(NAME(t), [t]))
unCx (Ex (CONST(0))) = return $ (\_ -> \f -> JUMP(NAME(f), [f]))
unCx (Ex e) = return $ (\t -> \f -> CJUMP(TEST(NE, e, CONST 0), t, f))
unCx (Cx genstm) = return genstm
unCx (Nx _) = error "Compiler error: Impossible usage of unCx"

unNx :: Gexp -> Frontend (Stm)
unNx (Ex e) = return $ EXP e
unNx (Nx stm) = return stm
unNx c = do e <- unEx c
            unNx $ Ex e



-- Cx-constructed expression comparing two strings for equality
eqStr :: Gexp -> Gexp -> Frontend Gexp
eqStr str1 str2 = 
  do str1' <- unEx str1
     str2' <- unEx str2
     funclabel <- Tmp.namedLabel "stringEqual"
     return $ Ex $ CALL (NAME funclabel,
                            [str1', str2'])

notEqStr :: Gexp -> Gexp -> Frontend Gexp
notEqStr str1 str2 = 
  do (Ex eqstr) <- eqStr str1 str2
     return $ Cx $ \t -> 
                   \f -> CJUMP(TEST (FrontEnd.EQ, eqstr, CONST(0)), t, f)
                        

strLessThan :: Gexp -> Gexp -> Frontend Gexp
strLessThan str1 str2 = 
  do str1' <- unEx str1
     str2' <- unEx str2
     funclabel <- Tmp.namedLabel "stringLessThan"
     return $ Ex $ CALL (NAME funclabel,
                            [str1', str2'])

strLessThanOrEq :: Gexp -> Gexp -> Frontend Gexp
strLessThanOrEq str1 str2 = 
  do lab <- Tmp.newLabel
     Ex eq <- eqStr str1 str2
     Ex lt <- strLessThan str1 str2
     return $ Cx $ \t ->
                   \f -> seqcon [ CJUMP (TEST (FrontEnd.EQ, eq, CONST 0), lab, t)
                                , LABEL lab
                                , CJUMP (TEST (FrontEnd.EQ, lt, CONST 0), f, t)]
                               


-- Comparing non-string values
eqCmp :: Gexp -> Gexp -> Frontend Gexp
eqCmp g1 g2 = 
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (FrontEnd.EQ, g1', g2'), t, f)
 
notEqCmp :: Gexp -> Gexp -> Frontend Gexp
notEqCmp g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (NE, g1', g2'), t, f)

lessThan :: Gexp -> Gexp -> Frontend Gexp
lessThan g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (FrontEnd.LT, g1', g2'), t, f)

lessThanOrEq :: Gexp -> Gexp -> Frontend Gexp
lessThanOrEq g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> CJUMP(TEST (LE, g1', g2'), t, f)

-- Arithmetic
arithmetic :: Absyn.Oper -> Gexp -> Gexp -> Frontend Gexp
arithmetic op g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Ex $ BINOP (transop op, g1', g2')
  where transop Absyn.PlusOp   = PLUS
        transop Absyn.MinusOp  = MINUS
        transop Absyn.TimesOp  = MUL
        transop Absyn.DivideOp = DIV
        transop Absyn.AndOp    = AND
        transop Absyn.OrOp     = OR
        transop o              = error $ "Compiler error : " ++ show o ++ "not implemented yet"

-- Literal
intExp :: Int -> Frontend Gexp
intExp val = return $ Ex $ CONST val

stringExp :: String -> Frontend Gexp
stringExp str =
  do lab <- Tmp.newLabel
     let frag = Frame.DATA lab str
     (v, t, l, frags) <- semantStGet
     semantStPut (v, t, l, frag:frags)
     return $ Ex $ NAME lab


-- Helper sequence function
constructEseq :: Gexp -> Gexp -> Frontend Gexp
constructEseq stm e =
  do stm' <- unNx stm
     exp' <- unEx e
     return $ Ex $ ESEQ (stm', exp')

letExpression :: [Gexp] -> Gexp -> Frontend Gexp
letExpression []   body = return body
letExpression decs body =
  do decs' <- mapM unNx decs
     body' <- unEx body
     return $ Ex $ ESEQ (seqcon decs', body')

-- Assignment
assign :: Gexp -> Gexp -> Frontend Gexp
assign var assgnval =
  do var' <- unEx var
     assgnval' <- unEx assgnval
     return $ Nx $ MOVE (var', assgnval')

-- Record and Array creation
createRecord :: [Gexp] -> Frontend Gexp
createRecord fieldvars =
  do address <- Tmp.newTemp
     allocfunlab <- Tmp.namedLabel "allocRecord"
     let alloc = MOVE(TEMP address, CALL (NAME allocfunlab, [CONST $ 4 * length fieldvars]))
     let idxs  = [1..length fieldvars]
     instrs <- mapM (uncurry $ initfield address) $ zip fieldvars idxs
     return $ Ex $ ESEQ(seqcon $ alloc:instrs, TEMP address)
  where initfield address fieldvar idx = do fieldvar' <- unEx fieldvar
                                            let baseaddr = TEMP address
                                            let addr = MEM (BINOP(PLUS, baseaddr, CONST $ idx * 4), 4)
                                            return $ MOVE (addr, fieldvar')

createArray :: Gexp -> Gexp -> Frontend Gexp
createArray sizexp initexp =
  do sizexp'  <- unEx sizexp
     initexp' <- unEx initexp
     allocarrfun <- Tmp.namedLabel "allocArray"
     return $ Ex $ CALL (NAME allocarrfun, [sizexp', initexp'])

-- Variable access
field :: Gexp -> Int -> Frontend Gexp
field recordge fieldnum =
  do fieldfunlab <- Tmp.namedLabel "field"
     recordge'   <- unEx recordge
     return $ Ex $ CALL (NAME fieldfunlab, [recordge', CONST fieldnum])

subscript :: Gexp -> Gexp -> Frontend Gexp
subscript arrge idxge =
  do arrge' <- unEx arrge
     idxge' <- unEx idxge
     subscriptfunlab <- Tmp.namedLabel "subscript"
     return $ Ex $ CALL (NAME subscriptfunlab, [arrge', idxge'])

simpleVar :: Access -> Level -> Frontend Gexp
simpleVar (varlevel, offset) fromLevel =
  return $ Ex $ accessFrameOff offset (frameAtLevel varlevel fromLevel $ TEMP $ Tmp.Named Reg.EBP)

frameAtLevel :: Level -> Level -> Exp -> Exp
frameAtLevel destlvl startlvl startlvlptr =
  if destlvl == startlvl
     then startlvlptr
     else case startlvl of
            TOP -> error "Functions from TOP level should not access static links"
            LEVEL{staticLinkOffset=offset, levelParent=parent} -> frameAtLevel destlvl parent $ accessFrameOff offset startlvlptr

accessFrameOff :: Int -> Exp -> Exp
accessFrameOff offset frameptr = MEM(BINOP (PLUS, frameptr, CONST offset), 4)

-- Conditional and loops
ifThen :: Gexp -> Gexp -> Frontend Gexp
ifThen testge thenge =
  do testge' <- unCx testge
     thenge' <- unNx thenge
     t <- Tmp.newLabel
     f <- Tmp.newLabel
     return $ Nx $ seqcon [(testge' t f), LABEL t, thenge', LABEL f]

ifThenElse :: Gexp -> Gexp -> Gexp -> Frontend Gexp
ifThenElse testge (Nx thenstm) (Nx elsestm) =
  do testge' <- unCx testge
     t <- Tmp.newLabel
     f <- Tmp.newLabel
     return $ Nx $ seqcon [testge' t f, LABEL t, thenstm, LABEL f, elsestm]
ifThenElse testge thenge elsege =
  do testge' <- unCx testge
     t <- Tmp.newLabel
     f <- Tmp.newLabel
     j <- Tmp.newLabel
     r <- Tmp.newTemp
     thenge' <- unEx thenge
     elsege' <- unEx elsege
     return $ Ex $ ESEQ (
                             seqcon [
                               testge' t f
                             , LABEL t
                             , MOVE (TEMP r, thenge')
                             , JUMP (NAME j, [j])
                             , LABEL f
                             , MOVE (TEMP r, elsege')
                             , LABEL j
                             ]
                           , TEMP r)

whileLoop :: Gexp -> Gexp -> Tmp.Label -> Frontend Gexp
whileLoop testge bodyge donelab =
  do testge' <- unCx testge
     bodyge' <- unNx bodyge
     testlab <- Tmp.newLabel
     bodylab <- Tmp.newLabel
     return $ Nx $ seqcon [ LABEL testlab
                          , testge' bodylab donelab
                          , LABEL bodylab
                          , bodyge'
                          , JUMP (NAME testlab, [testlab])
                          , LABEL donelab ]

forLoop :: Gexp -> Gexp -> Gexp -> Tmp.Label -> Gexp -> Frontend Gexp
forLoop loge hige bodyge donelab iteratorge =
  do loge' <- unEx loge
     hige' <- unEx hige
     bodyge' <- unNx bodyge
     iteratorge' <- unEx iteratorge
     limit <- Tmp.newTemp
     bodylab <- Tmp.newLabel
     inclab  <- Tmp.newLabel
     return $ Nx $ seqcon [ MOVE(iteratorge', loge')
                          , MOVE(TEMP limit, hige')
                          , CJUMP (TEST (LE, iteratorge', TEMP limit), bodylab, donelab)
                          , LABEL bodylab
                          , bodyge'
                          , CJUMP (TEST(FrontEnd.LT, iteratorge', TEMP limit), inclab, donelab)
                          , LABEL inclab
                          , MOVE(iteratorge', BINOP(PLUS, iteratorge', CONST 1))
                          , JUMP(NAME bodylab, [bodylab])
                          , LABEL donelab
                          ]

break :: Tmp.Label -> Frontend Gexp
break lab =
  return $ Nx $ JUMP(NAME lab, [lab])

callFunction :: Tmp.Label -> Level -> Level -> [Gexp] -> Frontend Gexp
callFunction funclab callerlvl calleelvl argsge =
  do argsge' <- mapM unEx argsge
     if calleelvl == TOP
        then return $ Ex $ CALL (NAME funclab, argsge')
        else do let calleeparent = levelParent calleelvl
                let staticlinkexp = frameAtLevel calleeparent callerlvl $ TEMP $ Tmp.Named $ Reg.EBP
                return $ Ex $ CALL (NAME funclab, staticlinkexp:argsge')

wrapFuncBody :: Stm -> Frontend Stm
wrapFuncBody (EXP bodyexp) =
  do temp <- Tmp.newTemp
     return $ seqcon [ MOVE (TEMP temp, bodyexp)
                     , MOVE (TEMP $ Tmp.Named $ Reg.EAX, TEMP temp) ]
wrapFuncBody body = return body

createProcFrag :: Tmp.Label -> Level -> Gexp -> Frontend ()
createProcFrag proclab level bodyge =
  do bodyge' <- unNx bodyge
     wrappedbody <- wrapFuncBody bodyge'
     let procfrag = Frame.PROC { Frame.procName  = proclab
                               , Frame.procBody  = wrappedbody
                               , Frame.procFrame = levelFrame level }
     (v, t, l, frags) <- semantStGet
     semantStPut (v, t, l, procfrag:frags)

createMainFrag :: Level -> Gexp -> Frontend ()
createMainFrag lvl bodyge = 
  do mainlab <- Tmp.namedLabel "tigermain"
     createProcFrag mainlab lvl bodyge

reset :: Frontend ()
reset = do (v, t, l, _) <- semantStGet
           semantStPut (v, t, l, []) 

getResult :: Frontend [Frag]
getResult = do (_, _, _, frags) <- semantStGet
               return frags
