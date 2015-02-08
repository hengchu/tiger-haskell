module TigerTranslate
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
  , TigerTranslate.break
  , callFunction
  , createProcFrag
  , createMainFrag
  , reset
  , getResult
  , nilGexp
  ) where

import qualified TigerFrame as Frame
import qualified TigerITree as Tr
import qualified TigerTemp as Tmp
import qualified TigerAbsyn as Absyn
import qualified TigerRegisters as Reg
import Data.IORef
import Control.Monad ()
import System.IO.Unsafe


type Uniq = Integer

uniqCounter :: IORef Integer
uniqCounter = unsafePerformIO $ newIORef 0

genUniq :: IO Integer
genUniq = do c <- readIORef uniqCounter 
             writeIORef uniqCounter (c+1)
             return c

data Level = LEVEL { levelFrame :: Frame.Frame
                   , staticLinkOffset :: Int
                   , levelParent :: Level
                   , levelUniq :: Uniq }
           | TOP
           deriving (Eq)

type Access = (Level, Int)

type Frag = Frame.Frag

data Gexp = Ex Tr.Exp
          | Nx Tr.Stm
          | Cx (Tmp.Label -> Tmp.Label -> Tr.Stm)
             -- True label   False label

outerMost :: Level
outerMost = TOP

nilGexp :: Gexp
nilGexp = Ex $ Tr.CONST(0)

newLevel :: Level -> [a] -> IO (Level, [(a, Access)])
newLevel parent formals =
  do let numformals = length formals
     (frame, (slOffset:offsets)) <- Frame.newFrame (numformals+1)
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

seqcon :: [Tr.Stm] -> Tr.Stm
seqcon (x:[]) = x
seqcon (x:xs) = Tr.SEQ(x, seqcon xs)
seqcon []     = error "Compiler error: Impossible usage of seqcon"

unEx :: Gexp -> IO (Tr.Exp)
unEx (Ex e)      = return e
unEx (Cx genstm) = do r <- Tmp.newTemp
                      t <- Tmp.newLabel
                      f <- Tmp.newLabel
                      return $ Tr.ESEQ(seqcon
                                       [ Tr.MOVE(Tr.TEMP r, Tr.CONST 1)
                                       , genstm t f
                                       , Tr.LABEL f
                                       , Tr.MOVE(Tr.TEMP r, Tr.CONST 0)
                                       , Tr.LABEL t
                                       ], Tr.TEMP r)
unEx (Nx s)      = return $ Tr.ESEQ(s, Tr.CONST 0)

unCx :: Gexp -> IO (Tmp.Label -> Tmp.Label -> Tr.Stm)
unCx (Ex (Tr.CONST(1))) = return $ (\t -> \_ -> Tr.JUMP(Tr.NAME(t), [t]))
unCx (Ex (Tr.CONST(0))) = return $ (\_ -> \f -> Tr.JUMP(Tr.NAME(f), [f]))
unCx (Ex e) = return $ (\t -> \f -> Tr.CJUMP(Tr.TEST(Tr.NE, e, Tr.CONST 0), t, f))
unCx (Cx genstm) = return genstm
unCx (Nx _) = error "Compiler error: Impossible usage of unCx"

unNx :: Gexp -> IO (Tr.Stm)
unNx (Ex e) = return $ Tr.EXP e
unNx (Nx stm) = return stm
unNx c = do e <- unEx c
            unNx $ Ex e



-- Cx-constructed expression comparing two strings for equality
eqStr :: Gexp -> Gexp -> IO Gexp
eqStr str1 str2 = 
  do str1' <- unEx str1
     str2' <- unEx str2
     funclabel <- Tmp.namedLabel "stringEqual"
     return $ Ex $ Tr.CALL (Tr.NAME funclabel,
                            [str1', str2'])

notEqStr :: Gexp -> Gexp -> IO Gexp
notEqStr str1 str2 = 
  do (Ex eqstr) <- eqStr str1 str2
     return $ Cx $ \t -> 
                   \f -> Tr.CJUMP(Tr.TEST (Tr.EQ, eqstr, Tr.CONST(0)), t, f)
                        

strLessThan :: Gexp -> Gexp -> IO Gexp
strLessThan str1 str2 = 
  do str1' <- unEx str1
     str2' <- unEx str2
     funclabel <- Tmp.namedLabel "stringLessThan"
     return $ Ex $ Tr.CALL (Tr.NAME funclabel,
                            [str1', str2'])

strLessThanOrEq :: Gexp -> Gexp -> IO Gexp
strLessThanOrEq str1 str2 = 
  do lab <- Tmp.newLabel
     Ex eq <- eqStr str1 str2
     Ex lt <- strLessThan str1 str2
     return $ Cx $ \t ->
                   \f -> seqcon [ Tr.CJUMP (Tr.TEST (Tr.EQ, eq, Tr.CONST 0), lab, t)
                                , Tr.LABEL lab
                                , Tr.CJUMP (Tr.TEST (Tr.EQ, lt, Tr.CONST 0), f, t)]
                               


-- Comparing non-string values
eqCmp :: Gexp -> Gexp -> IO Gexp
eqCmp g1 g2 = 
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> Tr.CJUMP(Tr.TEST (Tr.EQ, g1', g2'), t, f)
 
notEqCmp :: Gexp -> Gexp -> IO Gexp
notEqCmp g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> Tr.CJUMP(Tr.TEST (Tr.NE, g1', g2'), t, f)

lessThan :: Gexp -> Gexp -> IO Gexp
lessThan g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> Tr.CJUMP(Tr.TEST (Tr.LT, g1', g2'), t, f)

lessThanOrEq :: Gexp -> Gexp -> IO Gexp
lessThanOrEq g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Cx $ \t ->
                   \f -> Tr.CJUMP(Tr.TEST (Tr.LE, g1', g2'), t, f)

-- Arithmetic
arithmetic :: Absyn.Oper -> Gexp -> Gexp -> IO Gexp
arithmetic op g1 g2 =
  do g1' <- unEx g1
     g2' <- unEx g2
     return $ Ex $ Tr.BINOP (transop op, g1', g2')
  where transop Absyn.PlusOp   = Tr.PLUS
        transop Absyn.MinusOp  = Tr.MINUS
        transop Absyn.TimesOp  = Tr.MUL
        transop Absyn.DivideOp = Tr.DIV
        transop Absyn.AndOp    = Tr.AND
        transop Absyn.OrOp     = Tr.OR
        transop o              = error $ "Compiler error : " ++ show o ++ "not implemented yet"

-- Literal
intExp :: Int -> IO Gexp
intExp val = return $ Ex $ Tr.CONST val

stringExp :: String -> IO Gexp
stringExp str =
  do lab <- Tmp.newLabel
     let frag = Frame.DATA lab str
     frags <- readIORef fragsRef
     writeIORef fragsRef (frag:frags)
     return $ Ex $ Tr.NAME lab


-- Helper sequence function
constructEseq :: Gexp -> Gexp -> IO Gexp
constructEseq stm e =
  do stm' <- unNx stm
     exp' <- unEx e
     return $ Ex $ Tr.ESEQ (stm', exp')

letExpression :: [Gexp] -> Gexp -> IO Gexp
letExpression []   body = return body
letExpression decs body =
  do decs' <- mapM unNx decs
     body' <- unEx body
     return $ Ex $ Tr.ESEQ (seqcon decs', body')

-- Assignment
assign :: Gexp -> Gexp -> IO Gexp
assign var assgnval =
  do var' <- unEx var
     assgnval' <- unEx assgnval
     return $ Nx $ Tr.MOVE (var', assgnval')

-- Record and Array creation
createRecord :: [Gexp] -> IO Gexp
createRecord fieldvars =
  do address <- Tmp.newTemp
     allocfunlab <- Tmp.namedLabel "allocRecord"
     let alloc = Tr.MOVE(Tr.TEMP address, Tr.CALL (Tr.NAME allocfunlab, [Tr.CONST $ 4 * length fieldvars]))
     let idxs  = [1..length fieldvars]
     instrs <- mapM (uncurry $ initfield address) $ zip fieldvars idxs
     return $ Ex $ Tr.ESEQ(seqcon $ alloc:instrs, Tr.TEMP address)
  where initfield address fieldvar idx = do fieldvar' <- unEx fieldvar
                                            let baseaddr = Tr.TEMP address
                                            let addr = Tr.MEM (Tr.BINOP(Tr.PLUS, baseaddr, Tr.CONST $ idx * 4), 4)
                                            return $ Tr.MOVE (addr, fieldvar')

createArray :: Gexp -> Gexp -> IO Gexp
createArray sizexp initexp =
  do sizexp'  <- unEx sizexp
     initexp' <- unEx initexp
     allocarrfun <- Tmp.namedLabel "allocArray"
     return $ Ex $ Tr.CALL (Tr.NAME allocarrfun, [sizexp', initexp'])

-- Variable access
field :: Gexp -> Int -> IO Gexp
field recordge fieldnum =
  do fieldfunlab <- Tmp.namedLabel "field"
     recordge'   <- unEx recordge
     return $ Ex $ Tr.CALL (Tr.NAME fieldfunlab, [recordge', Tr.CONST fieldnum])

subscript :: Gexp -> Gexp -> IO Gexp
subscript arrge idxge =
  do arrge' <- unEx arrge
     idxge' <- unEx idxge
     subscriptfunlab <- Tmp.namedLabel "subscript"
     return $ Ex $ Tr.CALL (Tr.NAME subscriptfunlab, [arrge', idxge'])

simpleVar :: Access -> Level -> IO Gexp
simpleVar (varlevel, offset) fromLevel =
  return $ Ex $ accessFrameOff offset (frameAtLevel varlevel fromLevel $ Tr.TEMP $ Tmp.Named Reg.EBP)

frameAtLevel :: Level -> Level -> Tr.Exp -> Tr.Exp
frameAtLevel destlvl startlvl startlvlptr =
  if destlvl == startlvl
     then startlvlptr
     else case startlvl of
            TOP -> error "Functions from TOP level should not access static links"
            LEVEL{staticLinkOffset=offset, levelParent=parent} -> frameAtLevel destlvl parent $ accessFrameOff offset startlvlptr

accessFrameOff :: Int -> Tr.Exp -> Tr.Exp
accessFrameOff offset frameptr = Tr.MEM(Tr.BINOP (Tr.PLUS, frameptr, Tr.CONST offset), 4)

-- Conditional and loops
ifThen :: Gexp -> Gexp -> IO Gexp
ifThen testge thenge =
  do testge' <- unCx testge
     thenge' <- unNx thenge
     t <- Tmp.newLabel
     f <- Tmp.newLabel
     return $ Nx $ seqcon [(testge' t f), Tr.LABEL t, thenge', Tr.LABEL f]

ifThenElse :: Gexp -> Gexp -> Gexp -> IO Gexp
ifThenElse testge (Nx thenstm) (Nx elsestm) =
  do testge' <- unCx testge
     t <- Tmp.newLabel
     f <- Tmp.newLabel
     return $ Nx $ seqcon [testge' t f, Tr.LABEL t, thenstm, Tr.LABEL f, elsestm]
ifThenElse testge thenge elsege =
  do testge' <- unCx testge
     t <- Tmp.newLabel
     f <- Tmp.newLabel
     j <- Tmp.newLabel
     r <- Tmp.newTemp
     thenge' <- unEx thenge
     elsege' <- unEx elsege
     return $ Ex $ Tr.ESEQ (
                             seqcon [
                               testge' t f
                             , Tr.LABEL t
                             , Tr.MOVE (Tr.TEMP r, thenge')
                             , Tr.JUMP (Tr.NAME j, [j])
                             , Tr.LABEL f
                             , Tr.MOVE (Tr.TEMP r, elsege')
                             , Tr.LABEL j
                             ]
                           , Tr.TEMP r)

whileLoop :: Gexp -> Gexp -> Tmp.Label -> IO Gexp
whileLoop testge bodyge donelab =
  do testge' <- unCx testge
     bodyge' <- unNx bodyge
     testlab <- Tmp.newLabel
     bodylab <- Tmp.newLabel
     return $ Nx $ seqcon [ Tr.LABEL testlab
                          , testge' bodylab donelab
                          , Tr.LABEL bodylab
                          , bodyge'
                          , Tr.JUMP (Tr.NAME testlab, [testlab])
                          , Tr.LABEL donelab ]

forLoop :: Gexp -> Gexp -> Gexp -> Tmp.Label -> Gexp -> IO Gexp
forLoop loge hige bodyge donelab iteratorge =
  do loge' <- unEx loge
     hige' <- unEx hige
     bodyge' <- unNx bodyge
     iteratorge' <- unEx iteratorge
     limit <- Tmp.newTemp
     bodylab <- Tmp.newLabel
     inclab  <- Tmp.newLabel
     return $ Nx $ seqcon [ Tr.MOVE(iteratorge', loge')
                          , Tr.MOVE(Tr.TEMP limit, hige')
                          , Tr.CJUMP (Tr.TEST (Tr.LE, iteratorge', Tr.TEMP limit), bodylab, donelab)
                          , Tr.LABEL bodylab
                          , bodyge'
                          , Tr.CJUMP (Tr.TEST(Tr.LT, iteratorge', Tr.TEMP limit), inclab, donelab)
                          , Tr.LABEL inclab
                          , Tr.MOVE(iteratorge', Tr.BINOP(Tr.PLUS, iteratorge', Tr.CONST 1))
                          , Tr.JUMP(Tr.NAME bodylab, [bodylab])
                          , Tr.LABEL donelab
                          ]

break :: Tmp.Label -> IO Gexp
break lab =
  return $ Nx $ Tr.JUMP(Tr.NAME lab, [lab])

callFunction :: Tmp.Label -> Level -> Level -> [Gexp] -> IO Gexp
callFunction funclab callerlvl calleelvl argsge =
  do argsge' <- mapM unEx argsge
     if calleelvl == TOP
        then return $ Ex $ Tr.CALL (Tr.NAME funclab, argsge')
        else do let calleeparent = levelParent calleelvl
                let staticlinkexp = frameAtLevel calleeparent callerlvl $ Tr.TEMP $ Tmp.Named $ Reg.EBP
                return $ Ex $ Tr.CALL (Tr.NAME funclab, staticlinkexp:argsge')

fragsRef ::IORef [Frag]
fragsRef = unsafePerformIO $ newIORef []

wrapFuncBody :: Tr.Stm -> IO Tr.Stm
wrapFuncBody (Tr.EXP bodyexp) =
  do temp <- Tmp.newTemp
     return $ seqcon [ Tr.MOVE (Tr.TEMP temp, bodyexp)
                     , Tr.MOVE (Tr.TEMP $ Tmp.Named $ Reg.EAX, Tr.TEMP temp) ]
wrapFuncBody body = return body

createProcFrag :: Tmp.Label -> Level -> Gexp -> IO ()
createProcFrag proclab level bodyge =
  do bodyge' <- unNx bodyge
     wrappedbody <- wrapFuncBody bodyge'
     let procfrag = Frame.PROC { Frame.procName  = proclab
                               , Frame.procBody  = wrappedbody
                               , Frame.procFrame = levelFrame level }
     frags <- getResult
     writeIORef fragsRef (procfrag : frags)

createMainFrag :: Level -> Gexp -> IO ()
createMainFrag lvl bodyge = 
  do mainlab <- Tmp.namedLabel "tigermain"
     createProcFrag mainlab lvl bodyge

reset :: IO ()
reset = do writeIORef fragsRef []

getResult :: IO [Frag]
getResult = do frags <- readIORef fragsRef
               return frags
