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
  ) where

import qualified TigerFrame as Frame
import qualified TigerITree as Tr
import qualified TigerTemp as Tmp
import qualified TigerAbsyn as Absyn
import Data.IORef


type Uniq = Integer

uniqCounter :: IO (IORef Integer)
uniqCounter = newIORef 0

genUniq :: IO Integer
genUniq = do ref <- uniqCounter
             c <- readIORef ref
             writeIORef ref (c+1)
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

-- Cx-constructed expression comparing two strings for equality
eqStr :: Gexp -> Gexp -> Gexp
eqStr = undefined

notEqStr :: Gexp -> Gexp -> Gexp
notEqStr = undefined

strLessThan :: Gexp -> Gexp -> Gexp
strLessThan = undefined

strLessThanOrEq :: Gexp -> Gexp -> Gexp
strLessThanOrEq = undefined


-- Comparing non-string values
eqCmp :: Gexp -> Gexp -> Gexp
eqCmp = undefined

notEqCmp :: Gexp -> Gexp -> Gexp
notEqCmp = undefined

lessThan :: Gexp -> Gexp -> Gexp
lessThan = undefined

lessThanOrEq :: Gexp -> Gexp -> Gexp
lessThanOrEq = undefined

-- Arithmetic
arithmetic :: Absyn.Oper -> Gexp -> Gexp -> Gexp
arithmetic = undefined

-- Literal
intExp :: Int -> Gexp
intExp = undefined

stringExp :: String -> Gexp
stringExp = undefined


-- Helper sequence function
constructEseq :: Gexp -> Gexp -> Gexp
constructEseq = undefined

letExpression :: [Gexp] -> Gexp -> Gexp
letExpression = undefined

-- Assignment
assign :: Gexp -> Gexp -> Gexp
assign = undefined

-- Record and Array creation
createRecord :: [Gexp] -> Gexp
createRecord = undefined

createArray :: Gexp -> Gexp -> Gexp
createArray = undefined

-- Variable access
field :: Gexp -> Int -> Gexp
field = undefined

subscript :: Gexp -> Gexp -> Gexp
subscript = undefined

simpleVar :: Access -> Level -> Gexp
simpleVar = undefined

-- Conditional and loops
ifThen :: Gexp -> Gexp -> Gexp
ifThen = undefined

ifThenElse :: Gexp -> Gexp -> Gexp -> Gexp
ifThenElse = undefined

whileLoop :: Gexp -> Gexp -> Tmp.Label -> Gexp
whileLoop = undefined

forLoop :: Gexp -> Gexp -> Gexp -> Tmp.Label -> Gexp -> Gexp
forLoop = undefined

break :: Tmp.Label -> Gexp
break = undefined

callFunction :: Tmp.Label -> Level -> Level -> [Gexp] -> Gexp
callFunction = undefined

createProcFrag :: Tmp.Label -> Level -> Gexp -> IO ()
createProcFrag = undefined

createMainFrag :: Level -> Gexp -> IO ()
createMainFrag = undefined

reset :: IO ()
reset = undefined

getResult :: IO [Frag]
getResult = undefined
