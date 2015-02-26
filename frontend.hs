module FrontEnd
  (
    Semant
  , SemantError(..)
  , SemantErrorClass(..)
  , SemantState
  , PToken(..)
  , Frontend
  , SymbolTempState
  , Symbol
  , Label
  , Temp(..)
  , Offset
  , Frame(..)
  , Frag(..)
  , semantStGet
  , semantStPut
  , genUniq
  , Stm(..)
  , Exp(..)
  , Test(..)
  , Binop(..)
  , Relop(..)
  , Cvtop(..)
  , Ty(..)
  , Access
  , Level(..)
  , EnvEntry(..)
  , Venv
  , Tenv
  , LoopLevel
  , Uniq
  , initialSymbolTempState
  , initialSemantState
  )
  where

-- Label/Symbol > Parser > SemanticAnalysis

import TigerRegisters
import Text.Parsec
import Control.Monad.State
import Control.Monad.Except
import Data.IORef
import qualified Data.Map as Map
import qualified TigerLexer as TLex
import Distribution.Simple.Utils (lowercase)

-- This is a uniq number generator for the entire frontend.
type Uniq = Integer

-- These are the semantic types of tiger langauge. Used during type checking.
data Ty = Record ([(Symbol, Ty)], Uniq)
        | Nil
        | INT
        | String
        | Array (Ty, Uniq)
        | Name (Symbol, IORef (Maybe Ty))
        | Unit

instance Show Ty where
  show (Record (sts, u)) = "Record: (" ++ show sts ++ ", " ++ show u ++ ")"
  show Nil = "Nil"
  show INT = "Int"
  show String = "String"
  show (Array(t, u)) = "Array: (" ++ show t ++ ", " ++ show u ++ ")"
  show (Name(s, _)) = "Name: " ++ show s
  show Unit = "Unit"

instance Eq Ty where
  (Record _)           == Nil               = True
  Nil                  == (Record _)        = True
  Record (stypairs, u) == Record (stypairs', u') = stypairs == stypairs' && u == u'
  Nil                  == Nil               = True
  INT                  == INT               = True
  String               == String            = True
  Array (t, u)         == Array (t', u')    = t == t' && u == u'
  Name (s, ioref)      == Name (s', ioref') = s == s' && ioref == ioref'
  Unit                 == Unit              = True
  _ == _ = False

-- These are the entries of the environment mappings during type checking.
data EnvEntry = VarEntry { varAccess :: Access
                         , varTy::Ty
                         , varReadOnly::Bool }
              | FunEntry { funLevel :: Level
                         , funLabel :: Label
                         , funFormals::[(Ty, Access)]
                         , funResult::Ty}
  deriving(Show)

-- Environment mappings.
type Venv = Map.Map Symbol EnvEntry
type Tenv = Map.Map Symbol Ty

-- One of the state types for semant. It is used to track how many nested loops we're currently in.
type LoopLevel = Int

type SemantState = ( Venv   -- ^ variable map
                   , Tenv   -- ^ type map
                   , LoopLevel
                   , [Frag] -- ^ frag list
                   ) 

initialSemantState :: SemantState
initialSemantState = (Map.empty, Map.empty, 0, [])

-- Types of errors semant can generate.
data SemantErrorClass = TypeMismatch String String
                      | TypeLoop     [String]
                      | NotCallable  String
                      | UndefinedBinop String String
                      | Undefined    String
                      | ArgumentCount Int Int
                      | ArgumentName String String
                      | BreakOutsideOfLoop
                      | DuplicateDefinition String
                      | NotVariable String
  deriving(Show, Eq)

-- The actual error type.
data SemantError = SE TLex.AlexPosn SemantErrorClass
  deriving(Show, Eq)

-- The semant monad.
type Semant = StateT SemantState (ExceptT SemantError IO)

-- Simplified token type for the parser.
data PToken = PToken TLex.AlexPosn TLex.TokenClass
  deriving(Show, Eq)

-- All the states we need to track for the symbol module.
type SymbolTempState = ( Map.Map String Int -- ^ Map from string to symbol num
                       , Int                -- ^ Symbol Count
                       , Int                -- ^ Temp Count
                       , Int                -- ^ Label Count
                       , Integer )          -- ^ Uniq Gen

initialSymbolTempState :: SymbolTempState
initialSymbolTempState = (Map.empty, 0, 0, 0, 0)


-- The Frontend monad.
type Frontend = ParsecT [PToken] SymbolTempState Semant

-- Symbol type
type Symbol = (String, Int)

-- Temp types
data Temp = Temp Int
          | Named Register
            deriving (Eq)

instance Show Temp where
  show (Temp num) = "T"++show num
  show (Named reg) = lowercase $ show reg

type Label = Symbol
-- end of Temp types

-- Frame/Translate types
type Access = (Level, Int)

data Level = LEVEL { levelFrame :: Frame
                   , staticLinkOffset :: Int
                   , levelParent :: Level
                   , levelUniq :: Uniq }
           | TOP
           deriving (Show, Eq)

type Offset = Int
data Frame  = Frame { frameFormals     :: Int
                     ,frameOfflist     :: [Offset]
                     ,frameLocalCount  :: IORef Int -- Count of local variables in frame
                    }
                    deriving (Eq)

instance Show Frame where
  show _ = "<frame>"

data Frag = PROC { procName  :: Label
                 , procBody  :: Stm
                 , procFrame :: Frame }
          | DATA { dataLab :: Label
                 , dataStr :: String }
          deriving (Eq)

instance Show Frag where
  show (DATA {dataLab=lab, dataStr=str}) = "dataLab: " ++ show lab ++ "\n" ++ str
  show (PROC {procName=name, procBody=body}) = "procName: " ++ show name ++ "\n" ++ show body
-- end of Frame types

semantStGet :: Frontend SemantState
semantStGet = lift get

semantStPut :: SemantState -> Frontend ()
semantStPut = lift . put

genUniq :: Frontend Integer
genUniq = do (m, sc, tc, lc, u) <- getState
             putState (m, sc, tc, lc, u+1)
             return u
-- Itree

type Size = Int

data Stm = SEQ   (Stm, Stm)
         | LABEL (Label)
         | JUMP  (Exp, [Label])
         | CJUMP (Test, Label, Label)
         | MOVE  (Exp, Exp)
         | EXP   Exp
         deriving (Show, Eq)

data Exp = BINOP  (Binop, Exp, Exp)
         | CVTOP  (Cvtop, Exp, Size, Size)
         | MEM    (Exp, Size)
         | TEMP   Temp
         | ESEQ   (Stm, Exp)
         | NAME   Label
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

-- Itree end
