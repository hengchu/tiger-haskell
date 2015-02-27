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
  , prettyprintfrag
  , prettyprintstm
  , notrel
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
import Prelude hiding (EQ, LT, GT)

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

prettyprintfrag :: Frag -> String
prettyprintfrag (PROC l s _) = (fst l) ++ ":\n" ++ prettyprintstm s ++ "\n"
prettyprintfrag (DATA l s) = (fst l) ++ ":\n" ++ show s ++ "\n"

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

notrel EQ = NE
notrel NE = EQ
notrel LT = GE
notrel GT = LE
notrel LE = GT
notrel GE = LT
notrel ULT = UGE
notrel ULE = UGT
notrel UGT = ULE
notrel UGE = ULT
notrel FEQ = FNE
notrel FNE = FEQ
notrel FLT = FGE
notrel FLE = FGT
notrel FGT = FLE
notrel FGE = FLT

-- Pretty printer
prettyprintstm :: Stm -> String
prettyprintstm s = execState (prettyprintstm' s) ""

prettyprintstm' :: Stm -> Control.Monad.State.State String () 
prettyprintstm' s =
  let
    say s = do out <- get
               put $ out ++ s
    sayln s = say s >> say "\n"

    indent :: Int -> Control.Monad.State.State String ()
    indent 0 = return ()
    indent i = say " " >> (indent $ i-1)

    stm :: Stm -> Int -> Control.Monad.State.State String ()
    stm (SEQ(a, b)) d = indent d >> sayln "SEQ(" >> (stm a $ d+1)
                                 >> sayln " " >> (stm b $ d+1) >> say ")"
    stm (LABEL lab) d = indent d >> (say $ fst lab ++ ":")
    stm (JUMP(e, _)) d = indent d >> sayln "JUMP(" >> (exp e $ d+1) >> say ")"
    stm (CJUMP(TEST(r, a, b), t, f)) d = do indent d
                                            sayln "CJUMP("
                                            indent $ d+1
                                            say $ show r
                                            sayln ","
                                            exp a $ d+1
                                            sayln ","
                                            exp b $ d+1
                                            sayln ","
                                            indent $ d+1
                                            say $ fst t
                                            say ","
                                            say $ fst f
                                            say ")"
    stm (MOVE(a, b)) d = indent d >> sayln "MOVE(" >> (exp a $ d+1)
                                  >> sayln "," >> (exp b $ d+1)
                                  >> say ")"
    stm (EXP e) d = indent d >> sayln "EXP(" >> (exp e $ d+1) >> say ")"
                                                 

    exp :: Exp -> Int -> Control.Monad.State.State String ()
    exp (BINOP(p, a, b)) d = indent d >> say "BINOP(" >> (say $ show p) >> sayln ","
                                      >> (exp a $ d+1) >> sayln "," >> (exp b $ d+1)
                                      >> say ")"
    exp (CVTOP(p, e, s1, s2)) d = indent d >> say "CVTOP[" >> (say $ show s1)
                                           >> say "," >> (say $ show s2) >> say "]("
                                           >> (say $ show p) >> sayln ","
                                           >> (exp e $ d+1) >> say ")"
    exp (MEM(e, s)) d = indent d >> say "MEM[" >> (say $ show s)
                                 >> sayln "](" >> (exp e $ d+1) >> say ")"
    exp (TEMP t) d = indent d >> say "TEMP " >> (say $ show t)
    exp (ESEQ(s, e)) d = indent d >> sayln "ESEQ(" >> (stm s $ d+1)
                                  >> sayln "," >> (exp e $ d+1) >> say ")"
    exp (NAME lab) d = indent d >> say "NAME " >> (say $ fst lab)
    exp (CONST i) d = indent d >> say "CONST " >> (say $ show i)
    exp (CONSTF r) d = indent d >> say "CONSTF" >> (say $ show r)
    exp (CALL(e, el)) d = indent d >> sayln "CALL(" >> (exp e $ d+1)
                                   >> mapM_ (\a -> sayln "," >> (exp a $ d+2)) el
                                   >> say ")"

  in
    stm s 0

-- Itree end
