module TigerAbsyn
  (
    Var(..)
  , Exp(..)
  , Dec(..)
  , Ty(..)
  , Oper(..)
  , Efield
  , Tfield(..)
  , Vardec(..)
  , Formals(..)
  , Program(..)
  ) where

import TigerLexer
import qualified TigerSymbol as S

data Program = Pexp Exp
             | Pdecs [Dec]
               deriving (Show, Eq)

data Var = SimpleVar (S.Symbol, AlexPosn)
         | FieldVar (Var, S.Symbol, AlexPosn)
         | SubscriptVar (Var, Exp, AlexPosn)
           deriving (Show, Eq)

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp  (String, AlexPosn)
         | SeqExp     [(Exp, AlexPosn)]
         | AppExp     { appFunc::S.Symbol, appArgs::[Exp], appPos::AlexPosn                    }
         | OpExp      { opLeft::Exp, opOper::Oper, opRight::Exp, opPos::AlexPosn               }
         | RecordExp  { recordFields::[Efield], recordTyp::S.Symbol, recordPos::AlexPosn       }
         | AssignExp  { assignVar::Var, assignExp::Exp, assignPos::AlexPosn                    }
         | IfExp      { ifTest::Exp, ifThen::Exp, ifElse::Maybe Exp, ifPos::AlexPosn           }
         | WhileExp   { whileTest::Exp, whileBody::Exp, whilePos::AlexPosn                     }
         | ForExp     { forVar::Vardec, forLo::Exp, forHi::Exp, forBody::Exp, forPos::AlexPosn }
         | BreakExp   { breakPos::AlexPosn                                                     }
         | LetExp     { letDecs::[Dec], letBody::Exp, letPos::AlexPosn                         }
         | ArrayExp   { arrayTyp::S.Symbol, arraySize::Exp, arrayInit::Exp, arrayPos::AlexPosn }
           deriving (Show, Eq)

data Dec = FunctionDec { 
                         fundecName::S.Symbol
                       , fundecParams::[Tfield]
                       , fundecResult::Maybe(S.Symbol, AlexPosn)
                       , fundecBody::Exp
                       , fundecPos::AlexPosn 
                       }
         | VarDec { varDecVar::Vardec, 
                    varDecTyp::Maybe (S.Symbol, AlexPosn), 
                    varDecInit::Exp,
                    varDecPos::AlexPosn }
         | TypeDec { typeDecName::S.Symbol, typeDecTy::Ty, typeDecPos::AlexPosn }
           deriving (Show, Eq)

data Ty = NameTy (S.Symbol, AlexPosn)
        | RecordTy [Tfield]
        | ArrayTy (S.Symbol, AlexPosn)
           deriving (Show, Eq)

data Oper = PlusOp | MinusOp | TimesOp | DivideOp
          | EqOp   | NeqOp   | LtOp    | LeOp | GtOp | GeOp
            deriving (Show, Eq)

type Efield  = (S.Symbol, Exp, AlexPosn)
data Tfield  = Tfield  { tfieldName::S.Symbol, tfieldTyp::S.Symbol, tfieldPos::AlexPosn }
                 deriving (Show, Eq)
data Vardec  = Vardec  { vardecName::S.Symbol, vardecEscape::Bool }
                 deriving (Show, Eq)
data Formals = Formals { formalsVar::Vardec, formalsType::S.Symbol, formalsPos::AlexPosn }
                 deriving (Show, Eq)
