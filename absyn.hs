module TigerAlexer
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
  , Fundec(..)
  ) where

import TigerLexer
import qualified TigerSymbol as S

data Var = SimpleVar (S.Symbol, AlexPosn)
         | FieldVar (Var, S.Symbol, AlexPosn)
         | SubscriptVar (Var, S.Symbol, AlexPosn)

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

data Dec = FunctionDec [Fundec]
         | VarDec { varDecVar::Vardec, 
                    varDecTyp::Maybe (S.Symbol, AlexPosn), 
                    varDecInit::Exp,
                    varDecPos::AlexPosn }
         | TypeDec { typeDecName::S.Symbol, typeDecTy::Ty, typeDecPos::AlexPosn }

data Ty = NameTy (S.Symbol, AlexPosn)
        | RecordTy [Tfield]
        | ArrayTy (S.Symbol, AlexPosn)

data Oper = PlusOp | MinusOp | TimesOp | DivideOp
          | EqOp   | NeqOp   | LtOp    | LeOp | GtOp | GeOp

type Efield  = (S.Symbol, Exp, AlexPosn)
data Tfield  = Tfield  { tfieldName::S.Symbol, tfieldTyp::S.Symbol, tfieldPos::AlexPosn }
data Vardec  = Vardec  { vardecName::S.Symbol, vardecEscape::Bool }
data Formals = Formals { formalsVar::Vardec, formalsType::S.Symbol, formalsPos::AlexPosn }
data Fundec  = Fundec  { 
                         fundecName::S.Symbol
                       , fundecParams::[Formals]
                       , fundecResult::Maybe(S.Symbol, AlexPosn)
                       , fundecBody::Exp
                       , fundecPos::AlexPosn 
                       }
