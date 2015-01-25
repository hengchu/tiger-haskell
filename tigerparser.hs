module TigerParser
       (
         TigerParser.parse
       ) where

import qualified TigerLexer as TLex
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr
import Data.Functor.Identity
import qualified TigerAbsyn as TAbsyn
import qualified TigerSymbol as S
import Data.Maybe

type TParsec a b = Parsec [a] () b
type TigParsec b = TParsec PToken b

data PToken = PToken TLex.AlexPosn TLex.TokenClass
              deriving (Show, Eq)

data LvalueTail = Dot (S.Symbol, TLex.AlexPosn)
                | Bracket (TAbsyn.Exp, TLex.AlexPosn)

parse :: [TLex.Token] -> Either ParseError TAbsyn.Program
parse tokens =
    let

      ptokens   = map tokenToPToken tokens
      symbolmap = S.symbolMapFromStrings idstrs
                  where idstrs = map stripId ids
                        ids = filter isID ptokens
                        stripId (PToken _ (TLex.Id str)) = str
                        isID (PToken _ (TLex.Id _)) = True
                        isID (PToken _ _          ) = False

      psymbol :: String -> S.Symbol
      psymbol str = fromJust $ S.symbol str symbolmap

      pname :: S.Symbol -> String
      pname = S.name

      tokenToPToken :: TLex.Token -> PToken
      tokenToPToken (TLex.Token p c _) = PToken p c

      updatePos pos (PToken (TLex.AlexPn _ line col) c) toks
                   = setSourceLine (setSourceColumn pos col) (line)

      pSimpleToken :: TLex.TokenClass -> TigParsec PToken
      pSimpleToken simpTok = tokenPrim show updatePos acceptTok
        where acceptTok (PToken _ (TLex.Id _))      = Nothing
              acceptTok (PToken _ (TLex.Number _))  = Nothing
              acceptTok (PToken _ (TLex.Str _))     = Nothing
              acceptTok t@(PToken _ c) | simpTok == c = Just t
                                     | otherwise      = Nothing

      pId :: TigParsec (PToken, String)
      pId = tokenPrim show updatePos acceptTok
        where acceptTok i@(PToken _ (TLex.Id name)) = Just (i, name)
              acceptTok _                           = Nothing

      pNumber :: TigParsec (PToken, Int)
      pNumber = tokenPrim show updatePos acceptTok
        where acceptTok i@(PToken _ (TLex.Number num)) = Just (i, num)
              acceptTok _                              = Nothing

      pString :: TigParsec (PToken, String)
      pString = tokenPrim show updatePos acceptTok
        where acceptTok i@(PToken _ (TLex.Str str)) = Just (i, str)
              acceptTok _                           = Nothing

      parseExpTerm :: TigParsec (TAbsyn.Exp, TLex.AlexPosn)
      parseExpTerm = try parseAssignExp
                 <|> try parseVarExp
                 <|> try parseArrCreation
                 <|> try parseRecCreation
                 <|> try parseNum 
                 <|> try parseNil
                 <?> "Expression"

      parseNum = do (PToken pos _, num) <- try pNumber
                    return (TAbsyn.IntExp num, pos)

      parseNil = do (PToken pos _) <- try (pSimpleToken TLex.NIL)
                    return (TAbsyn.NilExp, pos)

      parseStr = do (PToken pos _, str) <- try pString
                    return (TAbsyn.StringExp (str, pos), pos)

      parseTypeId = do (PToken pos _, str) <- try pId
                       return (psymbol str, pos)

      parseSimpleId = parseTypeId

      parseArrCreation = do (s, spos) <- try parseTypeId
                            (sze, epos) <- try $ between (pSimpleToken TLex.LBRAK) (pSimpleToken TLex.RBRAK) expr
                            _ <- pSimpleToken TLex.OF
                            (inite, epos') <- expr
                            return $ (TAbsyn.ArrayExp { TAbsyn.arrayTyp = s
                                                      , TAbsyn.arraySize = sze
                                                      , TAbsyn.arrayInit = inite
                                                      , TAbsyn.arrayPos = spos }, spos)

      parseRecCreation = do (s, spos) <- try parseTypeId
                            (efields, epos) <- try $ between (pSimpleToken TLex.LBRAC) 
                                                             (pSimpleToken TLex.RBRAC) 
                                                             parseEfields
                            return $ (TAbsyn.RecordExp { TAbsyn.recordFields = efields
                                                       , TAbsyn.recordTyp = s
                                                       , TAbsyn.recordPos = spos }, spos)
                         where parseEfields = do fieldsAndPos <- parseEfield `sepBy` (pSimpleToken TLex.COMMA) 
                                                 return (map fst fieldsAndPos, snd $ head fieldsAndPos)
                               parseEfield  = do (s, spos) <- try parseTypeId
                                                 _ <- pSimpleToken TLex.EQ
                                                 (e, epos) <- expr
                                                 return ((s, e, epos), epos)

      parseVarExp = do (var, pos) <- parseLValue
                       return (TAbsyn.VarExp var, pos)

      -- This is a left-factored version of the L-value grammar
      parseLValue = do (s, pos) <- parseSimpleId
                       maybel'  <- optionMaybe $ parseLValue'
                       return $ case maybel' of
                                  Nothing -> (TAbsyn.SimpleVar (s, pos), pos)
                                  Just tails -> (attachTail (TAbsyn.SimpleVar(s, pos)) tails pos, pos)
                    where parseLValue' = try op1 <|> try op2 <|> try pDotId <|> pBracketExp <?> "l'-type expression"
                          pDotId = do _ <- pSimpleToken TLex.DOT
                                      (s, pos) <- parseSimpleId
                                      return $ [Dot (s, pos)]
                          pBracketExp = do (e, pos) <- between (pSimpleToken TLex.LBRAK) (pSimpleToken TLex.RBRAK) expr
                                           return $ [Bracket(e, pos)]
                          op1 = do dotid <- pDotId
                                   rest <- parseLValue'
                                   return $ dotid++rest
                          op2 = do bexp <- pBracketExp
                                   rest <- parseLValue'
                                   return $ bexp++rest 
                          attachTail var ((Dot (s, spos)):ts) pos = attachTail (TAbsyn.FieldVar (var, s, spos)) ts pos
                          attachTail var ((Bracket (e, epos)):ts) pos = attachTail (TAbsyn.SubscriptVar (var, e, epos)) ts pos
                          attachTail var [] _ = var
                                               
                                            
 
      parseAssignExp = do (var, vpos) <- parseLValue
                          _ <- pSimpleToken TLex.ASSIGN
                          (e, epos) <- expr
                          return (TAbsyn.AssignExp { TAbsyn.assignVar=var
                                                   , TAbsyn.assignExp=e
                                                   , TAbsyn.assignPos=vpos
                                                   }, vpos)
      
      table = [
                [ prefix (pSimpleToken TLex.MINUS)  negateOp ]
              , [ binary (pSimpleToken TLex.MULT)  (binaryOp TAbsyn.TimesOp) AssocLeft
                , binary (pSimpleToken TLex.DIV)   (binaryOp TAbsyn.DivideOp) AssocLeft ]
              , [ binary (pSimpleToken TLex.PLUS)  (binaryOp TAbsyn.PlusOp) AssocLeft
                , binary (pSimpleToken TLex.MINUS) (binaryOp TAbsyn.MinusOp) AssocLeft ]
              ]

      negateOp (a, pos) = (TAbsyn.OpExp { TAbsyn.opLeft  = TAbsyn.IntExp 0
                                        , TAbsyn.opOper  = TAbsyn.MinusOp
                                        , TAbsyn.opRight = a
                                        , TAbsyn.opPos   = pos }, pos)

      binaryOp op (a, pos) (b, _) = (TAbsyn.OpExp { TAbsyn.opLeft  = a
                                                  , TAbsyn.opOper  = op
                                                  , TAbsyn.opRight = b
                                                  , TAbsyn.opPos   = pos
                                                  }, pos)

      prefix  pOp fun       = Prefix  (try $ do { pOp<?>"prefix operator"; return fun })
      postFix pOp fun       = Postfix (try $ do { pOp<?>"postfix operator"; return fun })
      binary  pOp fun assoc = Infix   (try $ do { pOp<?>"binary operator"; return fun }) assoc

      expr = buildExpressionParser table parseExpTerm

      expr' = do (e, pos) <- expr <?> "Expression"
                 pSimpleToken TLex.EOF
                 return (e, pos)

    in

      case (Text.Parsec.parse expr' "INPUT" ptokens) of
        Left err -> Left err
        Right (e,p) -> Right $ TAbsyn.Pexp e
