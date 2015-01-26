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
      parseExpTerm = try parseSeqExps
                 <|> try parseLetExp
                 <|> try parseArrCreation
                 <|> try parseRecCreation
                 <|> try parseAssignExp
                 <|> try parseVarExp
                 <|> try parseWhileExp
                 <|> try parseBreakExp
                 <|> try parseForExp
                 <|> try parseStr
                 <|> try parseNum 
                 <|> try parseNil
                 <|> (try $ between (pSimpleToken TLex.LPAREN) (pSimpleToken TLex.RPAREN) (expr))
                 <?> "Expression"

      parseNum = do (PToken pos _, num) <- pNumber
                    return (TAbsyn.IntExp num, pos)

      parseNil = do (PToken pos _) <- (pSimpleToken TLex.NIL)
                    return (TAbsyn.NilExp, pos)

      parseStr = do (PToken pos _, str) <- pString
                    return (TAbsyn.StringExp (str, pos), pos)

      parseTypeId = do (PToken pos _, str) <- pId
                       return (psymbol str, pos)

      parseSimpleId = parseTypeId

      parseArrCreation = do (s, spos) <- parseTypeId
                            (sze, epos) <- between (pSimpleToken TLex.LBRAK) (pSimpleToken TLex.RBRAK) expr
                            _ <- pSimpleToken TLex.OF
                            (inite, epos') <- expr
                            return $ (TAbsyn.ArrayExp { TAbsyn.arrayTyp = s
                                                      , TAbsyn.arraySize = sze
                                                      , TAbsyn.arrayInit = inite
                                                      , TAbsyn.arrayPos = spos }, spos)

      parseRecCreation = do (s, spos) <- parseTypeId
                            (efields, epos) <- between (pSimpleToken TLex.LBRAC) 
                                                       (pSimpleToken TLex.RBRAC) 
                                                       parseEfields
                            return $ (TAbsyn.RecordExp { TAbsyn.recordFields = efields
                                                       , TAbsyn.recordTyp = s
                                                       , TAbsyn.recordPos = spos }, spos)
                         where parseEfields = do fieldsAndPos <- parseEfield `sepBy` (pSimpleToken TLex.COMMA) 
                                                 return (map fst fieldsAndPos, snd $ head fieldsAndPos)
                               parseEfield  = do (s, spos) <- parseTypeId
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

      parseWhileExp = do _ <- pSimpleToken TLex.WHILE
                         (e1, pos1) <- expr
                         _ <- pSimpleToken TLex.DO
                         (e2, pos2) <- expr
                         return $ (TAbsyn.WhileExp { TAbsyn.whileTest = e1
                                                   , TAbsyn.whileBody = e2
                                                   , TAbsyn.whilePos  = pos1
                                                   }, pos1)

      parseForExp = do _ <- pSimpleToken TLex.FOR
                       (s, spos) <- parseSimpleId
                       _ <- pSimpleToken TLex.ASSIGN
                       (lowe, lowepos) <- expr
                       _ <- pSimpleToken TLex.TO
                       (highe, highepos) <- expr
                       _ <- pSimpleToken TLex.DO
                       (e, epos) <- expr
                       return $ (TAbsyn.ForExp { TAbsyn.forVar  = TAbsyn.Vardec {TAbsyn.vardecName = s, TAbsyn.vardecEscape = False}
                                               , TAbsyn.forLo   = lowe
                                               , TAbsyn.forHi   = highe
                                               , TAbsyn.forBody = e
                                               , TAbsyn.forPos  = spos
                                               }, spos)

      parseLetExp = do PToken letpos _ <- pSimpleToken TLex.LET
                       ds <- parseDecs
                       pSimpleToken TLex.IN
                       es <- expr `sepBy` (pSimpleToken TLex.SEMICOLON)
                       pSimpleToken TLex.END
                       return $ (TAbsyn.LetExp { TAbsyn.letDecs=ds
                                               , TAbsyn.letBody=TAbsyn.SeqExp (es)
                                               , TAbsyn.letPos=letpos }, letpos)

      parseDecs = many parseDec

      parseDec = try parseVardec 
                 <|> try parseFundec 
                 <|> try parseTypeDeclaration 
                 <?> "dec"
                 where parseFundec = do PToken fundecpos _ <- pSimpleToken TLex.FUNCTION
                                        (id, idpos) <- parseSimpleId
                                        tfields <- between (pSimpleToken TLex.LPAREN)
                                                           (pSimpleToken TLex.RPAREN)
                                                           parseTyfields
                                        maybetypeid <- optionMaybe (pSimpleToken TLex.COLON >> parseTypeId)
                                        pSimpleToken TLex.EQ
                                        (e, epos) <- expr
                                        return $ TAbsyn.FunctionDec { TAbsyn.fundecName=id
                                                                    , TAbsyn.fundecParams=tfields
                                                                    , TAbsyn.fundecResult=maybetypeid
                                                                    , TAbsyn.fundecBody=e
                                                                    , TAbsyn.fundecPos=fundecpos }
                       parseTypeDeclaration = do PToken tdecpos _ <- pSimpleToken TLex.TYPE
                                                 (id, idpos) <- parseSimpleId
                                                 pSimpleToken TLex.EQ
                                                 (ty, typos) <- parseTy
                                                 return $ TAbsyn.TypeDec { TAbsyn.typeDecName=id
                                                                         , TAbsyn.typeDecTy=ty
                                                                         , TAbsyn.typeDecPos=tdecpos }
                                              

      parseVardec = do PToken varpos _ <- pSimpleToken TLex.VAR
                       (s, spos) <- parseSimpleId
                       maybetypeid <- optionMaybe (pSimpleToken TLex.COLON >> parseTypeId)
                       pSimpleToken TLex.ASSIGN
                       (e, epos) <- expr
                       let vardec = TAbsyn.Vardec{TAbsyn.vardecName=s, TAbsyn.vardecEscape=False}
                       return $ case maybetypeid of
                                  Nothing -> TAbsyn.VarDec { TAbsyn.varDecVar=vardec
                                                           , TAbsyn.varDecTyp=Nothing
                                                           , TAbsyn.varDecInit=e
                                                           , TAbsyn.varDecPos=varpos }
                                  Just (t, tpos) -> TAbsyn.VarDec { TAbsyn.varDecVar=vardec
                                                                  , TAbsyn.varDecTyp=Just (t, tpos)
                                                                  , TAbsyn.varDecInit=e
                                                                  , TAbsyn.varDecPos=varpos }

      parseTy = try parseRecordDef <|> try parseArrayDefinition <|> parseTypeIdDefinition <?> "types"
                where parseRecordDef = do PToken lbracpos _ <- pSimpleToken TLex.LBRAC
                                          tfields <- parseTyfields
                                          pSimpleToken TLex.RBRAC
                                          return (TAbsyn.RecordTy tfields, lbracpos)
                      parseArrayDefinition = do PToken arrpos _ <- pSimpleToken TLex.ARRAY
                                                pSimpleToken TLex.OF
                                                (s, spos) <- parseTypeId
                                                return (TAbsyn.ArrayTy(s, arrpos), arrpos)
                      parseTypeIdDefinition = do (s, spos) <- parseTypeId
                                                 return (TAbsyn.NameTy(s, spos), spos)

      parseTyfields = parseTyfield `sepBy` (pSimpleToken TLex.COMMA)

      parseTyfield = do (s, spos) <- parseSimpleId
                        _ <- pSimpleToken TLex.COLON
                        (t, tpos) <- parseTypeId
                        return $ TAbsyn.Tfield { TAbsyn.tfieldName = s
                                               , TAbsyn.tfieldTyp  = t
                                               , TAbsyn.tfieldPos  = spos }

      parseBreakExp = do PToken pos _ <- pSimpleToken TLex.BREAK
                         return (TAbsyn.BreakExp { TAbsyn.breakPos = pos }, pos)
      
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

      parseSeqExps = do es <- between (pSimpleToken TLex.LPAREN) (pSimpleToken TLex.RPAREN) $ expr `sepBy` (pSimpleToken TLex.SEMICOLON)
                        return $ case es of
                                      [] -> (TAbsyn.SeqExp([]), TLex.AlexPn 0 0 0)
                                      (_, epos):_ -> (TAbsyn.SeqExp(es), epos)

      expr = buildExpressionParser table parseExpTerm

      expr' = do (e, pos) <- expr <?> "Expression"
                 pSimpleToken TLex.EOF
                 return (TAbsyn.Pexp e)

      decs = do ds <- parseDecs
                pSimpleToken TLex.EOF
                return (TAbsyn.Pdecs ds)
  
      program = try expr' <|> try decs <?> "program"

    in

      case (Text.Parsec.parse program "INPUT" ptokens) of
        Left err -> Left err
        Right p -> Right p
