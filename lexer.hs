module Lexer
     ( Token(..)
     , TokenPos
     , tokenize
     ) where

import Numeric
import Data.Char
import Text.ParserCombinators.Parsec hiding (token, tokens)
import Control.Applicative ((<*), (*>), (<$>), (<*>))

data Token = ARRAY
           | IF
           | THEN
           | ELSE
           | WHILE
           | FOR
           | TO
           | DO
           | LET
           | IN
           | END
           | OF
           | BREAK
           | NIL
           | FUNCTION
           | VAR
           | TYPE
           | IMPORT
           | PRIMITIVE
           | Id String
           | Number Int
           | Str String
           | COMMA
           | COLON
           | SEMICOLON
           | LPAREN
           | RPAREN
           | LBRAK
           | RBRAK
           | LBRAC
           | RBRAC
           | DOT
           | PLUS
           | MINUS
           | MULT
           | DIV
           | EQ
           | NEQ
           | LT
           | LEQ
           | GT
           | GEQ
           | AMPERSAND
           | BAR
           | ASSIGN
     deriving (Show, Eq)

type TokenPos = (Token, SourcePos)

parsePos :: Parser Token -> Parser TokenPos
parsePos p = do a   <- p
                pos <- getPosition
                return (a, pos)

comma, colon, semicolon :: Parser TokenPos
comma     = parsePos $ char ',' >> return COMMA
colon     = parsePos $ char ':' >> return COLON
semicolon = parsePos $ char ';' >> return SEMICOLON

lparen, rparen, lbrak, rbrak, lbrac, rbrac :: Parser TokenPos
lparen = parsePos $ char '(' >> return LPAREN
rparen = parsePos $ char ')' >> return LPAREN
lbrak  = parsePos $ char '[' >> return LBRAK
rbrak  = parsePos $ char ']' >> return RBRAK
lbrac  = parsePos $ char '{' >> return LBRAC
rbrac  = parsePos $ char '}' >> return RBRAC

dot, plus, minus, div, eq, lt, leq, gt, geq :: Parser TokenPos
dot   = parsePos $ char '.'    >> return DOT
plus  = parsePos $ char '+'    >> return PLUS
minus = parsePos $ char '-'    >> return MINUS
div   = parsePos $ char '/'    >> return DIV
eq    = parsePos $ char '='    >> return Lexer.EQ
lt    = parsePos $ char '<'    >> return Lexer.LT
leq   = parsePos $ string "<=" >> return LEQ
gt    = parsePos $ char '>'    >> return Lexer.GT
geq   = parsePos $ string ">=" >> return GEQ

ampersand, bar, assign :: Parser TokenPos
ampersand = parsePos $ char '&'    >> return AMPERSAND
bar       = parsePos $ char '|'    >> return BAR
assign    = parsePos $ string ":=" >> return ASSIGN

nidentifier_ :: Parser Token
nidentifier_ = do fc <- oneOf firstChar
                  r  <- optionMaybe (many $ oneOf rest)
                  spaces
                  return $ case r of
                            Nothing -> Id [fc]
                            Just s  -> Id $ [fc] ++ s
                where firstChar = ['A'..'Z'] ++ ['a'..'z']
                      rest      = firstChar ++ ['0'..'9'] ++ ['-']

midentifier_ :: Parser Token
midentifier_ = do mainid <- string "_main"
                  spaces
                  return $ Id mainid

identifier_ :: Parser Token
identifier_ = midentifier_ <|> nidentifier_

identifier :: Parser TokenPos
identifier = parsePos identifier_

number_ :: Parser Token
number_ = do num <- many1 digit
             let (val,_):_ = readDec num
             return $ Number val

number = parsePos number_

tarr, tif, tthen, telse, twhile, tfor, tto, tdo, tlet, tin, 
  tend, tof, tbreak, tnil, tfunction, tvar, ttype, timport,
  tprimitive :: Parser TokenPos

tarr       = parsePos $ string "array" >> return ARRAY
tif        = parsePos $ string "if" >> return IF
tthen      = parsePos $ string "then" >> return THEN
telse      = parsePos $ string "else" >> return ELSE
twhile     = parsePos $ string "while" >> return WHILE
tfor       = parsePos $ string "for" >> return FOR
tto        = parsePos $ string "to" >> return TO
tdo        = parsePos $ string "do" >> return DO
tlet       = parsePos $ string "let" >> return LET
tin        = parsePos $ string "in" >> return IN
tend       = parsePos $ string "end" >> return END
tof        = parsePos $ string "of" >> return OF
tbreak     = parsePos $ string "break" >> return BREAK
tnil       = parsePos $ string "nil" >> return NIL
tfunction  = parsePos $ string "function" >> return FUNCTION
tvar       = parsePos $ string "var" >> return VAR
ttype      = parsePos $ string "type" >> return TYPE
timport    = parsePos $ string "import" >> return IMPORT
tprimitive = parsePos $ string "primitive" >> return PRIMITIVE

keywords = choice $ map try [
              tarr      
            , tif       
            , tthen     
            , telse     
            , twhile    
            , tfor      
            , tto       
            , tdo       
            , tlet      
            , tin       
            , tend      
            , tof       
            , tbreak    
            , tnil      
            , tfunction 
            , tvar      
            , ttype     
            , timport   
            , tprimitive
          ]

-- begin of string token

stresc :: Parser Char
stresc = do backslash <- char '\\'
            c <- oneOf "\\\"abfnrtv"
            return $ case c of
                          '\\' -> '\\'
                          '\"' -> '\"'
                          'a'  -> '\a'
                          'b'  -> '\b'
                          'f'  -> '\f'
                          'n'  -> '\n'
                          'r'  -> '\r'
                          't'  -> '\t'
                          'v'  -> '\v'

strhexesc :: Parser Char
strhexesc = do string "\\x"
               hex <- many1 hexDigit
               let (val,_):_ = readHex hex
               return $ chr val

stroctesc :: Parser Char
stroctesc = do char '\\'
               oct <- many1 octDigit
               let (val,_):_ = readOct oct
               return $ chr val

strnonesc :: Parser Char
strnonesc = noneOf "\\\""

strchar :: Parser Char
strchar = choice [ try stresc
                 , try strhexesc
                 , try stroctesc
                 , strnonesc
                 ]

string_ :: Parser Token
string_ = do char '"'
             content <- many strchar
             char '"' <?> "end of string quote"
             return $ Str content

stringtok = parsePos string_

-- end of string token

other :: Parser TokenPos
other = parsePos $ anyChar >> fail "Unrecognized char"

token :: Parser TokenPos
token = choice $ choices 
        where choices = (map try [ keywords
                                 , identifier
                                 , number
                                 , stringtok
                                 , assign
                                 , comma
                                 , colon
                                 , semicolon
                                 , lparen
                                 , rparen
                                 , lbrak
                                 , rbrak
                                 , lbrac
                                 , rbrac
                                 , dot
                                 , plus
                                 , minus
                                 , Lexer.div
                                 , leq
                                 , lt
                                 , geq
                                 , gt
                                 , eq
                                 , ampersand
                                 , bar
                                 ]) ++ [other]

tokens :: Parser [TokenPos]
tokens = spaces *> many (token <* spaces)

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokens ()
