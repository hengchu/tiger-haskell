module TigerITree
       ( Stm(..)
       , Exp(..)
       , Test(..)
       , Binop(..)
       , Relop(..)
       , Cvtop(..)
       , prettyprintstm
       )
       where

import qualified TigerTemp as Temp
import Control.Monad.State

type Size = Int

data Stm = SEQ   (Stm, Stm)
         | LABEL (Temp.Label)
         | JUMP  (Exp, [Temp.Label])
         | CJUMP (Test, Temp.Label, Temp.Label)
         | MOVE  (Exp, Exp)
         | EXP   Exp
         deriving (Show, Eq)

data Exp = BINOP  (Binop, Exp, Exp)
         | CVTOP  (Cvtop, Exp, Size, Size)
         | MEM    (Exp, Size)
         | TEMP   Temp.Temp
         | ESEQ   (Stm, Exp)
         | NAME   Temp.Label
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

-- Pretty printer
prettyprintstm :: Stm -> String
prettyprintstm s = execState (prettyprintstm' s) ""

prettyprintstm' :: Stm -> State String () 
prettyprintstm' s =
  let
    say s = do out <- get
               put $ out ++ s
    sayln s = say s >> say "\n"

    indent :: Int -> State String ()
    indent 0 = return ()
    indent i = say " " >> (indent $ i-1)

    stm :: Stm -> Int -> State String ()
    stm (SEQ(a, b)) d = indent d >> sayln "SEQ(" >> (stm a $ d+1)
                                 >> sayln " " >> (stm b $ d+1) >> say ")"
    stm (LABEL lab) d = if d == 0
                           then say $ (fst lab ++ ":")
                           else indent d >> (say $ fst lab)
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
                                                 

    exp :: Exp -> Int -> State String ()
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

