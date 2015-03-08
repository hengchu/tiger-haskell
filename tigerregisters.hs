module TigerRegisters
  (
    Register(..)
  , npseudoregs
  , availregs
  )
  where

data Register = EAX | EBX | ECX | EDX | EBP | ESP | ESI | EDI | ZERO
                    | PSEUDO Int
                deriving (Eq)

npseudoregs :: Int
npseudoregs = 20

availregs :: [Register]
availregs = [EAX, EBX, ECX, EDX, EBP, ESP, ESI, EDI] ++ (map PSEUDO [1..npseudoregs])

instance Show Register where
  show EAX = "eax"
  show EBX = "ebx"
  show ECX = "ecx"
  show EDX = "edx"
  show EBP = "ebp"
  show ESP = "esp"
  show ESI = "esi"
  show EDI = "edi"
  show ZERO = "zero"
  show (PSEUDO d) = "F"++show d
