module TigerRegisters
  (
    Register(..)
  , npseudoregs
  , availregs
  )
  where

data Register = EAX | EBX | ECX | EDX | EBP | ESP | ESI | EDI | ZERO
                    | PSEUDO Int
                deriving (Eq, Show)

npseudoregs :: Int
npseudoregs = 20

availregs :: [Register]
availregs = [EAX, EBX, ECX, EDX, EBP, ESP, ESI, EDI] ++ (map PSEUDO [1..npseudoregs])
