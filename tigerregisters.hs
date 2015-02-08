module TigerRegisters
  (
    Register(..)
  )
  where

data Register = EAX | EBX | ECX | EDX | EBP | ESP | ESI | EDI
                deriving (Eq, Show)
