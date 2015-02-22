module TigerSemantTypes
       (
         Ty(..)
       , Uniq
       ) where

import FrontEnd

instance Show Ty where
  show (Record (xs, u)) = "Record: (" ++ show xs ++ " )"
  show Nil = "Nil"
  show INT = "INT"
  show String = "String"
  show (Array(t, u)) = "Array: (" ++ show t ++ "[])"
  show (Name(s, ref)) = "Name: (" ++ show s ++ ", <ioref>)"
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
