module TigerFrame
       ( 
         Offset
       , Frame(..)
       , Frag(..)
       , newFrame
       , allocLocalInFrame
       , TigerFrame.exp
       )
       where

import Data.IORef
import qualified TigerITree as Tree
import qualified TigerTemp  as Temp

type Offset = Int
data Frame  = Frame { frameFormals     :: Int
                     ,frameOfflist     :: [Offset]
                     ,frameLocalCount  :: IORef Int -- Count of local variables in frame
                    }
                    deriving (Eq)
data Frag = PROC { procName  :: Temp.Label
                 , procBody  :: Tree.Stm
                 , procFrame :: Frame }
          | DATA { dataLab :: Temp.Label
                 , dataStr :: String }
          deriving (Eq)

newFrame :: Int -> IO (Frame, [Offset])
newFrame numFormals = 
  do localRef <- newIORef 0
     return (Frame { frameFormals=numFormals
                   , frameOfflist=offlist
                   , frameLocalCount=localRef }, offlist)
       where offlist = offsetListForNumFormals numFormals []
             offsetListForNumFormals 0 l  = reverse l
             offsetListForNumFormals n [] = offsetListForNumFormals (n-1) [4]
             offsetListForNumFormals n l@(x:_) = offsetListForNumFormals (n-1) ((x+4):l)

allocLocalInFrame :: Frame -> IO Offset
allocLocalInFrame Frame { frameLocalCount=locals } = 
  do c <- readIORef locals
     writeIORef locals (c+1)
     return $ c * (-4)

exp :: Int -> Tree.Exp -> Tree.Exp
exp offset frameLocation = Tree.MEM(Tree.BINOP(Tree.PLUS, frameLocation, Tree.CONST offset), 4)
