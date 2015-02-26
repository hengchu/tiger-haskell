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
import FrontEnd

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

exp :: Int -> Exp -> Exp
exp offset frameLocation = MEM(BINOP(PLUS, frameLocation, CONST offset), 4)
