import qualified TigerSemant as TS
import qualified TigerLexer  as TLex
import qualified TigerParser as TP
import Text.Parsec
import qualified FrontEnd as Frt
import Control.Monad.State
import Control.Monad.Except
import System.Environment
import qualified TigerCanon as TCan
import Data.List

isdata (Frt.DATA _ _) = True
isdata (Frt.PROC _ _ _) = False
isproc = not . isdata

trans = do frags <- TS.transprog
           let datafrags = filter isdata frags
           let procfrags = filter isproc frags
           linearized <- mapM TCan.linearize $ map Frt.procBody procfrags
           basicblocks <- mapM TCan.basicblocks linearized
           traces <- mapM TCan.tracesched basicblocks
           let joined = concat traces
           let datastrs = map Frt.prettyprintfrag datafrags
           let codestrs = map Frt.prettyprintstm joined
           let datastr = (concat . intersperse "\n") datastrs
           let codestr = (concat . intersperse "\n") codestrs
           return $ codestr ++ "\n" ++ datastr
           

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let eithertoken = TLex.scanner fileContent
          case eithertoken of
            Left lexerr -> print lexerr
            Right tok -> do let statemonad = runPT trans Frt.initialSymbolTempState file (map TP.token2ptoken tok)
                            let exceptmonad = evalStateT statemonad Frt.initialSemantState
                            let iomonad = runExceptT exceptmonad
                            stuff <- iomonad
                            case stuff of
                              Left parserr -> print parserr
                              Right (Left semanterr) -> print semanterr
                              Right (Right output) -> putStrLn output
