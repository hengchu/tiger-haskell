import qualified TigerSemant2 as TS
import qualified TigerLexer  as TLex
import qualified TigerParser2 as TP
import Text.Parsec
import qualified FrontEnd as Frt
import Control.Monad.State
import Control.Monad.Except
import System.Environment

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let eithertoken = TLex.scanner fileContent
          case eithertoken of
            Left lexerr -> print lexerr
            Right tok -> do let statemonad = runPT TS.transprog Frt.initialSymbolTempState file (map TP.token2ptoken tok)
                            let exceptmonad = evalStateT statemonad Frt.initialSemantState
                            let iomonad = runExceptT exceptmonad
                            stuff <- iomonad
                            print stuff
