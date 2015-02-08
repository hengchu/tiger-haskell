import qualified TigerSement as TS
import qualified TigerLexer  as TLex
import qualified TigerParser as TP
import System.Environment

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let eithertoken = TLex.scanner fileContent
          case eithertoken of
            Left lexerr -> print lexerr
            Right tokens -> do parseresult <- TP.parse tokens
                               case parseresult of
                                 Left parseError -> print parseError
                                 Right absyn -> do analysis <- TS.transProg absyn
                                                   case analysis of
                                                     Left sementerr -> print sementerr
                                                     Right frags -> mapM_ print frags
