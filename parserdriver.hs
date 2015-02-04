import TigerParser
import TigerLexer
import System.Environment

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let res = scanner fileContent
          case res of
            Left lexError -> print lexError
            Right tokens -> do parseresult <- TigerParser.parse tokens
                               case parseresult of
                                 Left  err -> print err
                                 Right program -> print program
