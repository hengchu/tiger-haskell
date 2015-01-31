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
            Right tokens -> (print res) >> (case TigerParser.parse tokens of
                               Left parseError -> print parseError
                               Right (pg, _) -> print pg)
