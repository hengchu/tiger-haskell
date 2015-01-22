import TigerLexer
import System.Environment

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let res = scanner fileContent
          case res of
            Left perror -> print perror
            Right tokens -> print tokens
