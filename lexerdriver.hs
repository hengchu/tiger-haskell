import Lexer
import System.Environment

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let res = tokenize file fileContent
          case res of
            Left perror -> print perror
            Right tokens -> print tokens
