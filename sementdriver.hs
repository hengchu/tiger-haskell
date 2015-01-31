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
            Right tokens -> case TP.parse tokens of
                              Left parseError -> print parseError
                              Right (absyn, smap) -> do analysis <- TS.transProg smap absyn
                                                        case analysis of
                                                          Left sementerr -> print sementerr
                                                          Right _ -> print "Sementic analysis complete."
