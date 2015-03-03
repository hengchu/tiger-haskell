import qualified TigerSemant as TS
import qualified TigerSemTr  as TSt
import qualified TigerLexer  as TLex
import qualified TigerParser as TP
import qualified TigerGenSymLabTmp as TGSLT
import TigerITree
import TigerFrame
import TigerCanon
import Data.List
import System.Environment

isdata :: Frag -> Bool
isdata (DATA _ _) = True
isdata (PROC _ _ _) = False

isproc :: Frag -> Bool
isproc = not . isdata

trans :: Frag -> TGSLT.GenSymLabTmpState -> (String, TGSLT.GenSymLabTmpState)
trans f state = if isdata f
                   then (prettyprintfrag f, state)
                   else let (stms, state') = canonicalize (procBody f) state
                            lab = procName f
                        in  (TGSLT.name lab ++ ":\n" ++ ((concat . intersperse "\n") $ map prettyprintstm stms), state')

transfoldhelper frag (str, state) = let (str', state') = trans frag state
                                    in  (str++"\n"++str', state')

main :: IO ()
main = do args <- getArgs
          let file = args !! 0
          fileContent <- readFile file
          let eithertoken = TLex.scanner fileContent
          case eithertoken of
            Left lexerr -> print lexerr
            Right toks -> do let ptoks = map TP.token2ptoken toks
                             let parseres = TP.runParser TP.parser file ptoks
                             case parseres of
                               Left parserr -> print parserr
                               Right (prog, gsltstate) -> do (semantres, gsltstate2) <- TSt.runSemTr (TS.transprog prog) gsltstate TSt.initialSemTrState
                                                             case semantres of
                                                               Left semanterr -> print semanterr
                                                               Right frags    -> do let (output, state) = foldr transfoldhelper (file++"\n", gsltstate2) frags
                                                                                    putStrLn output
