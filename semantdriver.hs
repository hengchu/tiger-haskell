import qualified TigerSemant as TS
import qualified TigerSemTr  as TSt
import qualified TigerLexer  as TLex
import qualified TigerParser as TP
import qualified TigerGenSymLabTmp as TGSLT
import TigerGraph
import TigerFlow
import qualified TigerCodeGen as CG
import TigerITree
import TigerFrame
import TigerCanon
import TigerInterference
import Data.List
import System.Environment

isdata :: Frag -> Bool
isdata (DATA _ _) = True
isdata (PROC _ _ _) = False

isproc :: Frag -> Bool
isproc = not . isdata

trans :: Frag -> TGSLT.GenSymLabTmpState -> (String, [Stm], TGSLT.GenSymLabTmpState)
trans f state = if isdata f
                   then (prettyprintfrag f, [], state)
                   else let (stms, state') = canonicalize (procBody f) state
                            lab = procName f
                        in  (TGSLT.name lab ++ ":\n" ++ ((concat . intersperse "\n") $ map prettyprintstm stms), stms, state')

transfoldhelper frag (str, stms1, state) = let (str', stms2, state') = trans frag state
                                    in  (str++"\n"++str', stms1 ++ stms2, state')

codegenfoldhelper stm (instrs, state) = let (instrs', state') = CG.codegen stm state
                                        in  (instrs ++ instrs', state')

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
                                                               Right frags    -> do let (output, stms, state) = foldl (flip transfoldhelper) (file++"\n", [], gsltstate2) frags
                                                                                    let (instrs, state2) = foldl (flip codegenfoldhelper) ([], state) stms
                                                                                    let (flowgraph, nodes) = instrs2graph instrs
                                                                                    let (intergraph, _) = interferenceGraph flowgraph
                                                                                    let dotfile = graph2dotfile "flowgraph" $ graph intergraph
                                                                                    putStrLn dotfile
