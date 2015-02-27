-- Much of the code in this file is adapted from
-- the ML version at here: http://www.cs.princeton.edu/~appel/modern/ml/chap8/canon.sml

module TigerCanon
  (
    linearize
  , basicblocks
  , tracesched
  )
  where

import FrontEnd
import TigerTemp
import qualified Data.Map as Map
import Control.Monad.IO.Class

commute :: Stm -> Exp -> Bool
commute (EXP(CONST _)) _ = True
commute _ (NAME _) = True
commute _ (CONST _) = True
commute _ _ = False

nop :: Stm
nop = EXP(CONST 0)

infixl % -- combines two Stm, ignores nop
(%) :: Stm -> Stm -> Stm
s % (EXP(CONST _)) = s
(EXP(CONST _)) % s = s
a % b              = SEQ(a, b)

linearize :: Stm -> Frontend [Stm]
linearize stm =
  let

    reorder dofunc (exps, build) =
      let f ((e@(CALL _)):rest) =
            do t <- newTemp
               f $ ESEQ(MOVE(TEMP t, e), TEMP t):rest
          f (a:rest) =
            do (stm0, e) <- dofunc a
               (stm0', el) <- f rest
               if commute stm0' e
                  then return (stm0 % stm0', e:el)
                  else do t <- newTemp
                          return (stm0 % MOVE(TEMP t,e ) % stm0', (TEMP t):el)
          f [] = return (nop, [])
      in do (stm0, el) <- f exps
            return (stm0, build el)
    
    expl :: [Exp] -> ([Exp] -> Exp) -> Frontend (Stm, Exp)
    expl el f = reorder doexp (el, f)

    expl' :: [Exp] -> ([Exp] -> Stm) -> Frontend Stm
    expl' el f = do (stm0, s) <- reorder doexp (el, f)
                    return $ stm0 % s

    doexp :: Exp -> Frontend (Stm, Exp)
    doexp (BINOP(p, a, b)) =
      expl [a, b] $ \[l, r] -> BINOP(p, l, r)
    doexp (CVTOP(p, a, s1, s2)) =
      expl [a] $ \[arg] -> CVTOP(p, arg, s1, s2)
    doexp (MEM(a, sz)) =
      expl [a] $ \[arg] -> MEM(arg, sz)
    doexp (ESEQ(s, e)) =
      do s' <- dostm s
         (s'', expr) <- expl [e] $ \[e'] -> e'
         return (s' % s'', expr)
    doexp (CALL(e, el)) =
      expl (e:el) $ \(func:args) -> CALL(func, args)
    doexp e = expl [] $ \[] -> e

    dostm :: Stm -> Frontend Stm
    dostm (SEQ(a, b)) = do a' <- dostm a
                           b' <- dostm b
                           return $ a' % b'
    dostm (JUMP(e, labs)) = 
      expl' [e] $ \[e'] -> JUMP(e', labs)
    dostm (CJUMP(TEST(p, a, b), t, f)) =
      expl' [a, b] $ \[a', b'] -> CJUMP(TEST(p, a', b'), t, f)
    dostm (MOVE(TEMP t, CALL(e, el))) =
      expl' (e:el) $ \(func:args) -> MOVE(TEMP t, CALL(func, args))
    dostm (MOVE(TEMP t, b)) =
      expl' [b] $ \[src] -> MOVE(TEMP t, src)
    dostm (MOVE(MEM(e, sz), src)) =
      expl' [e, src] $ \[e', src'] -> MOVE(MEM(e', sz), src')
    dostm (MOVE(ESEQ(s, e), src)) =
      do s' <- dostm s
         src' <- dostm $ MOVE(e, src)
         return $ s' % src'
    dostm (MOVE(a, b)) =
      expl' [a, b] $ \[a', b'] -> MOVE(a', b')
    dostm (EXP(CALL(e, el))) =
      expl' (e:el) $ \(func:arg) -> EXP(CALL(func, arg))
    dostm (EXP e) =
      expl' [e] $ \[e'] -> EXP e'
    dostm s =
      expl' [] $ \[] -> s

    linear (SEQ(a, b)) l = linear a $ linear b l
    linear s l = s : l

  in do stm' <- dostm stm
        return $ linear stm' []

basicblocks :: [Stm] -> Frontend ([[Stm]], Label)
basicblocks stms0 =
  do done <- newLabel
     let blocks ((head@(LABEL _)):tail) blist =
           let next ((s@(JUMP _)):rest) thisblock =
                 endblock rest $ s:thisblock
               next ((s@(CJUMP _)):rest) thisblock =
                 endblock rest $ s:thisblock
               next (stms@((LABEL lab): _)) thisblock =
                 next ((JUMP(NAME lab, [lab])):stms) thisblock
               next (s:rest) thisblock =
                 next rest $ s:thisblock
               next [] thisblock = next [JUMP(NAME done, [done])] thisblock

               endblock more thisblock =
                 blocks more $ (reverse thisblock):blist
           in next tail [head]
         blocks [] blist = return $ reverse blist
         blocks stms blist =
           do newlab <- newLabel
              blocks ((LABEL newlab):stms) blist
     blks <- blocks stms0 []
     return (blks, done)

enterblock :: [Stm] -> Map.Map Label [Stm] -> Map.Map Label [Stm]
enterblock b@(LABEL lab:_) table = Map.insert lab b table
enterblock _ table = table

splitlast :: [a] -> ([a], a)
splitlast [x] = ([], x)
splitlast (x:xs) = let (tail, l) = splitlast xs in (x:tail, l)

trace table b@(LABEL lab:_) rest =
  let table1 = Map.insert lab [] table
  in  case splitlast b of
        (most, JUMP(NAME lab, _)) ->
          case Map.lookup lab table1 of
            (Just b'@(_:_)) -> do t <- trace table1 b' rest
                                  return $ t ++ most
            _ -> do next <- getnext table rest
                    return $ b ++ next
        (most, CJUMP(TEST(opr, x, y), t, f)) ->
          case (Map.lookup t table, Map.lookup f table) of
            (_, Just b'@(_:_)) -> do t <- trace table b' rest
                                     return $ b ++ t
            (Just b'@(_:_), _) -> do tc <- trace table b' rest
                                     let cjump = [CJUMP(TEST(notrel opr, x, y), f, t)]
                                     return $ most ++ cjump ++ tc
            _ -> do f' <- newLabel
                    let cjump = [CJUMP(TEST(opr, x, y), t, f'), LABEL f',
                                 JUMP(NAME f, [f])]
                    return $ most ++ cjump
        (most, JUMP _) -> do next <- getnext table rest
                             return $ b ++ next
          

getnext table (b@(LABEL lab:_):rest) =
  case Map.lookup lab table of
    Just (_:_) -> trace table b rest
    _ -> getnext table rest
getnext table [] = return []

tracesched :: ([[Stm]], Label) -> Frontend [Stm]
tracesched (blocks, done) =
  do n <- getnext (foldr enterblock Map.empty blocks) blocks
     return $ n ++ [LABEL done]
