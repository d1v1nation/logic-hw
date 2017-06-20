module Main where
import MLCommon
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.List as List
import System.IO
import System.Random
import Tree

produceImpure depth = do
    if (depth == 0)
    then ((randomRIO (0 :: Int, 4 :: Int))) >>= (\t -> (return (Leaf ('D':(show t)))))
    else do
        arity <- ((randomRIO (0 :: Int, 2 :: Int)))
        append <- ((randomRIO (0 :: Int, 4 :: Int)))
        case arity of
            0 -> (return $ Leaf $ 'A':(show append))
            1 -> do
                    c <- produceImpure (depth - 1)
                    return $ Unary Not c
            2 -> do
                    token <- ((randomRIO (0 :: Int, 2 :: Int)) >>= (return . (toEnum :: Int -> BToken)))
                    l <- produceImpure (depth - 1)
                    r <- produceImpure (depth - 1)
                    return $ Binary token l r
                    
retree :: ParseTree -> [(String, ParseTree)] -> ParseTree
retree (Leaf str) subs = 
    let lu = List.lookup str subs in
        if (isJust lu)
        then fromJust lu
        else error " what the shiii "
retree (Binary t l r) s = Binary t (retree l s) (retree r s)
retree (Unary t c) s = Unary t (retree c s)
                    
produceAxiomatic depth axs = do
    aVar <- produceImpure depth
    bVar <- produceImpure depth
    cVar <- produceImpure depth
    wh <- randomRIO (1 :: Int, length axs)
    return (retree (fst (axs !! (wh - 1))) [("A", aVar), ("B", bVar), ("C", cVar)])
    
produceMP sts sta recur depth axs = do
    if (recur == 0 || (length sta) == 0)
    then produceAxiomatic depth axs
    else let len = length sta in do
        at <- randomRIO (0, (length sta) - 1)
        let lt = leftTree $ sta !! (at) in
            if (isJust (Map.lookup lt sts))
            then (return $ rightTree $ sta !! (at))
            else produceMP sts sta (recur - 1) depth axs
            
            
isAT (Binary Arrow _ _) = True
isAT _ = False

produce hOut len stm stl sta depth = do
    if (len == 0)
    then return ()
    else do
        ns <- produceMP stm sta 10 depth classicAxiomList
        hPutStrLn hOut (show ns)
        tail <- produce hOut (len - 1) (Map.insert ns len stm) (ns:stl)
            (if (isAT ns) then (ns:sta) else (sta)) depth
        return ()
            
produceH = withFile "test.in" WriteMode (\hOut -> produce hOut 50000 (Map.empty) [] [] 4)
        
main = produceH
