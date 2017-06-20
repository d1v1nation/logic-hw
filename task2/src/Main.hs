module Main where
import Tree2
import Parser2
import Axiomatic2
import MetaRules
import Proof
import Common
import Transform
import System.IO
import System.Environment
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.List as List

main = do
    args <- getArgs
    if (isNothing $ args !? 0)
    then
        hPutStrLn stderr "Specify mode: a for annotation, t for transformation"
    else
        let target = case (args !! 0) of
                            "a" -> annotateInvoke
                            "t" -> transformInvoke
                            _ -> error $ "Invalid mode"
        in do
            hIn <- fromMaybe (return stdin) $ fmap (\t -> openFile t ReadMode) (args !? 1)
            hOut <- fromMaybe (return stdout) $ fmap (\t -> openFile t WriteMode) (args !? 2)
            head <- hGetLine hIn
            target head hIn hOut
            hClose hIn
            hClose hOut
            return ()
    
transformInvoke h hIn hOut =
    let ph = parseHeader h
        tass = (fst $ ph)
        thyp = fst $ (last . fst $ ph)
        tst = (AST Arr [thyp, snd ph])
        ths = (List.intercalate "," (map (show . fst) (init tass))) ++ "|-" ++ (show tst)
    in do
        hPutStrLn hOut ths
        transformInput hIn hOut (Map.empty) 1 [] (tass) [] thyp
    
annotateInvoke h hIn hOut = do
    hPutStrLn hOut h
    annotateInput hIn hOut (Map.empty) 1 [] ((fst $ parseHeader h)) []
    
annotateInput hIn hOut sts it listcache ass errors = do
    end <- hIsEOF hIn
    if (end) then
        if (null errors)
        then
            (hPutStrLn hOut "Доказательство корректно.")
        else do
            hPutStrLn hOut "Доказательство некорректно:"
            mapM (hPutStrLn hOut) $ map (\p -> "(" ++ (show $ fst p) ++ ") " ++ (show $ snd p)) errors
            return ()
    else do
        line <- hGetLine hIn
        let st = parseStatement line
            p = prove axioms ass sts listcache st
            nm = (Map.insert st it sts)
            nl = (st:listcache)
            ne = if (not $ isOkay p) then (p, it):errors else errors
         in do
            hPutStrLn hOut $ annotate it line p
            -- ~ hFlush hAnnot
            -- ~ writeTransformed hTr sts listcache p st
            annotateInput hIn hOut nm (it + 1) nl ass ne
                        
{-# INLINE annotate #-}
annotate i line p =
    "(" ++ (show i) ++ ") " ++ (line) ++ (show $ p)
        
-- ~ possilby inline as well
prove :: [(AST, Int)] -> [(AST, Int)] -> (Map.Map AST Int) -> [AST] -> AST -> Proof
prove ax as prm prl st
    | (isJust axm) = Axiom (snd $ fromJust axm)
    | (isJust asm) = Assumption (fromJust asm)
    | not $ isNone c11 = c11
    | not $ isNone c12 = c12
    | not $ isNone c19 = c19
    | not $ isNone rmp = rmp
    | not $ isNone rm1 = rm1
    | not $ isNone rm2 = rm2
    | otherwise = None
        where
            c11 = checkEleventh st
            c12 = checkTwelfth st
            c19 = checkInduction st
            axm = List.find ((`satisfies` st) . fst) ax
            asm = List.find ((== st) . fst) as
            rmp = upholdsMP prm prl st
            rm1 = upholdsRule1 prm prl st
            rm2 = upholdsRule2 prm prl st

transformInput hIn hOut sts it listcache ass errors hyp = do
    end <- hIsEOF hIn
    if (end) then return ()
    else do
        line <- hGetLine hIn
        let st = parseStatement line
            p = prove axioms ass sts listcache st
            nm = (Map.insert st it sts)
            nl = (st:listcache)
            ne = if (not $ isOkay p) then (it, p):errors else errors
            trn = transform p hyp st it
         in do
            if (isOK trn)
            then do
                (mapM (hPutStrLn hOut) $ map show $ (\(OK a) -> a) $ transform p hyp st it)
                (transformInput hIn hOut nm (it + 1) nl ass ne hyp)
            else if (isPropagated trn)
            then let (i, c) = fromPropagated trn
                in 
                    hPutStrLn hOut $ "Доказательство некорректно со строки " ++ (show i) ++ " : " ++ (show c)
            else if (isErrFreeIn trn)
            then let (i, t, e) = fromErrFreeIn trn
                in 
                    hPutStrLn hOut $ "Доказательство некорректно со строки " ++ (show i) ++ " : используется правило с переменной " ++ (show t) ++ ", входящей свободно в предположение " ++ (show hyp) ++ "."
            else
                return ()

