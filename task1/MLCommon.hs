module MLCommon (
    leftTree,
    rightTree,
    parseHeader,
    parseStatement,
    annotate,
    classicAxiomList
) where
import Tree
import Text.Regex -- sorry
import qualified AttoParser as Parser
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Maybe

type Map = Map.Map

parseStatement = Parser.parseStatement


leftTree :: ParseTree -> ParseTree
leftTree (Binary _ l _) = l

rightTree :: ParseTree -> ParseTree
rightTree (Binary _ _ r) = r

headerDelims = mkRegex "(,)|(\\|-)"
parseHeader :: String -> ([(ParseTree, Int)], ParseTree)
parseHeader s = let ss = (splitRegex headerDelims s) in
    ((build 1 (init ss)), (parseStatement (last ss)))
    
{-# INLINE build #-}
build :: Int -> [String] -> [(ParseTree, Int)]
build _ [""] = [] -- god damn the case of "|-ANY"
build _ [] = []
build i (s:rem) = (parseStatement s, i):(build (i + 1) rem)


mppred ptm ((Binary Arrow _ rt), _) = (ptm == rt)
mppred _ _ = False

modusPonens :: ParseTree -> (Map ParseTree Int) -> [(ParseTree, Int)] -> Maybe (Int, Int)
modusPonens t stm listcache = 
    let fab = List.filter (mppred t) listcache
        lu x = Map.lookup (leftTree $ fst x) stm
    in  fmap fromJust $ List.find (isJust) $ List.map (\x -> 
        if (isJust (lu x)) 
        then Just (snd x, fromJust (lu x)) 
        else Nothing) fab

classicAxiomList :: [(ParseTree, Int)]
classicAxiomList = List.map (\t -> (parseStatement (fst t), snd t)) [
    ("A->B->A",1),
    ("(A->B)->(A->B->C)->(A->C)",2),
    ("(A&B)->A",3),
    ("(A&B)->B",4),
    ("A->B->(A&B)",5),
    ("A->(A|B)",6),
    ("B->(A|B)",7),
    ("(A->C)->(B->C)->(A|B)->C",8),
    ("(A->B)->(A->!B)->!A",9),
    ("!!A->A",10)
    ]

{- 
    annotate :
        args :
            axiom list
            assumption list
            proven map
            proven list - as per modusponens cache optimisation
            numeral
            statement (string)
        ret :
            annotated statement!
-}

{-# INLINE annotate #-}
annotate :: [(ParseTree, Int)] -> [(ParseTree, Int)]
        -> Map ParseTree Int -> [(ParseTree, Int)]
        -> Int -> String -> String
annotate ax as prm prl i s = 
    "(" ++ (show i) ++ ") " ++ (s) ++ (produceTail ax as prm prl s)
      
{-# INLINE produceTail #-}
produceTail ax as prm prl s =
    let 
    st = parseStatement s
    axm = List.find ((`satisfies` st) . fst) ax 
    in
    if (isJust axm) 
    then 
        let axf = fromJust axm in
        " (Сх. акс. " ++ (show (snd axf)) ++ ")"
    else
    let asm = List.find ((== st) . fst) as in
    if (isJust asm)
    then
        let asf = fromJust asm in
        " (Предположение " ++ (show (snd asf)) ++ ")"
    else
    let mpm = modusPonens st prm prl in
    if (isJust mpm)
    then
        let mpf = fromJust mpm in
        " (M.P. " ++ (show (fst mpf)) ++ ", " ++ (show (snd mpf)) ++ ")"
    else
        " (Не доказано)"

        
