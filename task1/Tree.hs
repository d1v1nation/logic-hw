module Tree (
    BToken(..),
    UToken(..),
    ParseTree(..),
    satisfies
) where
import qualified Data.Map.Strict as Map
import Control.Applicative
import Control.Monad.Identity (Identity)
import Data.Maybe


data UToken = Not deriving (Eq, Ord, Enum, Bounded)
data BToken = Arrow | And | Or deriving (Eq, Ord, Enum, Bounded)
data ParseTree = Binary BToken ParseTree ParseTree | Unary UToken ParseTree | Leaf String deriving (Eq, Ord)

instance Show UToken where
    show Not = "!"
    
instance Show BToken where
    show Arrow = "->"
    show And = "&"
    show Or = "|"
    
instance Show ParseTree where
    show (Binary t l r) = "(" ++ (show l) ++ (show t) ++ (show r) ++ ")"
    show (Unary t c) = (show t) ++ (show c)
    show (Leaf s) = s
    

{-|
    binding : produces a map of prop-vars of lh tree (axiom) to their bindings in rh tree
    returns Nothing if trees are not propvar-ismorophic
-}

binding :: ParseTree -> ParseTree -> Maybe (Map.Map String [ParseTree])
binding ax@(Binary at al ar) pp@(Binary pt pl pr) = if (at == pt)
    then ((Map.unionWith (++)) <$> (binding al pl) <*> (binding ar pr))
    else Nothing
binding ax@(Unary at al) pp@(Unary pt pl) = if (at == pt)
    then (binding al pl)
    else Nothing
binding ax@(Leaf id) pp = Just (Map.singleton id [pp])
binding _ _ = Nothing

{-|
    unbinding : folds a maybe-map into a single boolean telling of whether all binds are equal
-}

unbinding :: Maybe (Map.Map String [ParseTree]) -> Maybe Bool
unbinding = fmap ((and) . Map.elems . (Map.map (\l -> all (== (head l)) l)))

{-|
    satisfies : checks if rexpr satisfies lexpr axiom
-}

satisfies :: ParseTree -> ParseTree -> Bool
satisfies a s = let t = unbinding $ binding a s in
    if (isNothing t) then False else (fromJust t)
