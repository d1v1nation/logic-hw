{-# LANGUAGE TupleSections #-}

module MetaRules (
    upholdsMP,
    upholdsRule1,
    upholdsRule2
) where
import Tree2
import Parser2
import Axiomatic2
import Proof
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe

mpFilter eq target@(AST Arr [_, rh]) = (eq == rh)
mpFilter _ _ = False

justFilter (a, b, _, _) = (isJust a) && (isJust b)
justLifter ((Just a), (Just b), c, d) = (a, b, c, d)

bindWithExprs (a, b, c, d) = MP (c, a) (d, b)

lh target@(AST Arr [l, _]) = l

upholdsMP :: M.Map AST Int -> [AST] -> AST -> Proof
upholdsMP m l e =
    let fl = filter (mpFilter e) l
        fbl = map (\t ->
            let left = M.lookup (lh t) m
                right = M.lookup t m
            in
                (left, right, (lh t), t)
            ) fl
        fr = L.find (justFilter) fbl
    in
        if (isJust fr)
        then (bindWithExprs . justLifter) $ fromJust fr
        else None

upholdsRule1 :: M.Map AST Int -> [AST] -> AST -> Proof
upholdsRule1 m l e@(AST Arr [el, er@(AST Forall [(AST var _), err])]) =
    let ee = (AST Arr [el, err])
        p = M.lookup ee m
    in
        if (isJust p)
        then
            if (not $ hasAsFree var el)
            then 
                Rule1 (var, ee, fromJust p)
            else
                var `VarFreeIn` el
        else None
upholdsRule1 _ _ _ = None

upholdsRule2 :: M.Map AST Int -> [AST] -> AST -> Proof
upholdsRule2 m l e@(AST Arr [el@(AST Exists [(AST var _), elr]), er]) =
    let ee = (AST Arr [elr, er])
        p = M.lookup ee m
    in
        if (isJust p)
        then
            if (not $ hasAsFree var er)
            then 
                Rule2 (var, ee, fromJust p)
            else
                var `VarFreeIn` er
        else None
upholdsRule2 _ _ _ = None
