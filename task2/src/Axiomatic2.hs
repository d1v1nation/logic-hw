module Axiomatic2 where
import Tree2
import Parser2
import Proof
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

performSubst var with expr@(AST at ae)
    -- ~ re-bound here - no substitutions past this point.
    | ((at == Forall) || (at == Exists)) && ((ae !! 0) == (AST var [])) = expr
    | (at == var) = with
    | otherwise = (AST at (map (performSubst var with) ae))
    
hasAsFree = hasAsFree' S.empty

hasAsFree' set var expr@(AST at ae) 
    | (at == Forall) || (at == Exists) = hasAsFree' (S.insert (matchToken $ ae !! 0) set) var (ae !! 1)
    | (at == var) = if (at `S.notMember` set) then True else False
    | otherwise = or $ map (hasAsFree' set var) ae

    
asFreeSet = asFreeSet' S.empty

asFreeSet' bound expr@(AST at ae)
    -- ~ add binding
    | (at == Forall) || (at == Exists) = asFreeSet' (S.insert (matchToken $ ae !! 0) bound) (ae !! 1)
    -- ~ found free, can add
    | isVar at = if (at `S.notMember` bound) then (S.singleton at) else S.empty
    | otherwise = S.unions $ map (asFreeSet' bound) ae

isFreeToSubst :: AST -> Token -> AST -> Bool
isFreeToSubst with = isFreeToSubst' S.empty (asFreeSet with)
    
isFreeToSubst' boundset freeset var expr@(AST at ae)
    | (at == Forall) || (at == Exists) =
        let (AST v _) = (ae !! 0)
        in
            if (v == var)
            then True
            else isFreeToSubst' (S.insert v boundset) freeset var (ae !! 1)
    | at == var = S.null $ (S.intersection boundset freeset)
    | otherwise = and $ map (isFreeToSubst' boundset freeset var) ae
    
-- ~ 11: @xf -> f[x = t] assuming free xs are subst-able by ts
-- ~ take @xf, substitute all x for holders in f, then B I N D I N G
-- ~ also make sure we're able to subst
checkEleventh (AST Arr [(AST t e), right])
    | (t == Forall) =
        let (AST token _) = e !! 0
            hld = AST (Holder "_the11th") []
            rebuilt = performSubst token hld (e !! 1)
            bind = binding rebuilt right
            sub = fmap (head) (M.lookup "_the11th" (fromMaybe M.empty bind))
        -- ~ in (satisfies' bind) && (fromMaybe True $ fmap (\w -> isFreeToSubst w t (e !! 1)) sub)
        in if (satisfies' bind)
        then
            if (fromMaybe True $ fmap (\w -> isFreeToSubst w token (e !! 1)) sub)
            then (Axiom 11)
            else (TermNotSubstitutable (fromJust sub) (e !! 1) token)
        else
            None
    | otherwise = None
checkEleventh _ = None
    
checkTwelfth (AST Arr [left, (AST t e)])
    | (t == Exists) =
        let (AST token _) = e !! 0
            hld = AST (Holder "_the12th") []
            rebuilt = performSubst token hld (e !! 1)
            bind = binding rebuilt left
            sub = fmap (head) (M.lookup "_the12th" (fromMaybe M.empty bind))
        in if (satisfies' bind)
        then
            if (fromMaybe True $ fmap (\w -> isFreeToSubst w token (e !! 1)) sub)
            then (Axiom 12)
            else (TermNotSubstitutable (fromJust sub) (e !! 1) token)
        else
            None
    | otherwise = None
checkTwelfth _ = None

checkInduction (AST Arr [(AST Conj [ze, fa@(AST Forall [varExprA@(AST varA@(Var v) []), arrEx@(AST Arr [arrLeft, arrRight])])]), conclusion]) = -- (ze)&(@a(P(a)->P(a')))->P(a)
    let aLSubToaS = (performSubst varA (AST Succ [varExprA]) arrLeft)
        aLSubToaZ = (performSubst varA (AST Zero []) arrLeft)
    in
    if (aLSubToaZ == ze) && (aLSubToaS == arrRight) && (arrLeft == conclusion)
    then (Axiom 19)
    else None
checkInduction _ = None
    
