module Tree2 (
    Token(..),
    AST(..),
    matchToken,
    matchStr,
    satisfies,
    satisfies',
    varSubst,
    binding,
    binding',
    isVar
) where
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Maybe
import Data.List

data Token =
          Arr
        | Disj
        | Conj
        | Not
        | Equals
        | Forall
        | Exists
        | Mul
        | Plus
        | Succ
        | Zero
        | Holder String
        | Var String
        | Fun String
        | Pred String
        | Error
            deriving (Ord, Eq)
        
data AST = AST Token [AST] deriving (Ord, Eq)

varSubst t w a@(AST at ae)
    -- ~ re-bound here - no substitutions past this point.
    | ((at == Forall) || (at == Exists)) && ((matchToken (ae !! 0)) == t) = a
    | (at == t) = (AST w (map (varSubst t w) ae))
    | otherwise = (AST at (map (varSubst t w) ae))

nameify i v = "_" ++ (show i) ++ (matchStr v)
matchStr (AST (Var s) _) = s
matchToken (AST t _) = t

-- ~ this eq works in O(n2), thanks predicates, real nice of you!
    
-- ~ treeEq :: M.Map Token Token -> AST -> AST -> Bool
-- ~ treeEq d t1@(AST t1t t1e) t2@(AST t2t t2e)
    -- ~ | ((t1t == Forall) || (t1t == Exists)) && (t1t == t2t) =
        -- ~ let (AST tok1 _) = (t1e !! 0)
            -- ~ (AST tok2 _) = (t2e !! 0)
            -- ~ h = (Holder $ '_':(show d))
        -- ~ in
            -- ~ (treeEq (M.insert tok2 tok1 d) (t1e !! 1) (t2e !! 1))
    -- ~ | (isVar t1t) && (isVar t2t) =
        -- ~ let r = fromMaybe t2t (M.lookup t2t d)
        -- ~ in
            -- ~ (t1t == r)
    -- ~ | otherwise =
        -- ~ (t1t == t2t) &&
        -- ~ (length t1e) == (length t2e) &&
        -- ~ (and $ map (uncurry (treeEq d)) (zip t1e t2e))
        
-- ~ treeLE :: M.Map Token Token -> AST -> AST -> Bool
-- ~ treeLE d t1@(AST t1t t1e) t2@(AST t2t t2e)
    -- ~ | ((t1t == Forall) || (t1t == Exists)) && (t1t == t2t) =
        -- ~ let (AST tok1 _) = (t1e !! 0)
            -- ~ (AST tok2 _) = (t2e !! 0)
            -- ~ h = (Holder $ '_':(show d))
        -- ~ in
            -- ~ (treeLE (M.insert tok2 tok1 d) (t1e !! 1) (t2e !! 1))
    -- ~ | (isVar t1t) && (isVar t2t) =
        -- ~ let r = fromMaybe t2t (M.lookup t2t d)
        -- ~ in
            -- ~ (t1t <= r)
    -- ~ | otherwise =
        -- ~ (t1t <= t2t) &&
        -- ~ ((length t1e) < (length t2e)) || 
        -- ~ (((length t1e) <= (length t2e)) && 
        -- ~ (and $ map (uncurry (treeEq d)) (zip t1e t2e)))

isVar (Var _) = True
isVar _ = False


instance Show Token where
    show Arr = "->"
    show Disj = "|"
    show Conj = "&"
    show Not = "!"
    show Equals = "="
    show Forall = "@"
    show Exists = "?"
    show Mul = "*"
    show Plus = "+"
    show Succ = "'"
    show Zero = "0"
    show (Holder s) = "<" ++ s ++ ">"
    show (Var s) = s
    show (Fun s) = s
    show (Pred s) = s
    show Error = "ERR"
    
instance Show AST where
    show (AST t [l, r])
        | (t `elem` [Arr, Conj, Disj, Mul, Plus, Equals]) =
            "(" ++ (show l) ++ ")" ++ (show t) ++ "(" ++ (show r) ++ ")"
    show (AST Not [e]) =
            "!(" ++ (show e) ++ ")"
    show (AST t [v, e])
        | (t `elem` [Forall, Exists]) =
            (show t) ++ (show v) ++ "(" ++ (show e) ++ ")"
    show (AST Succ [e]) =
            "(" ++ (show e) ++ ")'"
    show (AST Zero _) =
            "0"
    show (AST (Holder t) _) =
            (t)
    show (AST (Var t) _) =
            (t)
    show (AST (Fun n) es) =
            (n) ++ "(" ++ (intercalate "," (map show es)) ++ ")"
    show (AST (Pred n) es) =
            (n) ++ (if (null es) then "" else "(" ++ (intercalate "," (map show es)) ++ ")")
    show (AST Error _) =
            "ERROR"
    

binding = binding' 0

binding' :: Int -> AST -> AST -> Maybe (M.Map String [AST])
binding' d axi@(AST (Holder s) r) stat =
    Just (M.singleton s [stat])
    
-- ~ quantifed case
-- ~ seeing as how (\q)xP(x) is same as (\q)yP(y)
-- ~ substitute bound by this quantifer xs as <_> and then match against ys
-- ~ if matches okay, assert that all <_> bindings are exactly y 
-- ~ and drop <_> binding from map
binding' d axi@(AST at ae) pr@(AST pt pe)
    | at == pt && (at == Forall || at == Exists) =
        let b = (binding' (d + 1) (varSubst (matchToken (ae !! 0)) (Holder (nameify d (ae !! 0))) (ae !! 1)) (pe !! 1))
        in
            if (isJust b)
            then if ((fromMaybe True $ fmap (Prelude.all (== (pe !! 0))) (M.lookup (nameify d (ae !! 0)) (fromJust b))))
                then Just (M.delete (nameify d (ae !! 0)) (fromJust b))
                else Nothing
            else Nothing

-- ~ generic case
binding' d a@(AST at ae) b@(AST bt be) =
    if (at == bt && (length ae) == (length be))
    then foldl (liftA2 $ M.unionWith (++)) (Just (M.empty)) (map (uncurry (binding' (d + 1))) (zip ae be))
    else Nothing
    
unbinding :: Maybe (M.Map String [AST]) -> Maybe Bool
unbinding = fmap $ (and) . (map (\l -> all (== head l) l)) . M.elems

satisfies :: AST -> AST -> Bool
satisfies ax pr = satisfies' $ binding ax pr

satisfies' = (fromMaybe False) . unbinding
