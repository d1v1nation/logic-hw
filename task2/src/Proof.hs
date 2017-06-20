module Proof where
import Tree2

data Proof = 
      Axiom Int
    | Assumption (AST, Int)
    | MP (AST, Int) (AST, Int)
    | Rule1 (Token, AST, Int)
    | Rule2 (Token, AST, Int)
    | None
    | TermNotSubstitutable AST AST Token
    | VarFreeIn Token AST
    | VarFreeInSupp Token AST
    
isNone None = True
isNone _ = False

isOkay None = False
isOkay (TermNotSubstitutable _ _ _) = False
isOkay (VarFreeIn _ _) = False
isOkay (VarFreeInSupp _ _) = False
isOkay (_) = True

instance Show Proof where
    show (Axiom a) = " (Сх. акс. " ++ (show a) ++ ")"
    show (Assumption a) = " (Предположение " ++ (show $ snd a) ++ ")"
    show (MP a b) = " (M.P. " ++ (show $ snd a) ++ ", " ++ (show $ snd b) ++ ")"
    show (Rule1 (_, _, a)) = " (Правило вывода 1: " ++ (show a) ++  ")"
    show (Rule2 (_, _, a)) = " (Правило вывода 2: " ++ (show a) ++ ")"
    show None = " (Не доказано)"
    show (TermNotSubstitutable a1 a2 t) = " (Не доказано: терм " ++ (show a1)
            ++ " не свободен для подстановки в формулу " ++ (show a2) ++ " вместо переменной "
            ++ (show t) ++ ")"
    show (VarFreeIn t a) = " (Не доказано: переменная " ++ (show t)
            ++ " входит свободно в формулу " ++ (show a) ++ ")"
    show (VarFreeInSupp t a) = " (Не доказано: но вообще эта ошибка должна получаться только при перестройке)"
