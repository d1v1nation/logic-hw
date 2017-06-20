module Parser2 (
    parseStatement,
    parseHeader
) where
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict as Map
import Data.Text
import Data.Char
import Control.Applicative
import Control.Monad.Identity (Identity)
import Data.Maybe
import Tree2

chainl1' acc p s = (do
    sep <- s
    n <- p
    chainl1' (sep acc n) p s)
    <|> (return acc)

chainl1 p s = do
    left <- p
    chainl1' left p s
    
chainr1 p s = do
    left <- p
    right <- (do 
        con <- s
        part <- chainr1 p s
        return (\x -> con x part)
        ) <|> (return id)
    return (right left)
    
collect1 it sep = do
    c <- it
    r <- (do
        cons <- sep
        rest <- collect1 it sep
        return (\x -> cons x rest)
        ) <|> (return (:[]))
    return (r c)
    
zero = do 
    Atto.char '0'
    return (AST Zero [])
    
lc = Atto.satisfy isLower
    where isLower c = c >= 'a' && c <= 'z'
    
uc = Atto.satisfy isUpper
    where isUpper c = c >= 'A' && c <= 'Z'
    
space = Atto.skipWhile isSpace
    
comma = Atto.char ',' >> space
    
commaCons = comma >> return (:)

chainmaker s con = (Atto.string (pack s)) >> return con


nameWith ca = do
    l <- ca
    m <- Atto.many' $ Atto.digit <|> ca
    return (l:m)
    
variable = do
    s <- nameWith lc
    return (AST (Var s) [])
    
function = do
    s <- nameWith lc
    Atto.char '('
    args <- (collect1 term commaCons) <|> (return [])
    Atto.char ')'
    return (AST (Fun s) args)
    
succwrap expr = (do
    Atto.char '\''
    succwrap (AST Succ (expr:[]))
    ) <|> (return expr)
    
predicate = do
    l <- nameWith uc
    args <- (do
        Atto.char '('
        actualArgs <- (collect1 term commaCons) <|> (return [])
        Atto.char ')'
        return actualArgs
        ) <|> (return [])
    return (AST (Pred l) args)

parensWith ret = do
    Atto.char '('
    r <- ret
    Atto.char ')'
    return r

notP = do
    Atto.char '!'
    i <- unary
    return (AST Not [i])
    
quantifier sym cons = do
    Atto.char sym
    v <- variable <|> (bind Holder)
    e <- unary
    return (cons (v:e:[]))
    
eq = do
    l <- plus
    Atto.char '='
    r <- plus
    return (AST (Equals) (l:r:[]))
    
bind with = do
    Atto.char '<'
    s <- Atto.many' $ Atto.digit <|> Atto.char '_'
    Atto.char '>'
    return (AST (with s) [])
  
term :: Atto.Parser AST
term = plus

primitive = (bind Holder) <|> function <|> variable <|> zero <|> (parensWith term)
leSucc = primitive >>= succwrap
mul = leSucc `chainl1` (chainmaker "*" (\ l r -> AST Mul (l:r:[])))
plus = mul `chainl1` (chainmaker "+" (\ l r -> AST Plus (l:r:[])))
unary = predicate <|> eq <|> notP <|> (parensWith expr) 
    <|> (quantifier '@' (AST Forall))
    <|> (quantifier '?' (AST Exists))
    <|> (bind Holder)
conj = unary `chainl1` (chainmaker "&" (\ l r -> AST Conj (l:r:[])))
disj = conj `chainl1` (chainmaker "|" (\ l r -> AST Disj (l:r:[])))
arr = disj `chainr1` (chainmaker "->" (\ l r -> AST Arr (l:r:[])))
    
expr = arr

header = do
    stmts <- (collect1 expr commaCons) <|> (return [])
    Atto.char '|'
    Atto.char '-'
    target <- expr
    return (Prelude.zip stmts ([1..] :: [Int]), target)
    
runParse p s = Atto.feed (Atto.parse p (pack s)) (pack "")

fromDone def (Atto.Done _ t) = t
fromDone def _ = def

parseStatement = fromDone (AST Error []) . (runParse expr)
parseHeader = fromDone ([], (AST Error [])) . (runParse header)

