module AttoParser (
    parseStatement
) where
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict as Map
import Data.Text
import Control.Applicative
import Control.Monad.Identity (Identity)
import Data.Maybe
import Tree

chainl1 :: Atto.Parser ParseTree -> Atto.Parser (ParseTree -> ParseTree -> ParseTree) -> Atto.Parser ParseTree
chainl1 p s = do
    left <- p
    right <- (do 
        con <- s
        part <- chainl1 p s
        return (\x -> con x part)
        ) <|> (return id)
    return (right left)
    

chainr1h :: ParseTree -> Atto.Parser (ParseTree -> ParseTree -> ParseTree) -> Atto.Parser ParseTree -> Atto.Parser ParseTree
chainr1h le s p = (do -- le as a qualified expression
    sep <- s
    rest <- chainr1 p s
    return (sep le rest)
    ) <|> (return le)
    
chainr1 p s = do
    start <- p
    help <- chainr1h start s p
    return help
        
variable :: Atto.Parser ParseTree
variable = do
    l <- Atto.letter
    m <- Atto.many' $ Atto.digit <|> Atto.letter
    return (Leaf (l:m))
    
parens :: Atto.Parser ParseTree
parens = do
    Atto.char '('
    inner <- arrC
    Atto.char ')'
    return inner
    
notP = do
    Atto.string (pack "!")
    x <- notP <|> parens <|> variable
    return (Unary Not x)
    
andP = do
    Atto.string (pack "&")
    return (Binary And)
    
orP = do
    Atto.string (pack "|")
    return (Binary Or)
    
arrP = do
    Atto.string (pack "->")
    return (Binary Arrow)

simpleC = notP <|> parens <|> variable
andC = simpleC `chainl1` andP
orC = andC `chainl1` orP
arrC = orC `chainr1` arrP

runParse s = Atto.feed (Atto.parse arrC (pack s)) (pack "")

fromDone :: (Atto.Result ParseTree) -> ParseTree
fromDone (Atto.Done _ t) = t
fromDone s = error $ show $ s

parseStatement = fromDone . runParse

    
    
