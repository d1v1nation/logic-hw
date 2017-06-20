module Common where
import Tree2
import Parser2
import Axiomatic2
import MetaRules
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

intuAxiomList = map (\t -> (parseStatement (fst t), snd t)) [
    ("<1>-><2>-><1>",1),
    ("(<1>-><2>)->(<1>-><2>-><3>)->(<1>-><3>)",2),
    ("(<1>&<2>)-><1>",3),
    ("(<1>&<2>)-><2>",4),
    ("<1>-><2>->(<1>&<2>)",5),
    ("<1>->(<1>|<2>)",6),
    ("<2>->(<1>|<2>)",7),
    ("(<1>-><3>)->(<2>-><3>)->(<1>|<2>)-><3>",8),
    ("(<1>-><2>)->(<1>->!<2>)->!<1>",9),
    ("!!(<1>)-><1>",10)
    ]
-- ~ 11 and 12 provided separately.

arithmAxiomList = map (\t -> (parseStatement (fst t), snd t)) [
    ("a=b->a'=b'", 13),
    ("a=b->a=c->b=c",14),
    ("a'=b'->a=b",15),
    ("!(a'=0)",16),
    ("a+b'=(a+b)'",17),
    ("a+0=a",18),
    ("a*0=0",19),
    ("a*b'=a*b+a",20)
    ]
    
totalAxiomList :: [(AST, Int)]
totalAxiomList = intuAxiomList ++ arithmAxiomList




(!?) [] _ = Nothing
(!?) l 0 = Just $ head l
(!?) l i = (tail l) !? (i - 1)

axioms = totalAxiomList
