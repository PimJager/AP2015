module skeleton11

import StdList, StdInt, StdMisc, StdClass, StdEnum
import Control.Monad.State
import Control.Monad.Identity //needs to be explicitly imported even though it is only used by State
import Data.Functor
import Control.Applicative
import Control.Monad
from Data.Func import $
from StdFunc import o, id
import Data.List

:: Prog :== [Instr]

:: Instr = Write Expr
         | Atomic [Instr]

:: Expr = Int   Int
        | Plus  Expr Expr
        | Times Expr Expr
        | Read

:: Env :== State Int Int

//For some reason I can't get automatic deriving to work, I'm not quite sure on what the correct
//way in Clean would be and can't recover it from 7 weeks ago. 
//This didn't work:
//derive class == Expr, Instr
//error: "==" no instance available of type Instr
instance == Instr where
    (==) (Write e1) (Write e2)      = e1 == e2
    (==) (Atomic is1) (Atomic is2)  = both 
                                        (length is1 == length is2)
                                        (foldl both True $ zipWith (==) is1 is2)
    (==) _ _                        = False
instance == Expr where
    (==) (Int i1) (Int i2)                  = i1 == i2
    (==) (Plus e11 e21) (Plus e12 e22)      = both (e11 == e12) (e21 == e22)
    (==) (Times e11 e21) (Times e12 e22)    = both (e11 == e12) (e21 == e22)
    (==) Read Read                          = True
    (==) _ _                                = False 

both :: Bool Bool -> Bool //&& or And or and or /\ are not the and-operator in Clean?
both True True  = True
both _ _        = False

evalExpr :: Expr -> Env
evalExpr (Int i)        = put i >>| return i
evalExpr (Plus e1 e2)   = (+) <$> (evalExpr e1) <*> (evalExpr e2)
evalExpr (Times e1 e2)  = (*) <$> (evalExpr e1) <*> (evalExpr e2)
evalExpr (Read)         = gets id

evalInstr :: Instr -> Env
evalInstr (Write e)     = evalExpr e >>= \r -> put r >>| return r
evalInstr (Atomic is)   = mapM evalInstr is >>= return o head o reverse

evalProg :: Prog -> Env
evalProg [i]    = evalInstr i
evalProg [i:p]  = evalInstr i >>| evalProg p

runProg :: Prog -> Int
runProg p = evalState (evalProg p) 0

// Takes a list of programs (list of instructions and reorders them in any
// possible ordering of their instructions (that is instruction ordering within a single)
// program is preserved, however instructions of another program might be interleaved 
ordering :: [[a]] -> [[a]] | == a
ordering [p]    = [p]
ordering ps     = nub $ concatMap (\p -> ordering` p (delete p ps)) ps
// That a nub is needed here is ugly... the ordering` function produces duplicates
// and idealy it wouldn't do that
ordering` :: [a] [[a]] -> [[a]] | == a
ordering` [] ps = ordering ps
ordering` p ps  = map (\rest -> [head p : rest]) $ ordering [tail p : ps]

possibleResults :: [Prog] -> [Int]
possibleResults ps = map runProg $ ordering ps

prog0 = [ Write (Int 12)
        , Write (Plus Read (Int 1))
        ]
prog1 = [ Write (Times Read (Int 2)) ]
test0 = [ prog0 ]       // result should be [13]
test1 = [ prog0, prog1] // result should be [13, 25, 26] (or any permutation, the order doesn't matter(
prog2 = [ Atomic [ Write $ Int 5
                 , Write $ Plus Read Read
                 ]
        ]
test2 = [prog0, prog2] //results hould be [13, 10, 11]

Start = possibleResults test2


/**

* Semantics in Clean

* The advantages are that it is immediately possible to test if the semantics have the 
* intended effect (by executing the program). I personally also feel that semantics described using
* a functional language are easier to read than a description in mathematics.

* Possible disadvantages are that some people might find semantics described using a functional
* language less clear to read, or that they might not know the language at all, mathematics is a
* more universal language.
* Also semantics described using a language requires a thorough understanding of the semantics of 
* that language. For most audiences the semantics of the mathimetical notation will be more familiar
* than those of Clean.

* Therefore I would not model in a functional language in case your audience is not familiar with
* functional languages, as in this case mathematics is propably clearer for them.

*/
