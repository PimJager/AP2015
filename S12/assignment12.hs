{-# LANGUAGE GADTs #-}

module Assignment12 where

import Prelude hiding (print, read)
import Data.Maybe
import Control.Monad
import Data.Functor
import Control.Applicative


-- Haskell has + and * in the Num typeclass, and defining a num instance for Bool seems rather
-- dirty. Since + and * are operations of a Ring we instead we define our own ring class. (The 
-- Ring instance from Data.Algebra for Bool is no good since it treats + as /=)
class Ring a where
    zero :: a
    (^+^) :: a -> a -> a
    neg :: a -> a
    one :: a
    (^*^) :: a -> a -> a

instance Ring Int where
    zero    = 0
    (^+^)   = (+)
    neg     = negate
    one     = 1
    (^*^)   = (*)

instance Ring Bool where
    zero    = False
    (^+^)   = (||)
    neg     = id 
    one     = True
    (^*^)   = (&&)

infixl 6 +. , :+
infixl 7 *. , :*
infixr 4 ^.
infix  4 =.= , :==

class Arith x where
    lit :: Show a => a -> x a 
    (+.) :: Ring a => x a -> x a -> x a
    (*.) :: Ring a => x a -> x a -> x a
class Store x where
    read :: x Int
    write :: x Int -> x Int
class Truth x where
    (^.) :: x Bool -> x Bool -> x Bool --XOR
    (-.) :: x Bool -> x Bool
class Equals x where
    (=.=) :: Eq a => x a -> x a -> x Bool
class Except x where
    throw   :: x a
    try     :: x a -> x a -> x a

class (Arith x, Store x, Except x, Equals x) => Aexpr x
class (Arith x, Truth x, Except x, Equals x) => Bexpr x
class (Aexpr x, Bexpr x) => Expr x

-- Printing

-- In haskell, Show is already a class in Prelude, so I call it Print (print is also a function
-- in Prelude, but Prelude.Show is needed for lit, and Prelude.print is not needed in this example)
newtype Print a = Print {print :: [String] -> [String]}

binOp :: String -> Print a -> Print b -> Print c
binOp op (Print x) (Print y) = Print $ \c -> x (op : y c)

instance Arith Print where
    lit a   = Print $ \c -> (show a : c)
    (+.)    = binOp "+."
    (*.)    = binOp "+."
instance Store Print where
    read             = Print $ \c -> ("read" : c)
    write (Print x)  = Print $ \c -> ("write" : x c)
instance Truth Print where
    (^.)             = binOp "XOR"
    (-.) (Print x)   = Print $ \c -> ("!" : x c)
instance Equals Print where
    (=.=)   = binOp "=.="
instance Except Print where
    throw   = Print $ \c -> ("fail" : c)
    try     = binOp "<|>"
instance Aexpr Print
instance Bexpr Print
instance Expr  Print

-- Evaluation

newtype Step a = Step {runStep :: State -> (Maybe a, State)}
type State = Int

instance Functor Step where
    fmap f step     = Step $ \s -> let (r, s') = runStep step s in (fmap f r, s')
instance Applicative Step where
    pure a          = Step $ \s -> (pure a, s)
    (<*>) sf s      = ap sf s
instance Monad Step where
    return          = pure
    (>>=) step f    = Step $ \s -> case runStep step s of
                        (Just a, s')    -> runStep (f a) s'
                        (Nothing, s')   -> (Nothing, s')
instance Alternative Step where
    empty           = Step $ \s -> (Nothing, s)
    (<|>) st1 st2   = Step $ \s -> case runStep st1 s of
                        (Just a, s')    -> (Just a, s')
                        (Nothing, s')   -> runStep st2 s'

put :: State -> Step State
put s   = Step $ \_ -> (pure s, s)
get :: Step State
get     = Step $ \s -> (pure s, s)

instance Arith Step where
    lit     = return
    (+.)    = liftM2 (^+^) 
    (*.)    = liftM2 (^*^) 
instance Store Step where
    read    = get
    write x = x >>= put
instance Truth Step where
    (^.)    = liftM2 (\a b -> (a || b) && (not (a && b)))
    (-.)    = liftM not
instance Equals Step where
    (=.=)   = liftM2 (==)
instance Except Step where
    throw   = empty
    try     = (<|>)
instance Aexpr Step
instance Bexpr Step
instance Expr  Step

seven :: Aexpr e => e Int
seven = lit 3 +. lit 4

throw1 :: Expr e => e Int 
throw1 = lit 3 +. throw

six :: Expr e => e Int
six = write (lit 3) +. read

try1 :: Expr e => e Int
try1 = try throw1 (lit 42)

loge :: Expr e => e Bool
loge = lit True *. (-.) (lit True)

comp :: Expr e => e Bool
comp = (lit 1 =.= lit 2) ^. (-.) ((-.) (lit True))

{-
typeFail :: Expr e => e Bool
typeFail = (lit True) =.= (lit (1::Int))
-}

-- GADTs 
data Expression a where
    Lit     :: a -> Expression a
    (:+)    :: Ring a => Expression a -> Expression a -> Expression a
    (:*)    :: Ring a => Expression a -> Expression a -> Expression a
    Read    :: Expression State
    Write   :: Expression State -> Expression State
    XOR     :: Expression Bool -> Expression Bool -> Expression Bool
    Not     :: Expression Bool -> Expression Bool
    (:==)   :: Eq a => Expression a -> Expression a -> Expression a
    Throw   :: Expression a
    Try     :: Expression a -> Expression a -> Expression a

class Print' a where
    print' :: a -> [String] -> [String]

instance Print' (Expr a) where
    print' _ _ = []