module skeleton8

import StdList, StdInt, StdMisc, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Void,
    Data.Either
import qualified iTasks
import qualified Text
from Text import class Text, instance Text String
from Data.Func import $
from StdFunc import o
from StdTuple import fst
from Data.Map import :: Map, put, get, newMap
from Data.List import union, removeMember, instance Functor []
import qualified Data.List as List
import qualified Text as T
from Text import class Text, instance Text String

/*
 * The store is a map from Ident (String) to Val. This allows for identifying the type
 * of the tetrieved value easily (unlike Dynamic) while also allowing for easy expantion
 * of the types of values possible to be stored (unlike storing a tuple of maps) by
 * simply extending the store type. 
 */

:: Val = I Int | S [Int] | B Bool
class val a where
    val :: a -> Val
instance val Int where val i = I i
instance val [Int] where val is = S is
instance val Bool where val b = B b
class unval a where
    unval :: Val -> a
//Note: Unvalling of wrong type is caugt by clean typesystem
instance unval Int where unval (I i) = i
instance unval [Int] where unval (S is) = is
instance unval Bool where unval (B b) = b


:: Ident :== String
:: Store :== Map Ident Val 

// === semantics
:: Sem a    = Sem (Store -> (Either String a, Store))

runSem :: (Sem a) Store -> (Either String a, Store)
runSem (Sem f) st = f st

instance Functor Sem where
    fmap f s    = Sem $ \st -> let (r, st`) = runSem s st in (f <$> r, st`)

instance Applicative Sem where
    pure a      = Sem $ \st -> (pure a, st)
    (<*>) sf s  = ap sf s

instance Monad Sem where
    bind s f    = Sem $ \st -> let (r, st`) = runSem s st in case r of
        Left e  = (Left e, st`)
        Right a = runSem (f a) st`

store :: Ident Val -> Sem Val
store k v   = Sem $ \st -> (pure v, put k v st)

read :: Ident -> Sem Val
read k      = Sem $ \st -> case get k st of
    Just v  = (pure v, st)
    Nothing = runSem (fail $ 'T'.concat ["Unknown identifier: ", k]) st

fail :: String -> Sem a
fail e      = Sem $ \st -> (Left e, st) 

:: Elem :== Sem Int
:: Set  :== Sem [Int] 

eval :: (Sem a) -> Either String a 
//note, does not unpack either on purpose since toString [1,2,3] == " ", which is not very useful
eval e = let (r,_) = runSem e newMap in r

// ================== 

int :: Int -> Elem
int i   = return i

set :: [Int] -> Set
set is  = return is

bool :: Bool -> Sem Bool
bool b = return b

new :: Set
new     = return []

size :: Set -> Elem
size s = Sem $ \st -> let (Right s`, st`) = runSem s st in (return $ length s`, st`)

// ==== Operators =====
//On Integers
instance + Elem where
    (+) e1 e2 = liftM2 (+) e1 e2
instance * Elem where
    (*) e1 e2 = liftM2 (*) e1 e2
instance - Elem where
    (-) e1 e2 = liftM2 (-) e1 e2

(==.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | == a
(==.) s1 s2 = liftM2 (==) s1 s2

(<.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | < a
(<.) s1 s2 = liftM2 (<) s1 s2

//On sets
instance + Set where
    (+) s1 s2 = union s1 s2
instance - Set where
    (-) s1 s2 = difference s1 s2

union :: Set Set -> Set
union s1 s2 = liftM2 'List'.union s1 s2
difference :: Set Set -> Set
difference s1 s2 = 'List'.difference <$> s1 <*> s2
intersection :: Set Set -> Set
intersection s1 s2 = 'List'.intersect <$> s1 <*> s2
insert :: Elem Set -> Set
insert e s = liftM2 (\a b -> [a:b]) e s
delete :: Elem Set -> Set
delete e s = 'List'.delete <$> e <*> s

//Storage and retrieval
var :: Ident -> Sem a | unval a
var k = unval <$> read k

(=.) infixl 2 :: Ident (Sem a) -> (Sem a) | val a
(=.) k e = e >>= \e` -> store k (val e`) >>| return e`

//Composition
(:.) infixl 1 :: (Sem a) (Sem b) -> (Sem b)
(:.) s1 s2 = s1 >>| s2

:: THEN = THEN
:: ELSE = ELSE
IF :: (Sem Bool) THEN (Sem a) ELSE (Sem a) -> Sem a
IF c _ t _ e = c >>= \c` -> if c` t e

::DO = DO
WHILE :: (Sem Bool) DO (Sem a) -> Sem Int
WHILE c _ b = helper c b 0
    where
        helper c b i = c >>= \c` -> if (c`) (b >>| helper c b (i+1)) (return i)

// examples

expr1 :: Elem
expr1 = int 2

expr2 :: Elem
expr2 = expr1 + expr1

expr3 :: Elem
expr3 = expr1 + expr1 * int 3


expr4 :: Set
expr4 = insert (int 3) new

x = "x"
y = "y"
z = "z"

//to make sure that state changes in the if-condition
//are bound in the then/else expressions
exprT = 
    x =. int 7 :.
    IF ( x =. int 6 :. bool True ) THEN
        (var x)
    ELSE 
        (int 5)

// test unvalling of wrong type
//caught by clean typesystem
/*exprU = 
    x =. bool False :.
    y =. var x + int 5 :.
    var y*/

expr5 :: Set
expr5 =
    x =. expr4 :.
    var x

expr6 :: Elem
expr6 =
    x =. insert (int 11) new :.
    x =. size (var x) :.
    var x

expr7 :: Set
expr7 =
    x =. insert (int 11) new :.
    y =. var x

expr8 :: Set
expr8 =
    x =. insert (int 11) new :.
    x =. insert (size (var x)) (var x) :.
    var x

expr9 :: Set
expr9 =
    x =. insert (int 0) new :.
    IF (size (var x) ==. int 0) THEN
        (x =. insert (int 0) (var x))
    ELSE
        (x =. delete (int 0) (var x)) :.
    var x

expr10 :: Set
expr10 =
	z =. int 7 :.
	x =. new :.
	x =. insert (var z) (var x) :.
	y =. union (var x) (var x) :.
	WHILE (size (var x) <. int 5) DO
		(x =. insert (size (var x)) (var x)) :.
	z =. difference (var x) (intersection (var x) (insert (var z) new))

Start = (map eval exprs, map eval expre)
    where
        exprs = [expr4, expr5, expr7, expr8, expr9, expr10]
        expre = [expr1, expr2, expr3, exprT, expr6]

