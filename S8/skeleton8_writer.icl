module skeleton8_writer

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
from StdFunc import o
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Trans
from StdTuple import fst, snd

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
:: SemW a   :== WriterT [String] Sem a

runSem :: (Sem a) Store -> (Either String a, Store)
runSem (Sem f) st = f st

instance Functor Sem where
    fmap f s    = Sem $ \st -> let (r, st`) = runSem s st in (f <$> r, st`)

instance Applicative Sem where
    pure a      = Sem $ \st -> (pure a, st)
    (<*>) sf s  = ap sf s

instance Monad Sem where
    bind s f   = Sem $ \st -> let (r, st`) = runSem s st in case r of
        Left e  = (Left e, st`)
        Right a = runSem (f a) st`

store :: Ident Val -> SemW Val
store k v   = liftT $ Sem $ \st -> (pure v, put k v st)

read :: Ident -> SemW Val
read k = liftT (read` k)
    where
        read` :: Ident -> Sem Val
        read` k      = Sem $ \st -> case get k st of
            Just v  = (pure v, st)
            Nothing = runSem (fail $ 'T'.concat ["Unknown identifier: ", k]) st

copy :: SemW Store
copy  = liftT (Sem $ \st -> (pure st, st))

fail :: String -> Sem a
fail e      = Sem $ \st -> (Left e, st) 

:: Elem :== SemW Int
:: Set  :== SemW [Int] 
:: BL   :== SemW Bool

eval :: (SemW a) -> (Either String (a, [String]))
eval e = eval` e newMap
eval` e st = let s = runWriterT e in let (r,_) = runSem s st in r

print :: (SemW a) -> String
print e = print` e newMap
print` e st = let r = eval` e st in case r of
    (Left m) = 'T'.concat ["Error in expression: ", m]
    (Right (_, rep)) = 'T'.concat $ 'List'.intersperse " " rep

result :: (SemW a) -> a
result e = result` e newMap
result` e st = let r = eval` e st in case r of
    (Left m) = abort "No result"
    (Right (r,_)) = r

// ================== 

int :: Int -> Elem
int i   = tell [toString i] >>| return i


set :: [Int] -> Set
set is  =       tell ["[",printSet is,"]"] >>| return is

bool :: Bool -> BL
bool b = tell [toString b] >>| return b
true = bool True
false = bool False

new :: Set
new     = t "∅" >>| return []

size :: Set -> Elem
size s = t "length:" >>| s >>= \s` -> return $ length s`


// ==== Operators =====
//On Integers
instance + Elem where
    (+) e1 e2 = t "+" >>| liftM2 (+) e1 e2
instance * Elem where
    (*) e1 e2 = t "*" >>| liftM2 (*) e1 e2
instance - Elem where
    (-) e1 e2 = t "-" >>| liftM2 (-) e1 e2

(==.) infix 4 :: (SemW a) (SemW a) -> SemW Bool | == a
(==.) s1 s2 = t "==" >>| liftM2 (==) s1 s2

(<.) infix 4 :: (SemW a) (SemW a) -> SemW Bool | < a
(<.) s1 s2 = t "<" >>| liftM2 (<) s1 s2

//On sets
instance + Set where
    (+) s1 s2 = union s1 s2
instance - Set where
    (-) s1 s2 = difference s1 s2

union :: Set Set -> Set
union s1 s2 = t "∪" >>| liftM2 'List'.union s1 s2
difference :: Set Set -> Set
difference s1 s2 = t "\\" >>| 'List'.difference <$> s1 <*> s2
intersection :: Set Set -> Set
intersection s1 s2 = t "∩" >>| 'List'.intersect <$> s1 <*> s2
insert :: Elem Set -> Set
insert e s = t "insert" >>| liftM2 (\a b -> [a:b]) e s
delete :: Elem Set -> Set
delete e s = t "delete" >>| 'List'.delete <$> e <*> s

//Storage and retrieval
var :: Ident -> SemW a | unval a
var k = t k >>| (read k) >>= \v -> return $ unval v

(=.) infixl 2 :: Ident (SemW a) -> (SemW a) | val a
(=.) k e = tell [k,"="] >>| e >>= \e` -> store k (val e`) >>| return e`

//Composition
(:.) infixl 1 :: (SemW a) (SemW b) -> (SemW b)
(:.) s1 s2 = s1 >>| t ";\n" >>| s2


:: THEN = THEN
:: ELSE = ELSE
IF :: (SemW Bool) THEN (SemW a) ELSE (SemW a) -> SemW a
IF c _ th _ e = t "if" >>| c >>= \c` -> 
    if c` 
        (copy >>= \st -> 
            t "then" >>| th >>= \r -> t "neht" >>| tell ["else",print` e st,"esle"] >>| return r) 
        (copy >>= \st -> 
            tell ["then",print` th st,"neth"] >>| t "else" >>| e >>= \r -> t "esle" >>| return r)


::DO = DO
//For printing here we use quite a horrible hack. We execute c (which prints it) and depending
//on the value print or execute b. Afterwards we silence all executions of b and c
WHILE :: (SemW Bool) DO (SemW a) -> SemW Int
WHILE c _ b = t "while" >>| c >>= \c` -> if c` 
                    (t "do" >>| b >>| helper c b 2 >>= \i -> t "od" >>| return i) 
                    (copy >>= \st -> tell ["do", print` b st, "od"] >>| return 0)
    where
        helper :: (SemW Bool) (SemW a) Int -> SemW Int
        helper c b i = supress c >>= \c` -> if (c`) (supress b >>| helper c b (i+1)) (return i)
        supress x = pass (x >>= \x_ -> return (x_, (\_->mempty)))


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

//to make sure that state changes in the if-condition
//are bound in the then/else expressions
exprT = 
    x =. int 7 :.
    IF ( x =. int 6 :. true ) THEN
        (var x)
    ELSE 
        (int 5)

//test reading not stored vars
exprF :: Elem
exprF = 
    var x

// test unvalling of wrong type
//caught by clean typesystem
/*exprU = 
    x =. bool False :.
    y =. var x + int 5 :.
    var y*/

//simpel DO loop
exprD :: Elem
exprD = 
    x =. int 0 :.
    WHILE (var x <. int 3) DO
        (x =. var x + int 1) :.
    var x

/*
Start = (map eval exprs, map eval expre)
    where
        exprs = [expr4, expr5, expr7, expr8, expr9, expr10]
        expre = [expr1, expr2, expr3, exprT, expr6, exprT, exprF]
*/

Start = (print expr9, result expr9)

/** Helper functions for dealing with printing **/
printSet :: [Int] -> String
printSet is = 'T'.concat $ 'List'.intersperse "," $ map toString is

t s = tell [s]

//supress :: (SemW a) -> SemW a


/* ***
Control.Monad.Writer is actually wrong.... (monad implementation is incorrect)
therefore we redo it here
*** */


:: WriterT w m a = WriterT (m (a, w))

:: Writer w a :== WriterT w Identity a

instance Monad (WriterT w m) | Monad m & Monoid w where
  bind m k = WriterT $ runWriterT m >>= \(a, w) ->
              runWriterT (k a) >>= \(b, w`) ->
              return (b, mappend w w`)

/* ** are not even in Control.Monad.Writer ... ** */
instance Functor (WriterT w m) | Monad m & Monoid w where
    fmap f s    = s >>= return o f

instance Applicative (WriterT w m) | Monad m & Monoid w where
    pure a      = WriterT $ pure (a, mempty)
    (<*>) sf s  = ap sf s

instance MonadTrans (WriterT w) | Monoid w where
  liftT m = WriterT $ m >>= \a -> return (a, mempty)

runWriterT :: (WriterT a u:b c) -> u:(b (c,a))
runWriterT (WriterT w) = w

writer :: .(.(a,b) -> WriterT b .Identity a)
writer = WriterT o Identity

runWriter :: .((WriterT a .Identity b) -> (b,a))
runWriter = runIdentity o runWriterT

execWriter :: (WriterT a .Identity b) -> a
execWriter m = snd (runWriter m)

mapWriter :: u:((a,b) -> .(c,d)) -> v:((WriterT b .Identity a) -> WriterT d .Identity c), [v <= u]
mapWriter f = mapWriterT (Identity o f o runIdentity)

execWriterT :: .(WriterT a b c) -> b a | Monad b
execWriterT m = runWriterT m >>= \(_, w) -> return w

mapWriterT :: .(u:(a (b,c)) -> v:(d (e,f))) (WriterT c u:a b) -> WriterT f v:d e
mapWriterT f m = WriterT $ f (runWriterT m)

tell :: a -> .(WriterT a b Void) | Monad b
tell w = WriterT $ return (Void, w)

listen :: .(WriterT a b c) -> .(WriterT a b (c,a)) | Monad b
listen m = WriterT $ runWriterT m >>= \(a, w) ->
                     return ((a, w), w)

pass :: .(WriterT a b (c,a -> d)) -> .(WriterT d b c) | Monad b
pass m = WriterT $ runWriterT m >>= \((a, f), w) ->
                     return (a, f w)

listens :: (a -> b) .(WriterT a c d) -> WriterT a c (d,b) | Monad c & Monoid a
listens f m = listen m >>= \(a, w) -> return (a, f w)

censor :: (a -> b) .(WriterT a c d) -> .(WriterT b c d) | Monad c & Monoid a
censor f m = pass $ m >>= \a -> return (a, f)
