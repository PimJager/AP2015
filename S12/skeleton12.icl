module skeleton12

import Data.Maybe
import Control.Monad
import StdInt, StdString, StdBool

class arith x where
	lit :: a -> x a | toString a
	(+.) infixl 6 :: (x a) (x a) -> x a | + a // integer addition, Boolean OR
	(*.) infixl 7 :: (x a) (x a) -> x a | * a // integer multiplication, Boolean AND
class store x where
	read  :: (x Int)
	write :: (x Int) -> x Int
class truth x where
	(XOR) infixr 3 :: (x Bool) (x Bool) -> x Bool
	-.    :: (x Bool) -> x Bool
class (=.=) infix 4 x :: (x a) (x a) -> x Bool | == a
class except x where
	throw :: (x a)
	try   :: (x a) (x a) -> x a

class aexpr x | arith, store, except, =.= x
class bexpr x | arith, truth, except, =.= x
class expr x  | aexpr, bexpr x

:: Step a = Step (State -> (Maybe a, State))
:: State :== Int

/*seven :: e Int | aexpr e
seven = lit 3 +. lit 4

throw1 :: e Int | expr e
throw1 = lit 3 +. throw

six :: e Int | expr e
six = write (lit 3) +. read

try1 :: e Int | expr e
try1 = try throw1 (lit 42)

loge :: e Bool | expr e
loge = lit True *. -. (lit True)

comp :: e Bool | expr e
comp = lit 1 =.= lit 2 XOR -. (-. (lit True))*/

Start = 0
