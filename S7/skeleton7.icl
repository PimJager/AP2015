module skeleton7

from iTasks import always, hasValue, :: TaskValue(..), :: Task, :: Stability, :: TaskCont(..), :: Action, updateInformation, viewInformation, class descr, instance descr String, :: UpdateOption, :: ViewOption(..), -||-, -||, ||-, startEngine, class Publishable, >>*, class TFunctor, instance TFunctor Task, class TApplicative, instance TApplicative Task, instance Publishable Task, Void
import Data.Tuple, StdClass, StdList, iTasks._Framework.Generic, Text.JSON, Data.Functor, Control.Applicative, Control.Monad, Data.Map, Data.Either, Data.Functor
import qualified iTasks as I
from iTasks import -||
import qualified Text as T
import qualified Data.List as List
from Data.Func import $
from Text import class Text, instance Text String
from StdFunc import o

e = Insert New (Oper New +. (Union (Integer 7) (Size (Integer 9))))

:: Expression
	= New
	| Insert		 Element Set
	| Delete		 Element Set
	| Variable		 Ident
	| Union			 Set 	Set
	| Difference  	 Set 	Set
	| Intersection	 Set 	Set
	| Integer		 Int
	| Size			 Set
	| Oper			 Element Op Element
	| (=.)  infixl 2 Ident Expression

:: Op 		=	+. | -. | *.
:: Set 		:== Expression
:: Element	:== Expression
:: Ident	:== String
:: Val 		= 	I Int | S [Int]
derive class iTask Expression, Op, Val

// === State
:: State 	:== Map Ident Val

// === semantics
// Datatype ===
:: Sem a	= Sem (State -> (Either String a, State))

runSem :: (Sem a) State -> (Either String a, State)
runSem (Sem f) st = f st

instance Functor Sem where
	fmap f s 	= Sem $ \st -> let (r, st`) = runSem s st in (f <$> r, st`)

instance Applicative Sem where
	pure a 		= Sem $ \st -> (pure a, st)
	(<*>) sf s 	= ap sf s

instance Monad Sem where
	bind s f 	= Sem $ \st -> let (r, st`) = runSem s st in case r of
		Left e 	= (Left e, st`)
		Right a	= runSem (f a) st`

store :: Ident Val -> Sem Val
store k v 	= Sem $ \st -> (pure v, put k v st)

read :: Ident -> Sem Val
read k 		= Sem $ \st -> case get k st of
	Just v 	= (pure v, st)
	Nothing = runSem (fail $ 'T'.concat ["Unknown identifier: ", k]) st

fail :: String -> Sem a
fail e 		= Sem $ \st -> (Left e, st) 

// Evaluation ===
evalS :: Expression -> Sem [Int]
evalS e = eval e >>= \rs -> case rs of
	I i 	= fail $ 'T'.concat["Expected set but found element: ", toString i]
	S rs` 	= return rs`
evalI :: Expression -> Sem Int
evalI e = eval e >>= \r -> case r of 
	S s 	= fail $ 'T'.concat["Expected element but found set: ", toString s]
	I r` 	= return r`

eval :: Expression -> Sem Val
eval New 					= return $ S []
eval (Insert e s)			= evalI e >>= \e` -> evalS s >>= \s` -> return $ S [e`:s`]
eval (Delete e s) 			= evalI e >>= \e` -> evalS s >>= \s` -> return $ S $ filter ((<>)e`) s`
eval (Variable k)			= read k
eval (Union s1 s2)			= opS s1 s2 'List'.union
eval (Difference s1 s2)		= opS s1 s2 'List'.difference
eval (Intersection s1 s2) 	= opS s1 s2 'List'.intersect
eval (Integer i)			= return $ I i
eval (Size s)				= evalS s >>= \s` -> return $ I $ length s`
eval (Oper e1 o e2)			= evalI e1 >>= \e1` -> evalI e2 >>= \e2` -> return $ I $ (op o) e1` e2`
eval (=. k e) 				= eval e >>= \e_ -> store k e_

opS :: Expression Expression ([Int] [Int] -> [Int]) -> Sem Val		
opS s1 s2 f = evalS s1 >>= \s1` -> evalS s2 >>= \s2` -> return $ S $ f s1` s2`
op :: Op -> (Int -> Int -> Int)
op (+.) = +
op (-.) = -
op (*.) = *

evalExpression :: Expression State -> Either String Val
evalExpression expr state = let (r, _) = runExpression expr state in r

evalExpression` :: Expression -> Either String Val
evalExpression` e = evalExpression e newMap

runExpression :: Expression State -> (Either String Val, State)
runExpression expr state = runSem (eval expr) state

// === Printing
print e = 'T'.concat $ print` e []

print` :: Expression [String] -> [String]
print` New 					c 	= ["∅":c]
print` (Insert e s)			c 	= print` e [":" : print` s c]
print` (Delete e s) 		c 	= ["{" : print` e ["}∖(" : print` s [")" : c]]]
print` (Variable k) 		c 	= [k:c]
print` (Union s1 s2) 		c   = printOpS s1 s2 "∪" c
print` (Difference s1 s2)	c 	= printOpS s1 s2 "∖" c
print` (Intersection s1 s2) c 	= printOpS s1 s2 "∩" c
print` (Integer i) 			c 	= [toString i : c]
print` (Size s) 			c 	= ["|" : print` s ["|" : c]]
print` (Oper e1 op e2) 		c 	= ["(" : print` e1 [(printOp op) : print` e2 [")" : c]]]
print` (=. k e) 			c 	= [k : ["=" : print` e c]]

printOpS :: Expression Expression String [String] -> [String]
printOpS e1 e2 f c = ["(" : print` e1 [")" : [f : ["(" : print` e2 [")" : c]]]]]

printOp :: Op -> String
printOp (+.) = "+"
printOp (-.) = "-"
printOp (*.) = "*"

// === simulation
(>>>=)     :== 'I'.tbind
(>>>|) a b :== 'I'.tbind a (\_ -> b)
treturn    :== 'I'.return
ActionOk   :== 'I'.ActionOk
ActionQuit :== 'I'.ActionQuit
ActionNew  :== 'I'.ActionNew

:: IState :== (Expression, State, Either String Val)
emptyISstate = (New, newMap, Left "no result yet")

taskState :: 'I'.ReadWriteShared IState IState
taskState = 'I'.sharedStore "state" emptyISstate

views = 	'I'.viewSharedInformation "Expression:" [ViewWith (\(e,_,_) -> print e)] taskState
		-||	'I'.viewSharedInformation "Last result:" [ViewWith (\(_,_,r) -> r)] taskState  
		-||	'I'.viewSharedInformation "State:" [ViewWith (\(_,s,_) -> s)] taskState

mainTask :: (Task IState)
mainTask = ('I'.forever (
				'I'.updateSharedInformation "Expression" [] taskState
				>>* ['I'.OnAction 'I'.ActionDelete delete
					,'I'.OnAction ('I'.Action "Eval" []) evaluate
					]
			))
			-|| views
			>>* ['I'.OnAction 'I'.ActionFinish ('I'.always $ 'I'.get taskState)]
			where
				delete 	= 'I'.always ('I'.upd (\_-> emptyISstate) taskState)
				evaluate= 'I'.always ('I'.upd (\(e,s,_) -> 
								let (res, s`) = runExpression e s in
								(e,s`,res)
									  ) taskState )


Start world = startEngine mainTask world