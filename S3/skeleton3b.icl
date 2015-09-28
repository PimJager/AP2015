module skeleton3b

/*
	Advanced Programming.
	Skeleton for exercise 3.3 and 3.4.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

import StdEnv, StdGeneric, StdMaybe, GenEq

//------------------ show --------------
generic show_ a :: a [String] -> [String]

show a = show_{|*|} a []

//------------------ parse --------------

:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

//------------------ some data types --------------

:: T		= C
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)

//------------------ general useful --------------

instance + String where (+) s t = s+++t
derive bimap Maybe, []

//------------------ to test if parse and show work properly --------------

test :: t -> Bool | gEq{|*|}, show_{|*|}, parse{|*|} t
test x
	= case parse{|*|} (show x) of
		Just (y,[])	= x === y
		_			= False

/**************** End Prelude, add all new code below this line *************************/

/*************** SHOW ************/
show_{|Int|}  i c = [toString i:c]
show_{|Bool|} b c = [toString b:c]
show_{|OBJECT|} f (OBJECT o) c 		= f o c
show_{|UNIT|} _ c 					= c
show_{|PAIR|} fa fb (PAIR a b) c 	= fa a (fb b c)
show_{|EITHER|} fa _ (LEFT a) c		= fa a c
show_{|EITHER|} _ fb (RIGHT b) c 	= fb b c
show_{|CONS of {gcd_name, gcd_arity}|} f (CONS o) c 
	| gcd_arity == 0	= [gcd_name:f o c]  
	| otherwise 		= ["(":gcd_name:f o [")":c]]

derive show_ T, Color, Tree, [] //, (,)

/*************** PARSE ************/

parse{|Int|}  [i:r] 	  	= Just (toInt i, r)
parse{|Bool|} ["True" :r] 	= Just (True ,r)
parse{|Bool|} ["False":r] 	= Just (False,r)
parse{|Bool|} _ 			= Nothing
parse{|OBJECT|} f r 		= OBJECT <$> f r 
parse{|UNIT|} r 			= Just (UNIT, r)
parse{|PAIR|} fa fb r 		= case fa r of
	Just (res, rest) 		= (PAIR res) <$> fb rest
	_						= Nothing
parse{|EITHER|} fa fb r 	= case fa r of
		Nothing 				= RIGHT <$> fb r
		result 					= LEFT <$> result
parse{|CONS of {gcd_name, gcd_arity}|} f r
	| gcd_arity == 0 	= case r of
		[gcd_name:c] 		= CONS <$> f c
		otherwise 			= Nothing
	| otherwise 		= case r of
		["(":gcd_name:x] 	= case f x of
			Just (res, [")":rest])	= Just (CONS res, rest)
			otherwise 				= Nothing
		otherwise 			= Nothing

derive parse T, Color, Tree, [] //, (,)

/********** fmap for Result  *****/

(<$>) infixl 4 :: (a -> b) (Result a) -> (Result b) 
(<$>) f Nothing 		= Nothing
(<$>) f (Just (a, r)) 	= Just ((f a), r)

/*************** 4. Pretty Tuple ************/

show_{|(,)|} fa fb (a,b) c 	= ["(":fa a (fb b [")":c])]
parse{|(,)|} fa fb ["(":r]	= case fa r of
	Just (resa, rest)	= case fb rest of
		Just (resb, [")":rest_]) = Just ((resa, resb), rest_) 
		_ 						 = Nothing
	_ 					= Nothing

//------------------ tests --------------
derive gEq Tree

Start = (Start1, Start3, Start8, Start7, Start4, Start5, Start6)

Start1 = and [ test b \\ b <- [False, True]]
Start2 = (show aList, show aTree, show (True, 4))
Start3 = test [1..5]
Start4 = test aTree //This does not work :(
Start5 = show aTree
Start6 :: Result (Tree Int)
Start6 = parse{|*|} Start5
Start7 = test (1,True)
Start8 = show (1,True)
/*
 * For some reason parse parses ["(","Bin","2","Tip","(","Bin","4","Tip","Tip",")",")"] as
 * 'Tip' with ["Bin","2","Tip","(","Bin","4","Tip","Tip",")",")"] as rest.
 * So the first "(" is parsed as "Tip" and then the parser stops... 
 * Which is rather weird. I can't figure out why that happens.
 * So there is an error in my implementation wich shows itself on trees, by I can't find where
 */


aTree :: Tree Int
aTree = Bin 2 Tip (Bin 4 Tip Tip)
aList :: [Int]
aList = [1..10]

