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

show_{|Int|}  i c = [toString i:c]
show_{|Bool|} b c = [toString b:c]

show a = show_{|*|} a []

//------------------ parse --------------

:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

parse{|Bool|} ["True" :r] = Just (True ,r)
parse{|Bool|} ["False":r] = Just (False,r)
parse{|Bool|} _ = Nothing

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

//------------------ tests --------------

Start = and [test b \\ b <- [False, True]]
