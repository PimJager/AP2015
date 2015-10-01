module skeleton4

/*
	Advanced Programming.
	Skeleton for assignment 4.
	To be used in a project with the environment iTasks.
	Pieter Koopman, pieter@cs.ru.nl
*/

import iTasks
import Text

($) infixr 8; // :: (a->r) -> a -> r
($) f a = f a

:: Idea	:== Note
:: Ideas :== [Idea]
:: Name	:== String

:: NamedIdeas = { name :: Name, ideas :: Ideas}
derive class iTask NamedIdeas // generic magic

//For showing:
:: NumberedIdeas :== [(Int, Idea)]
:: NNIdeas = {name_ :: Name, ideas_ :: NumberedIdeas}
				//naming these name & ideas causes the typechecker to not be 
				//able to determine the types at ::42, ::48 and ::49. Which
				//is weird since all functions have explicitly stated types
derive class iTask NNIdeas

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   enterInformation "Enter your name" []
	>>= task

editIdeas :: Name -> Task NamedIdeas
editIdeas name
	=           enterInformation (name +++ " add your idea") []
	>>= \ideas . return {name = name, ideas = ideas}

mainTask =   doIdentified editIdeas
		 >>= viewInformation "The result" [ViewWith (\res -> filterI (formatI res))] //how to do function composition?
		 where
		 	formatI :: NamedIdeas -> NNIdeas
		 	formatI {name=n, ideas=mi} = 	{ name_ = n
		 								 	, ideas_ = zip2 [1..] mi
		 								 	}
		 	filterI :: NNIdeas -> NNIdeas
		 	filterI {name_=n, ideas_=mi} = 	{ name_ = n	
		 									, ideas_ = filter (\(n,i) -> textSize $ toString i >= 10) mi
		 									}

Start :: *World -> *World
Start world = startEngine mainTask world
