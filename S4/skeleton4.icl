module skeleton4

/*
	Advanced Programming.
	Skeleton for assignment 4.
	To be used in a project with the environment iTasks.
	Pieter Koopman, pieter@cs.ru.nl
*/

import iTasks

:: Idea	:== String
:: Name	:== String

:: NamedIdea = { name :: Name, idea :: Idea}
derive class iTask NamedIdea // generic magic

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   enterInformation "Enter your name" []
	>>= task

editIdea :: Name -> Task NamedIdea
editIdea name
	=           enterInformation (name +++ " add your idea") []
	>>= \idea . return {name = name, idea = idea}

mainTask =   doIdentified editIdea
		 >>= viewInformation "The result" []

Start :: *World -> *World
Start world = startEngine mainTask world
