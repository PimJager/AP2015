module skeleton5

/*
	Advanced Programming.
	Assignment 5
	Pim Jager
*/

import iTasks
import Text

($) infixr 8; // :: (a->r) -> a -> r
($) f a = f a
(>>) infixl 1 :: !(Task a) (!Task b) -> Task b | iTask a & iTask b
(>>) ta tb = ta >>= \_ -> tb

:: Idea	= 	{idea 		:: String
			,details 	:: Note
			,user		:: Name
			,number 	:: Int}
:: EnterIdea =	{idea_ 		:: String
				,details_ 	:: Note}
:: Name :== String
:: Ideas :== [Idea]
derive class iTask Idea, EnterIdea

//Store
ideas :: ReadWriteShared Ideas Ideas
ideas = sharedStore "Ideas" []
nextNumber :: Task Int
nextNumber = get ideas >>= \ids -> case ids of
	[] 		= return 1
	[i:is]	= return ((i.number) + 1)

enterIdeas :: Name -> Task Ideas
enterIdeas name = enterInformation (name +++ "  add an idea") [] >>* 
					[OnAction ActionOk $ hasValue (\idea_ -> 
							nextNumber 
						>>= \num -> get ideas 
						>>=	\ids -> let idea = 	{idea 		= idea_.idea_
												,details 	= idea_.details_
												,user 		= name
												,number 	= num}
									in set [idea:ids] ideas)
					]

viewIdeas :: Task Ideas
viewIdeas = enterChoiceWithShared "Ideas" [] ideas 
				>&^ (viewSharedInformation "Selection" []) 
				>> get ideas

mainTask :: Task Ideas
mainTask =   enterInformation "Enter your name" []
				>>= \name-> (forever $ enterIdeas name) -||- viewIdeas

Start :: *World -> *World
Start world = startEngine mainTask world