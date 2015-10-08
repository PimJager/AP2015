module skeleton5

/*
	Advanced Programming.
	Assignment 5
	Pim Jager - s4644425
*/

import iTasks
import Text

($) infixr 8; // :: (a->r) -> a -> r
($) f a = f a
(>>) infixl 1 :: !(Task a) !(Task b) -> Task b | iTask a & iTask b
(>>) ta tb = ta >>= \_ -> tb

:: Idea		= {idea :: String, details :: Note, user :: Name, number :: Int, likes :: Int}
:: Idea_ 	= {idea_ :: String, details_ :: Note}
:: Name 	:== String
derive class iTask Idea, Idea_

ideas :: ReadWriteShared [Idea] [Idea]
ideas = sharedStore "Ideas" []
nextNumber :: Task Int
nextNumber = get ideas >>= \ids -> case ids of
	[] 		= return 1
	[i:is]	= return ((i.number) + 1)

removeIdea :: Idea [Idea] -> [Idea]
removeIdea i is = reverse $ removeIdea_ i is []
	where
		removeIdea_ _ [] c = c
		removeIdea_ (i=:{number=n}) [(ii=:{number=ni}):is] c = if (n==ni) (removeIdea_ i is c)
																		  (removeIdea_ i is [ii:c])

likeIdea :: Idea [Idea] -> [Idea]
likeIdea i is = reverse $ likeIdea_ i is []
	where
		likeIdea_ _ [] c = c
		likeIdea_ (i=:{number=n}) [(ii=:{number=ni}):is] c = if (n==ni) (likeIdea_ i is [newIdea:c])
																		(likeIdea_ i is c)
		newIdea = {i & likes=i.likes+1}

enterIdeas :: Name -> Task [Idea]
enterIdeas name = enterInformation (name +++ "  add an idea") [] >>* 
					[OnAction ActionOk $ hasValue (\idea_ -> 
							nextNumber 
						>>= \num -> get ideas 
						>>=	\ids -> let idea = 	{idea 		= idea_.idea_
												,details 	= idea_.details_
												,user 		= name
												,number 	= num
												,likes 		= 0}
									in set [idea:ids] ideas)
					]

viewIdeas :: Name -> Task [Idea]
viewIdeas name = (enterChoiceWithShared "Ideas" [] ideas)
					>&^ (viewSharedInformation "Selection" [])
					>>* [OnAction (Action "Delete all" [ActionIcon "Delete all"]) deleteAll
						,OnAction ActionDelete delete
						,OnAction ActionOk cancelSelection
						,OnAction (Action "Like" [ActionIcon "Like"]) like
						]
					where
						deleteAll = always (upd (const []) ideas)
						delete = hasValue (\i -> upd (\is -> removeIdea i is) ideas)
						cancelSelection = always $ get ideas
						like = ifValue (\i -> i.user <> name) (\i -> upd (likeIdea i) ideas)

mainTask :: Task [Idea]
mainTask =   enterInformation "Enter your name" []
				>>= \name-> forever (enterIdeas name -|| (forever $ viewIdeas name))

Start :: *World -> *World
Start world = startEngine mainTask world