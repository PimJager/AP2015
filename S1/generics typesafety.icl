module skeleton1

/*
	Course I00032 Advanced Programming 2014
	Skeleton for assignment 1
	Pieter Koopman
*/

import StdEnv

Start :: [Ordering]
Start = [	a >< b
		, 	c >< d
		, 	(Bin 1 Tip Tip) >< c
		, 	(3,4) >< (2,1)
		,	[1..] >< [1..5]
		]
	where
		a :: [Int]
		a = []
		b :: [Int]
		b = []
		c :: (Tree Int)
		c = Tip
		d :: (Tree Int)
		d = Tip

/**************** Prelude: ****************************/
//	Example types
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)         
:: Rose a	= Rose a [Rose a]

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b

//	Generic type representations
:: RoseG a	:== PAIR a [Rose a]

// Conversions
fromRose :: (Rose a)	-> RoseG a
fromRose (Rose a l)		= PAIR a l

// Oerdering

::	Ordering = Smaller | Equal | Bigger

instance == Ordering where
	(==) Smaller Smaller 	= True
	(==) Equal Equal		= True
	(==) Bigger Bigger		= True
	(==) _ _				= False

class (><) infix 4 a :: !a !a -> Ordering

instance >< Int where		// Standard ordering for Int
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Char where		// Standard ordering for Char
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< String where	// Standard lexicographical ordering
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Bool where		// False is smaller than True
	(><) False True  = Smaller
	(><) True  False = Bigger
	(><) _     _     = Equal

/**************** End Prelude *************************/



/************ 1. Ordering by Overloading **************/

/* Commented due to 3.1
instance >< (a,b) | >< a & >< b where
	(><) (a1,_) (a2,_) = a1 >< a2

instance >< [a] | >< a where
	(><) [a] [b]			= a >< b
	(><) [a:as] [b:bs]
		| (a >< b) == Equal	= as >< bs
		| otherwise			= a >< b
	(><) [] []				= Equal	
	(><) [] _				= Smaller
	(><) _ _ 				= Bigger

instance >< Color where
	(><) a b = (toInt a) >< (toInt b)
		where
			toInt Red 		= 1
			toInt Yellow 	= 2
			toInt Blue 		= 3

instance >< (Rose a) | >< a where 
	(><) (Rose a ars) (Rose b brs) 
		| (a >< b) == Equal	= ars >< brs
		| otherwise			= a >< b

instance >< (Tree a) | >< a where
	(><) Tip Tip 	= Equal
	(><) Tip _		= Smaller
	(><) _	 Tip 	= Bigger
	(><) (Bin a at1 at2) (Bin b bt1 bt2)
		| (a >< b) == Equal	&& (at1 >< bt1) == Equal	= at2 >< bt2
		| (a >< b) == Equal								= at1 >< at2
		| otherwise										= a >< b
*/


/******************* 2. Generic Ordering **************/
/* (1) */
:: ListG a 		:== EITHER UNIT (PAIR a [a])

:: ColorG 		:== EITHER UNIT (EITHER UNIT UNIT)

:: TupleG a b 	:== PAIR a b

:: TreeG a		:== EITHER (PAIR a (PAIR (Tree a) (Tree a))) UNIT

/* (2) */
listToGen :: [a] -> (ListG a)
listToGen []		= LEFT UNIT
listToGen [a:as]	= RIGHT (PAIR a as)

tupleToGen :: (a,b) -> (TupleG a b)
tupleToGen (a,b)	= PAIR a b

colorToGen :: Color -> (ColorG)
colorToGen Blue 	= LEFT UNIT
colorToGen Yellow	= RIGHT (LEFT UNIT)
colorToGen Red		= RIGHT (RIGHT UNIT)

treeToGen :: (Tree a) -> (TreeG a)
treeToGen Tip			= RIGHT UNIT
treeToGen (Bin a t1 t2)	= LEFT (PAIR a (PAIR t1 t2))

/* (3) */
// RIGHT (PAIR 1 (RIGHT (PAIR 2 (RIGHT 3 (LEFT UNIT)))))
// No, it's (RIGHT (PAIR 1 [2,3])), but that's a good thing as we want toGen to be lazy. 
// Otherwise it would not be possible to listToGen [1..] 

/* (4) */
/* No, say for example we use this gen class:
 * class toGen a b :: a b -> Gen a b
 * Then we could have gen types as such:
 * :: IntG 		:== EITHER UNIT (PAIR UNIT Int) 	// :: Int = Nul | Cont Int
 * :: CharG 	:== EITHER UNIT (PAIR UNIT Char)	// :: Char = CharNul | Cont Char
 * :: Gen a b	:== EITHER (EITHER IntG CharG) (EITHER (ListG a) (TupleG a b)) 
 * Note that toGen requires two parameters, because a tuple (1 value) has two
 * inner types which we both need to know to be able to deconstruct it into a PAIR
 * So now we can't implement a simple toGen Int -> GenG _ _ function as toGen
 * requires two parameters.
 */

/********** 3. Ordering via generic programming *******/
/* (1) */
instance >< UNIT where 
	(><) _ _ = Equal

instance >< (PAIR a b) | >< a & >< b where
	(><) (PAIR a1 b1) (PAIR a2 b2)
		| (a1 >< a2) == Equal	= b1 >< b2
		| otherwise 			= a1 >< a2

instance >< (EITHER a b) | >< a & >< b where
	(><) (LEFT a) (LEFT b) 		= a >< b
	(><) (LEFT _) _		 		= Bigger
	(><) (RIGHT a) (RIGHT b)	= a >< b
	(><) _		   (LEFT _)		= Smaller

instance >< [a] | >< a where (><) l m = listToGen l >< listToGen m

instance >< Color where (><) c1 c2 = colorToGen c1 >< colorToGen c2

instance >< (Tree a) | >< a where (><) t1 t2 = treeToGen t1 >< treeToGen t2

instance >< (a,b) | >< a & >< b where (><) t1 t2 = tupleToGen t1 >< tupleToGen t2

/* (2) */
// Yes

/* (3) */
// It is now no longer necessary to write similar functions, such as equals, or exporting to
// an external representation (i.e. a JSON string) for each and every type.

/* (4) */ 
// Errorhandling get's a little less clear, as errors might be happning in i.e. the generic
// compare functions, and this means that the errors will be specified for 
// UNIT, PAIR or EITHER instead of the constructors of the data type which
// we care about