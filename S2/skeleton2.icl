module skeleton2

/*
	Skeleton for Exercise 2 of Advanced Programming.
	Works fine with the environment Everything, but you can also use 
	StdEnv and manually add StdMaybe from the directory {Application}\Libraries\StdLib.
	
	Pieter Koopman, 2013
*/

import StdEnv, StdMaybe

/**************** Prelude *************************/

($) infixr 8; // :: (a->r) -> a -> r
($) f a = f a

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b
:: CONS   a		= CONS String a

//	Generic type representations
:: ListG a	:== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b	:== CONS (PAIR a b)
:: TG		:== CONS UNIT

// Conversions
fromList :: [a]	-> ListG a
fromList []		= LEFT (CONS "Nil" UNIT)
fromList [a:as]	= RIGHT (CONS "Cons" (PAIR a as))

toList :: (ListG a) -> [a]
toList (LEFT (CONS "Nil" UNIT)) = []
toList (RIGHT (CONS "Cons" (PAIR a as))) = [a:as]

// to GEN functions, from exercise 1 with CONS added:
//:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
treeToGen :: (Tree a) -> (TreeG a)
treeToGen Tip			= LEFT (CONS "Tip" UNIT)
treeToGen (Bin t1 a t2)	= RIGHT (CONS "Bin" (PAIR a (PAIR t1 t2)))

toTree :: (TreeG a) -> (Tree a)
toTree (LEFT (CONS "Tip" UNIT)) = Tip
toTree (RIGHT (CONS "Bin" (PAIR a (PAIR t1 t2)))) = Bin t1 a t2

tupleToGen :: (a,b) -> (TupG a b)
tupleToGen (a,b)	= CONS "tuple" (PAIR a b)

toTuple :: (TupG a b) -> (a,b)
toTuple (CONS "tuple" (PAIR a b)) = (a,b)

/**************** End Prelude *************************/

/**************** Part 1 *******************************/

:: Tree a = Tip | Bin (Tree a) a (Tree a)

class Container t
where
	Cinsert   :: a (t a) -> t a      | <        a
	Ccontains :: a (t a) -> Bool     | <, Eq    a
	Cshow     ::   (t a) -> [String] | toString a
	Cnew	  :: t a

instance Container [] where
	Cinsert 	a as 	= [a:as]
	Ccontains 	n [] 	= False
	Ccontains 	n [a:as]
		| n == a 		= True
		| True			= Ccontains n as
	Cshow as 			= map toString as //intersperse "," $ 
	Cnew 				= []

instance Container (Tree) where
	Cnew = Tip
	//Tree grows left when using Cinsert
	Cinsert a Tip 							= Bin Tip a Tip
	Cinsert a (Bin lt b rt) 				= Bin (Cinsert a lt) b rt
	Cshow Tip 			= ["T"]
	Cshow (Bin lt b rt) = ["(Bin ":Cshow lt] ++ [toString b:Cshow rt] ++ [")"]
	Ccontains _ Tip 			= False
	Ccontains a (Bin lt b rt) 	= a == b || Ccontains a lt || Ccontains a rt

// Possible test:
//Start = (Ccontains 3 c, Ccontains 8 c, Cshow c) 
//	where c = Cinsert 6 $ Cinsert 4 $ Cinsert 2 $ Cinsert 8 []

//Start = (Ccontains 4 t, Ccontains 1 t, Cshow t)
//	where t = Cinsert 4 $ Cinsert 0 $ Cinsert 6 $ Cinsert 1 Tip

/**************** Part 3 *******************************/
//	Example types
show :: a -> [String] | show_ a
show a = show_ a []

class show_ a where show_ :: a [String] -> [String]

instance show_ Int    where show_ i c 	= ["Int"  : toString i : c]
instance show_ Bool   where show_ b c 	= ["Bool" : toString b : c]
instance show_ String where show_ s c 	= ["String" : s : c]

instance show_ UNIT where 
	show_ _ c 				= ["UNIT" : c]
instance show_ (PAIR a b) 	| show_ a & show_ b where 
	show_ (PAIR a b) c 		= ["PAIR" : (show_ a (show_ b c))]
instance show_ (EITHER a b) | show_ a & show_ b  where 
	show_ (LEFT a) c 		= ["ELEFT" : (show_ a c)]
	show_ (RIGHT a) c 		= ["ERIGHT" : (show_ a c)]
instance show_ (CONS a) 	| show_ a where
	show_ (CONS s a) c 		= ["CONS" : s : (show_ a c)]

instance show_ (Tree a) | show_ a where show_ t c = show_ (treeToGen t) c
instance show_ [a] 		| show_ a where show_ a c = show_ (fromList a) c
instance show_ (a,b) 	| show_ a & show_ b where 
	show_ t c = show_ (tupleToGen t) c

/*Start = (show t, show tr, show l, show tr1) where
	t :: (Bool, String)
	t 	= (False, "Hoi")
	tr :: Tree Int
	tr 	= Cinsert 4 $ Cinsert 0 $ Cinsert 6 $ Cinsert 1 Tip
	l :: [Int]
	l 	= Cinsert 6 $ Cinsert 4 $ Cinsert 2 $ Cinsert 8 []
	tr1 :: TreeG Int
	tr1 = treeToGen $ toTree $ treeToGen tr*/

/**************** Part 4 *******************************/
:: Result a = Fail | Match a [String]
result :: b (a [String] -> b) (Result a) -> b
result def _ Fail 		= def
result _ f (Match a r)	= f a r

class parse a :: [String] -> Result a

:: Parser a = Parser ([String] -> Result a)

runParser :: (Parser a) [String] -> Result a
runParser (Parser f) inp = f inp

pItem :: Parser String
pItem = Parser $ \inp -> case inp of
	[x:xs] 	= Match x xs
	[]		= Fail

pAny :: Parser a | parse a
pAny = Parser $ \i -> parse i

pSatisfy :: (String -> Bool) -> Parser String
pSatisfy f = pItem >>= \x -> if (f x) (inject x) empty

pStr :: String -> Parser String
pStr s = pSatisfy ((==)s)

pInt :: Parser Int
pInt = pStr "Int" >> pSatisfy (\inp -> all ((==)True) $ map isDigit $ fromString inp) >>= (\i -> inject (toInt i))

pBool :: Parser Bool
pBool = pStr "Bool" >> pItem >>= \b -> inject (b=="True")

pString :: Parser String
pString = pStr "String" >> pItem

pUNIT :: Parser UNIT
pUNIT = pStr "UNIT" >> inject UNIT

pEITHER :: Parser (EITHER a b) | parse a & parse b
pEITHER = pEITHER_ pAny pAny

//Parses an EITHER and lets you spcify which parser to use for the innards of the EITHER
pEITHER_ :: (Parser a) (Parser b) -> Parser (EITHER a b) | parse a & parse b
pEITHER_ pl pr = left_ <|> right_
	where
		left_ 	= pStr "ELEFT" >> pl >>= \l -> inject (LEFT l)
		right_	= pStr "ERIGHT" >> pr >>= \r -> inject (RIGHT r)

pCONS :: Parser (CONS a) | parse a
pCONS = pStr "CONS" >> pItem >>= \str -> pAny >>= \a -> inject (CONS str a)

//like pCONS but with a condition on the String. This fails when the CONS is not equal to the provided string
pCONSM :: String -> Parser (CONS a) | parse a
pCONSM match = pStr "CONS" >> pItem >>= \str -> if (str == match) (pAny >>= \a -> inject (CONS str a)) empty

pPAIR :: Parser (PAIR a b) | parse a & parse b
pPAIR = pStr "PAIR" >> pAny >>= \p1 -> pAny >>= \p2 -> inject (PAIR p1 p2)

//We want the parser to Fail when we are not actually reading a tree but something with
//the same structure (See Start5) so we use pEITHER_ in conjunction with pCONSM to check 
//if the CONS match. If we wouldn't do this then the failure would happen in the toTree
//function which doesn't support failure, so would yield an actual error
pTree :: Parser (Tree a) | parse a
pTree = pEITHER_ (pCONSM "Tip") (pCONSM "Bin") >>= \t -> inject (toTree t)

pTup :: Parser (a,b) | parse a & parse b
pTup = pCONSM "tuple" >>= \t -> inject (toTuple t)

//We want the parser to Fail when we are not actually reading a list but something with
//the same structure (See Start5) so we use pEITHER_ in conjunction with pCONSM to check 
//if the CONS match. If we wouldn't do this then the failure would happen in the toList
//function which doesn't support failure, so would yield an actual error
pList :: Parser [a] | parse a
pList = pEITHER_ (pCONSM "Nil") (pCONSM "Cons") >>= \l -> inject (toList l)

//NOTE: Why can't we use: 'parse = runParser pInt', why do we need to specify the r :(

instance parse Int where parse r = runParser pInt r 
instance parse Bool where parse r = runParser pBool r
instance parse String where parse r = runParser pString r
instance parse UNIT where parse r = runParser pUNIT r
instance parse (EITHER a b) | parse a & parse b where parse r = runParser pEITHER r
instance parse (CONS a) | parse a where parse r = runParser pCONS r
instance parse (PAIR a b) | parse a & parse b where parse r = runParser pPAIR r

instance parse (Tree a) | parse a where parse r = runParser pTree r
instance parse (a,b) | parse a & parse b where parse r = runParser pTup r
instance parse [a] | parse a where parse r  = runParser pList r

:: T = C

/************ Functor, Applicative and Monad *******/

//:: Parser a = Parser ([String] -> Result a)
//runParser :: (Parser a) [String] -> Result a
//:: Result a = Fail | Match a [String]
//class parse a :: [String] -> Result a

instance Functor Result where
	fmap _ Fail 		= Fail
	fmap f (Match a r) 	= Match (f a) r

instance Functor Parser where
	fmap f p 	= Parser $ \inp -> f <$> runParser p inp

instance Applicative Parser where
	pure a 		= Parser $ \inp -> Match a inp
	(<*>) f p 	= Parser $ \inp -> result Fail (\f1 inp1 -> runParser (f1 <$> p) inp1) $ runParser f inp

instance Alternative Parser where
	empty 		= Parser $ const Fail
	(<|>) p1 p2	= Parser $ \inp -> result (runParser p2 inp) (\a r -> Match a r) $ runParser p1 inp 

instance Monad Parser where
	inject a 	= pure a
	(>>=) p f 	= Parser \inp -> result Fail (\res inp1 -> runParser (f res) inp1) $ runParser p inp

// These are obviously somewhere in the library, but writing 
// them myself seems like a fun exercise
class Functor f where 
	fmap :: (a->b) (f a) -> (f b)

(<$>) infixl 4 :: (a -> b) (f a) -> (f b) | Functor f
(<$>) f a = fmap f a

class Applicative f | Functor f where
	pure :: a -> (f a)
	(<*>) infixl 4 :: (f (a -> b)) (f a) -> (f b)

class Alternative f | Applicative f where
	empty :: (f a)
	(<|>) :: (f a) (f a) -> (f a)


class Monad m | Applicative m where
	inject :: a -> (m a)
	(>>=) infixl 4 :: (m a) (a -> m b) -> (m b)

(>>) infixl 4 :: (m a) (m b) -> (m b) | Monad m
(>>) m1 m2 = m1 >>= \_ -> m2


/**************** Starts *******************************/

Start = ("add your own Start rule!\n", Start5)

// Possible tests:
//Start1 :: ([String],Result T)
//Start1 = (strings,parse strings) where strings = show C

Start2 :: ([String],Result (Int,Bool))
Start2 = (strings,parse strings) where strings = show (1,False)

Start3 :: ([String],Result [Int])
Start3 = (strings,parse strings) where strings = show l; l :: [Int]; l = [1..4]

Start4 :: (Tree Int, [String],Result (Tree Int))
Start4 = (t,strings,parse strings)
where
	strings = show t
	
	t :: Tree Int
	t = Bin (Bin Tip 2 (Bin Tip 3 Tip)) 4 (Bin (Bin Tip 5 Tip) 6 Tip)

Start5 :: (Result [Int])
Start5 = parse not_a_list
	where
		t :: EITHER (CONS UNIT) (CONS (PAIR Int (EITHER (CONS UNIT) (UNIT))))
		t = RIGHT (CONS "not a list constructor" (PAIR 5 (LEFT (CONS "neither" UNIT))))
		not_a_list = show t

Start6 :: (Result Int, Result Bool, Result String, Result (Bool, Int), Result [String], Result (EITHER Int Int), Result (Tree Int), Result (Tree Int))
Start6 = (parse i, parse b, parse s, parse p, parse l, parse ei, parse ts, parse tc)
	where
		i = show 5
		b = show False
		s = show "Test"
		p = show (True, 9)
		l = show ["Lijstitem1", "lijstitem2"]
		e :: EITHER Int Int
		e = RIGHT 5
		ei = show e
		t1 :: Tree Int
		t1 = Tip
		ts = show t1
		t2 :: Tree Int
		t2 = Cinsert 5 $ Cinsert 1 $ Tip
		tc = show t2


























