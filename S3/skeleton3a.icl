module skeleton3a

/*
	Advanced Programming.
	Skeleton for exercise 3.1 and 3.2.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

import StdEnv, StdMaybe

/************* showing *******************/

class show_0 a where show_0 :: a [String] -> [String]

//Removed for exercise 1.2
/*instance parse0 Int
instance show_0 Int  where show_0 i    c = [IntTag :toString i:c]
instance show_0 Bool where show_0 b    c = [BoolTag:toString b:c]
instance show_0 UNIT where show_0 unit c = [UNITTag:c] */

IntTag	:== "Int"
BoolTag	:== "Bool"
UNITTag	:== "UNIT"
PAIRTag	:== "PAIR"

show :: a -> [String] | show_0 a
show a = show_0 a []

/**************** parsing *************************/

:: Result a :== Maybe (a,[String])

class parse0 a :: [String] -> Result a

//Removed for exercise 1.2
/*instance parse0 Int
where
	parse0 [IntTag,i:r] = Just (toInt i, r)
	parse0 r = Nothing
instance parse0 Bool
where
	parse0 [BoolTag,b:r] = Just (b=="True", r)
	parse0 r = Nothing
instance parse0 UNIT
where
	parse0 [UNITTag:r] = Just (UNIT, r)
	parse0 r = Nothing*/

/**************** Example Types and conversions *************************/

:: T		= C
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b
:: CONS   a		= CONS String a

//	Generic type representations
:: TG		:== CONS UNIT
:: ColorG	:== EITHER (EITHER (CONS UNIT) (CONS UNIT)) (CONS UNIT)
:: ListG a	:== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b	:== CONS (PAIR a b)

// Conversions

fromT :: T									-> TG
fromT c										= CONS "C" UNIT

fromColor :: Color 							-> ColorG
fromColor Red								= LEFT (LEFT  (CONS "Red"    UNIT))
fromColor Yellow							= LEFT (RIGHT (CONS "Yellow" UNIT))
fromColor Blue								=       RIGHT (CONS "Blue"   UNIT)

fromList :: [a]								-> ListG a
fromList []									= LEFT  (CONS "Nil"  UNIT)
fromList [a:as]								= RIGHT (CONS "Cons" (PAIR a as))

fromTree :: (Tree a)						-> TreeG a
fromTree Tip								= LEFT  (CONS "Tip" UNIT)
fromTree (Bin a l r)						= RIGHT (CONS "Bin" (PAIR a (PAIR l r)))

fromTup :: (a,b)							-> TupG a b
fromTup (a,b)								= CONS "Tuple2" (PAIR a b)

toT :: TG									-> T
toT (CONS _ UNIT)							= C

toColor :: ColorG							-> Color
toColor (LEFT (LEFT  (CONS _ UNIT)))		= Red
toColor (LEFT (RIGHT (CONS _ UNIT)))		= Yellow
toColor       (RIGHT (CONS _ UNIT))			= Blue

toList :: (ListG a)							-> [a]
toList (LEFT  (CONS s UNIT))        		= []
toList (RIGHT (CONS s (PAIR a as)))		 	= [a:as]

toTree :: (TreeG a)							-> Tree a
toTree (LEFT  (CONS s UNIT))                = Tip
toTree (RIGHT (CONS s (PAIR a (PAIR l r)))) = Bin a l r

toTup :: (TupG a b)							-> (a,b)
toTup (CONS s (PAIR a b))					= (a,b)

/**************** to test if parse and show work properly *************************/

test :: t -> Bool | eq0, show_0, parse0 t
test x
	= case parse0 (show x) of
		Just (y,[])	= eq0 x y
		_			= False

/**************** equality with a class for each kind *************************/

class eq0 t ::                              t       t      -> Bool
class eq1 t :: (a a -> Bool)               (t a)   (t a)   -> Bool
class eq2 t :: (a a -> Bool) (b b -> Bool) (t a b) (t a b) -> Bool

instance eq0 UNIT			where eq0 _ _                       = True
instance eq0 Int			where eq0 n m                       = n == m

instance eq1 CONS			where eq1 f   (CONS s x) (CONS t y) = s == t && f x y

instance eq2 PAIR			where eq2 f g (PAIR a b) (PAIR x y) = f a x && g b y
instance eq2 EITHER			where eq2 f g (LEFT  x)  (LEFT  y)  = f x y
							      eq2 f g (RIGHT x)  (RIGHT y)  = g x y
							      eq2 f g _          _          = False

instance eq0 [a] | eq0 a	where eq0   l m = eq1 eq0 l m
instance eq1 []				where eq1 f l m = eq2 (eq1 eq0) (eq1 (eq2 f (eq1 f))) (fromList l) (fromList m)

/**************** map *************************/

class map0 t ::                    t      -> t
class map1 t :: (a -> b)          (t a)   -> t b
class map2 t :: (a -> b) (c -> d) (t a c) -> t b d 

instance map0 Int			where map0 i              = i
instance map0 UNIT			where map0 UNIT           = UNIT

instance map1 CONS			where map1 f   (CONS n x) = CONS n (f x)

instance map2 PAIR			where map2 f g (PAIR x y) = PAIR  (f x) (g y)
instance map2 EITHER		where map2 f g (LEFT  x)  = LEFT  (f x)
							      map2 f g (RIGHT y)  = RIGHT (g y)

/**************** End Prelude *************************/

/**************** please add all new code below this line *************************/

($) infixr 8; // :: (a->r) -> a -> r
($) f a = f a

instance eq0 Color		where 
	eq0  c1 c2 	= eq2 (eq2 (eq1 eq0) (eq1 eq0)) (eq1 eq0) (fromColor c1) (fromColor c2)
instance ==  Color		where (==) c1 c2 = eq0 c1 c2	// just to use the well-known notation...
instance show_0 Color	where 
	show_0 cl c = show_2 (show_2 (show_1 show_0) (show_1 show_0)) (show_1 show_0) (fromColor cl) c
instance parse0 Color	where 
	parse0 r 	= toColor <$> (parse2 (parse2 (parse1_cons ((==)"Red") parse0) (parse1_cons ((==)"Yellow") parse0)) (parse1_cons ((==)"Blue") parse0) r) 

instance eq2 (,) where
	eq2 fa fb t1 t2 = eq1 (eq2 fa fb) (fromTup t1) (fromTup t2)
instance eq0 (a,b) | eq0 a & eq0 b where
	eq0 t1 t2 = eq2 eq0 eq0 t1 t2

/**************** 1.2 Show/parse without tags *************************/
/************ SHOW *************/
//show_0 defined on line 16
class show_1 t where 
	show_1 :: (a [String] -> [String]) (t a) [String] -> [String] 
class show_2 t where 
	show_2 :: (a [String] -> [String]) (b [String] -> [String]) (t a b) [String] -> [String]

//to define: String, PAIR, EITHER, CONS, List, tuple, Tree
instance show_0 Int  where show_0 i    c = [toString i:c]
instance show_0 Bool where show_0 b    c = [toString b:c]
instance show_0 UNIT where show_0 unit c = c
instance show_2 PAIR where
	show_2 fa fb (PAIR a b) c = fa a $ fb b c
instance show_0 (PAIR a b) | show_0 a & show_0 b where
	show_0 p c 	= show_2 show_0 show_0 p c
instance show_1 CONS where
	show_1 f (CONS s a) c = [s:f a c]
instance show_0 (CONS a) | show_0 a where
	show_0 co c = show_1 show_0 co c
instance show_2 EITHER where
	show_2 fa _ (LEFT a) c	= fa a c
	show_2 _ fb (RIGHT b) c	= fb b c
instance show_0 (EITHER a b) | show_0 a & show_0 b where
	show_0 e c 	= show_2 show_0 show_0 e c

instance show_1 [] where
	show_1 f as c 	= show_2 (show_1 show_0) (show_1 (show_2 f (show_1 f))) (fromList as) c
instance show_0 [a] | show_0 a where
	show_0 as c 	= show_1 show_0 as c
instance show_2 (,) where
	show_2 fa fb t c = show_1 (show_2 fa fb) (fromTup t) c
instance show_0 (a,b) | show_0 a & show_0 b where
	show_0 t c 	= show_2 show_0 show_0 t c
instance show_1 Tree where
	show_1 f t c = show_2 (show_1 show_0) (show_1 (show_2 f (show_2 (show_1 f) (show_1 f)))) (fromTree t) c
instance show_0 (Tree a) | show_0 a where
	show_0 t c 	= show_1 show_0 t c
instance show_0 T where
	show_0 t c	= show_1 show_0 (fromT t) c

/************* PARSE ************/
//pase0 defined on line 34
//Note that all parsers expect the input to be in a correct format, malformed strings yield
//non-gracious errors.
class parse1 t where
	parse1 :: ([String] -> Result a) [String] -> Result (t a)
class parse2 t where
	parse2 :: ([String] -> Result a) ([String] -> Result b) [String] -> Result (t a b)

instance parse0 Int where
	parse0 [i:r] = Just (toInt i, r)
instance parse0 Bool where
	parse0 ["True":r] 	= Just (True, r)
	parse0 ["False":r] 	= Just (False, r)
	parse0 _ 			= Nothing // any other string is not a valid Bool
instance parse0 UNIT where
	parse0 r 	= Just (UNIT, r)
instance parse1 CONS where
	parse1 f r = parse1_cons (const True) f r
//Since we don't save any structural information, and some datastructures (ike color) 
//rely on nothing but structure and constructor information in their generic
//representation it needs to be possible to check the constructors
parse1_cons :: (String -> Bool) ([String] -> Result a) [String] -> Result (CONS a)
parse1_cons match f [s:r] = if (match s) ((CONS s) <$> f r) Nothing
instance parse0 (CONS a) | parse0 a where
	parse0 r 	= parse1 parse0 r
instance parse2 PAIR where
	parse2 fa fb r = case fa r of
		Nothing 			= Nothing
		(Just (res,rest)) 	= (PAIR res) <$> fb rest
instance parse0 (PAIR a b) | parse0 a & parse0 b where
	parse0 r = parse2 parse0 parse0 r
instance parse2 EITHER where
	parse2 fa fb r = case fa r of
		Nothing 			= RIGHT <$> fb r
		result				= LEFT <$> result
instance parse0 (EITHER a b) | parse0 a & parse0 b where
	parse0 r = parse2 parse0 parse0 r

instance parse1 [] where
	parse1 f r 		= toList <$> parse2 (parse1_cons ((==)"Nil") parse0) (parse1_cons ((==)"Cons") (parse2 f (parse1 f))) r
instance parse0 [a] | parse0 a where
	parse0 r 		= parse1 parse0 r
instance parse2 (,) where
	parse2 fa fb r 	= toTup <$> parse1 (parse2 fa fb) r
instance parse0 (a,b) | parse0 a & parse0 b where
	parse0 r 		= parse2 parse0 parse0 r
instance parse1 Tree where
	parse1 f r 		= toTree <$> parse2 (parse1 parse0) (parse1 (parse2 f (parse2 (parse1 f) (parse1 f)))) r
instance parse0 (Tree a) | parse0 a where
	parse0 r 		= parse1 parse0 r
instance parse0 T where
	parse0 t 		= toT <$> parse1 parse0 t


/********** fmap for Result  *****/

(<$>) infixl 4 :: (a -> b) (Result a) -> (Result b) 
(<$>) f Nothing 		= Nothing
(<$>) f (Just (a, r)) 	= Just ((f a), r)

/*
 - The below definition yields the following error:
 	"Result used with wrong arity"
   Writing 'instance Functor (Result a' ofcourse results in a kind-error. 
   Is this due to Result being (I presume) a type-synonym due to the :== operator?
- 
class Functor f where 
	fmap :: (a->b) (f a) -> (f b)
instance Functor Result where
	fmap _ Nothing 			= Nothing
	fmap f (Just (a,r)) 	= Just ((f a), r)
*/

/**************** 2 Generic map *************************/

instance map0 Bool where map0 b = b
instance map0 T    where map0 t = t

instance map1 []	where map1 f l = toList $ map2 (map1 map0) (map1 (map2 f (map1 f))) (fromList l)
instance map1 Tree where 
	map1 f t 		= toTree $ map2 (map1 map0) (map1 (map2 f (map2 (map1 f) (map1 f)))) (fromTree t)
instance map2 (,) where
	map2 fa fb t 	= toTup $ map1 (map2 fa fb) (fromTup t)

/********* TESTs **********/

Start = Start13 //(Start1, Start4, Start2, Start3, Start5, Start6, Start8, Start7, Start9)

Start1 = (show $ PAIR 1 False)
l :: EITHER Bool Int
l = LEFT True
r :: EITHER Bool Int
r = RIGHT 4
Start2 = show l
Start3 = show r
Start4 :: Result (PAIR Int Bool)
Start4 = parse0 Start1
Start5 :: Result (EITHER Bool Int)
Start5 = parse0 Start2
Start6 :: Result (EITHER Bool Int)
Start6 = parse0 Start3

Start10 = show [1..3]
Start11 :: Result [Int]
Start11 = parse0 Start10

Start8 = map (\c -> show $ fromColor c) [Red, Yellow, Blue]
Start7 :: Result Color
Start7 = parse0 $ show $ fromColor Blue
Start9
 =	[ and [ test i \\ i <- [-25 .. 25]]
	, and [ c == toColor (fromColor c) \\ c <- [Red, Yellow, Blue]]
	, and [ test c \\ c <- [Red,Yellow,Blue]]
	, test [1 .. 3]
	, test [(a,b) \\ a <- [1 .. 2], b <- [5 .. 7]]
	// maps
	, map1 ((+) 1) [0 .. 5] == [1 .. 6]
	]

Start13 = (	map1 fac aList
		  , map1 fac aTree
		  , map2 (map1 fac) (map1 fac) (aTree, aTree)
		  , map1 (\x -> (x, fac x)) aList
		  )
	where
		fac :: Int -> Int
		fac 0 = 1
		fac n = prod [1..n]
		aTree :: Tree Int
		aTree = Bin 2 Tip (Bin 4 Tip Tip)
		aList :: [Int]
		aList = [1..10]
