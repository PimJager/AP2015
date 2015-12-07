{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE ExistentialQuantification #-}
{-#LANGUAGE GADTs #-}

module Assignment10 where

import Data.Functor
import Control.Monad
import Control.Monad.State
import Data.Monoid hiding (Any, All)
import Data.List.Split
import qualified Data.List as L

data Matcher a where
    Is              :: (Matchable a) => (Matcher a) -> Matcher a
    EqualTo         :: (Matchable a) => a -> Matcher a
    LessThan        :: (Matchable a) => a -> Matcher a
    Not             :: (Matchable a) => (Matcher a) -> Matcher a
    Either          :: (Matchable a) => (Matcher a) -> (Matcher a) -> Matcher a
    Contains        :: (Matchable a) => a -> Matcher [a]
    Any             :: (Matchable a) => Matcher a -> Matcher [a] 
        -- does the matcher hold for ANY value in the list?
    All             :: (Matchable a) => Matcher a -> Matcher [a]
        -- does the matcher hold for ALL values in the list
    ContainsString  ::                  String -> Matcher String

deriving instance Show (Matcher a)

class (Show a, Ord a, Eq a) => Matchable a

deriving instance Matchable Bool
deriving instance Matchable Int 
deriving instance Matchable Char
deriving instance (Matchable a) => Matchable [a] -- also covers String

data AssertThat     = forall a. (Matchable a) => AssertThat String a (Matcher a)
                    |           Chain AssertThat AssertThat
                    |           Empty

-- in Haskell (*) is defined in the Num typeclass and can therefore not be overloaded 
-- easily, so we use another approach 

instance Monoid AssertThat where
    mempty          = Empty
    mappend Empty b = b
    mappend a Empty = a
    mappend a b     = Chain a b 

(.*) :: AssertThat -> AssertThat -> AssertThat
(.*) = mappend

count :: AssertThat -> Int
count Empty                 = 0
count (AssertThat _ _ _)    = 1
count (Chain a1 a2)         = count a1 + count a2

testAT (AssertThat d v m) = test' d v m
    where
    test' d v (Is m)    = test' d v m
    test' _ v m         = testM v m

testM :: (Matchable a) => a -> Matcher a -> Bool
testM v (EqualTo x)         = v == x
testM v (LessThan x)        = v < x
testM v (Not m)             = not $ testM v m
testM v (Either x y)        = testM v x || testM v y
testM v (Contains x)        = elem x v
testM v (Any m)             = any (\a -> testM a m) v
testM v (All m)             = all (\a -> testM a m) v
testM v (ContainsString s)  = L.isInfixOf s v

-- deep embedding
instance Show AssertThat where
    show (AssertThat m _ _) = m
    show (Empty)            = ""
    show (Chain a b)        = show a ++ " .* " ++ show b

data ST = ST {passes :: AssertThat, fails :: AssertThat} deriving (Show)

testWithState :: AssertThat -> State ST ()
testWithState Empty                 = return ()
testWithState a@(AssertThat d v m)
    | testAT a                      = modify (\st -> st {passes = passes st .* a})
    | otherwise                     = modify (\st -> st {fails = fails st .* a})
testWithState (Chain a1 a2)         = testWithState a1 >> testWithState a2

test :: AssertThat -> IO ()
test a = let (ST {passes = p, fails = f}) = execState (testWithState a) $ ST mempty mempty in 
            do
                putStr      "passes = "
                putStr      $ show $ count p
                putStr      ", fails = "
                putStrLn    $ show $ count f
                mapM_ putStrLn $ 
                    zipWith (\f' i -> show i ++ ": " ++ f') 
                            (filter ((>0).length) $ splitOn " .* " $ show f) 
                            [1..]

correc1 = AssertThat "a == a" 'a' $ Is $ EqualTo 'a'
correc2 = AssertThat "a == Either b a" 'a' $ Is $ Either (EqualTo 'b') (EqualTo 'a')
correI1 = AssertThat "1 /= 2" (1::Int) $ Not $ EqualTo 2
fail1   = AssertThat "a == Either b c" 'a' $ Is $ Either (EqualTo 'b') (EqualTo 'c')
fail2   = AssertThat "2 == Either 1 3" (2::Int) $ Is $ Either (EqualTo 1) (EqualTo 3)
fail5   = AssertThat "all [1..4] EqualTo 2" [(1::Int)..4] $ All $ EqualTo 2
correc5 = AssertThat "any [1..4] EqualTo 2" [(1::Int)..4] $ Any $ EqualTo 2
correc6 = AssertThat "all [1..4] < 5" [(1::Int)..4] $ All $ LessThan 5

mix     = correc1 .* correI1 .* fail1 .* correc2 .* fail2

-- shallow embedding
-- The shallow embedding is reahter easy to achieve, as we can just
-- reify the shallow embedding to a deep embedding
-- Note: If this would be a library and we wouldn't want our users to be
-- able to user the deep constructors directly, it would simply be possible to
-- only export the shallow functions
is              :: (Matchable a) => (Matcher a) -> Matcher a
is              = Is
equalTo         :: (Matchable a) => a -> Matcher a
equalTo         = EqualTo
lessThan        :: (Matchable a) => a -> Matcher a
lessThan        = LessThan
--not and either are primed to not clash with Prelude
not'            :: (Matchable a) => (Matcher a) -> Matcher a
not'            = Not      
either'         :: (Matchable a) => (Matcher a) -> (Matcher a) -> Matcher a
either'         = Either
contains        :: (Matchable a) => a -> Matcher [a]
contains        = Contains
containsString  :: String -> Matcher String
containsString  = ContainsString
any'            :: (Matchable a) => Matcher a -> Matcher [a] 
any'            = Any
all'            :: (Matchable a) => Matcher a -> Matcher [a]
all'            = All

assertThat :: forall a. (Matchable a) => String -> a -> (Matcher a) -> AssertThat
assertThat = AssertThat

correc3 = assertThat "20 == 20" (20::Int) $ is $ equalTo 20
correc4 = assertThat "True == either true false" True $ is $ either' (equalTo True) (equalTo False)
fail3   = assertThat "True /= True" True $ is $ not' $ equalTo True
fail4   = assertThat "2 == either 3 9" (2::Int) $ either' (equalTo 3) (equalTo 9)

mix2    = correc3 .* fail3 .* fail4 .* correc4

-- Test cases from assignment

allA = a1 .* a2 .* a3 .* a4 .* a5 .* a6 .* a7 .* a8 .* a9
a1 = AssertThat "a1" ((2::Int)*2) $ Is $ EqualTo $ 2+2
a2 = AssertThat "a2" ((3::Int)*3) $ EqualTo $ 3+3
a3 = AssertThat "a3" (length [0..3]) $ Not $ EqualTo 4
a4 = assertThat "a4" [(0::Int)..3] $ contains 2
a5 = AssertThat "a5" [(0::Int)..3] $ Contains 7
a6 = AssertThat "a6" [(0::Int)..3] $ Either (EqualTo [1]) (Contains 7)
a7 = AssertThat "a7" "hello world" $ ContainsString "hello"
a8 = AssertThat "a8" "hello world" $ ContainsString "world"
a9 = AssertThat "a9" "Who is afraid of red, yellow and blue" $ ContainsString "Red"

{- 
output of executing > test allA

*Assignment10 L> test allA
passes = 4, fails = 5
1: a2
2: a3
3: a5
4: a6
5: a9

-}