{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE DeriveAnyClass #-}

module Assignment10 where

import Data.Functor
import Control.Monad

data Matcher a where
    Is          :: (Matchable a) => Matcher a -> Matcher a
    EqualTo     :: (Matchable a) => a -> a -> Matcher a
    LessThan    :: (Matchable a) => a -> a -> Matcher a
    Not         :: (Matchable a) => Matcher a -> Matcher Bool
    Either      :: (Matchable a, Matchable b) => Matcher a -> Matcher b -> Matcher Bool

deriving instance Show (Matcher a)

class (Show a, Ord a, Eq a) => Matchable a

deriving instance Matchable Bool
deriving instance Matchable Int 
deriving instance Matchable Char

class Test a where
    test :: a -> Bool

instance (Matchable a) => Test (Matcher a) where
    test (Is m)         = test m
    test (EqualTo x y)  = x == y
    test (LessThan x y) = x < y
    test (Not m)        = not $ test m
    test (Either x y)   = test x || test y

testE = Either ('a' `EqualTo` 'b') (True `EqualTo` False)