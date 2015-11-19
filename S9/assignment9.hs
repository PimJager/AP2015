{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE FlexibleInstances #-}

module Assignment9 where

import Prelude hiding (read, print)
import Data.Functor
import Control.Monad
import Data.Map as M
import qualified Data.List as L

data Op       = (:+) | (:-) | (:*)  deriving (Show)
type Set      = Expression [Int]
type Element  = Expression Int
type Ident    = String
data Val      = I Int | S [Int]  deriving (Show)

data Expression a where 
    New             :: Set
    Insert          :: Element -> Set -> Set
    Delete          :: Element -> Set -> Set
    Variable        :: Ident -> Expression a
    Union           :: Set -> Set -> Set
    Difference      :: Set -> Set -> Set
    Intersection    :: Set -> Set -> Set
    Integer         :: Int -> Element
    Size            :: Set -> Element
    Oper            :: Element -> Op -> Element -> Element
    (:=)            :: Ident -> Expression a -> Expression a 

deriving instance Show (Expression a)


-- === State
type State = Map Ident Val 

-- === semantics
newtype Sem a = Sem {runSem :: State -> (Either String a, State)} 

instance Functor Sem where
    fmap f s    = Sem $ \st -> let (r, st') = runSem s st in (f <$> r, st')

instance Applicative Sem where
    pure a      = Sem $ \st -> (pure a, st)
    (<*>) sf s  = ap sf s

instance Monad Sem where
    return a    = pure a
    (>>=) s f   = Sem $ \st -> let (r, st') = runSem s st in case r of
        Left e  -> (Left e, st')
        Right a -> runSem (f a) st'
    fail e      = Sem $ \st -> (Left e, st) 

store :: Ident -> Val -> Sem Val
store k v   = Sem $ \st -> (pure v, M.insert k v st)

read :: Ident -> Sem Val
read k      = Sem $ \st -> case M.lookup k st of
    Just v  -> (pure v, st)
    Nothing -> runSem (fail $ concat ["Unknown identifier: ", k]) st

-- Evaluation ====
class Unpack a where
    unpack :: (Expression a) -> Sem a

instance Unpack [Int] where
    unpack s = eval s >>= \rs -> case rs of S rs' -> return rs'

instance Unpack Int where
    unpack e = eval e >>= \rs -> case rs of I rs' -> return rs'

eval :: Expression a -> Sem Val
eval New                    = return $ S []
eval (Insert e s)           = unpack e >>= \e' -> unpack s >>= \s' -> return $ S (e':s')
eval (Delete e s)           = unpack e >>= \e' -> unpack s >>= \s' -> 
                                    return $ S $ L.filter ((/=)e') s'
eval (Variable k)           = read k
eval (Union s1 s2)          = opS s1 s2 L.union
eval (Difference s1 s2)     = opS s1 s2 (L.\\)
eval (Intersection s1 s2)   = opS s1 s2 L.intersect
eval (Integer i)            = return $ I i
eval (Size s)               = unpack s >>= \s' -> return $ I $ length s'
eval (Oper e1 o e2)         = unpack e1 >>= \e1' -> unpack e2 >>= \e2' -> return $ I $ (op o) e1' e2'
eval ((:=) k e)             = eval e >>= \e' -> store k e'

opS :: Set -> Set -> ([Int] -> [Int] -> [Int]) -> Sem Val      
opS s1 s2 f = unpack s1 >>= \s1' -> unpack s2 >>= \s2' -> return $ S $ f s1' s2'
op :: Op -> (Int -> Int -> Int)
op (:+) = (+)
op (:-) = (-)
op (:*) = (*)

evalExpression :: Expression a -> State -> Either String Val
evalExpression expr state = let (r, _) = runExpression expr state in r

evalExpression' :: Expression a -> Either String Val
evalExpression' e = evalExpression e M.empty

runExpression :: Expression a -> State -> (Either String Val, State)
runExpression expr state = runSem (eval expr) state

-- === Printing
print :: Expression a -> String
print e = concat $ print' e []

print' :: Expression a -> [String] -> [String]
print' New                  c   = ("∅":c)
print' (Insert e s)         c   = print' e (":" : print' s c)
print' (Delete e s)         c   = ("{" : print' e ("}∖(" : print' s (")" : c)))
print' (Variable k)         c   = (k:c)
print' (Union s1 s2)        c   = printOpS s1 s2 "∪" c
print' (Difference s1 s2)   c   = printOpS s1 s2 "∖" c
print' (Intersection s1 s2) c   = printOpS s1 s2 "∩" c
print' (Integer i)          c   = (show i : c)
print' (Size s)             c   = ("|" : print' s ("|" : c))
print' (Oper e1 op e2)      c   = ("(" : print' e1 ((printOp op) : print' e2 (")" : c)))
print' ((:=) k e)             c   = (k : ("=" : print' e c))

printOpS :: Expression a -> Expression a -> String -> [String] -> [String]
printOpS e1 e2 f c = ("(" : print' e1 (")" : (f : ("(" : print' e2 (")" : c)))))

printOp :: Op -> String
printOp (:+) = "+"
printOp (:-) = "-"
printOp (:*) = "*"

-- Example programs

