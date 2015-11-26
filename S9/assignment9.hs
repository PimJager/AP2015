{-#LANGUAGE GADTs #-}
{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE FlexibleInstances #-}

module Assignment9 where

import Prelude hiding (read, print)
import Data.Functor
import Control.Monad
import Data.Map as M
import qualified Data.List as L

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
    (:.)            :: (HasVal a, HasVal b) => Expression a -> Expression b -> Expression b

deriving instance Show (Expression a)

data Op       = (:+) | (:-) | (:*)  deriving (Show)
type Set      = Expression [Int]
type Element  = Expression Int
type Ident    = String
data Val      = I Int | S [Int]  deriving (Show)

-- TODO: instances for the operators +, -, *
instance Num (Element) where
    e1 + e2         = Oper e1 (:+) e2
    e1 - e2         = Oper e1 (:-) e2
    e1 * e2         = Oper e1 (:*) e2
    abs e1          = error "abs not defined for Expression"
    signum e1       = error "signum not defined for Expression"
    fromInteger i   = Integer $ fromInteger i

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

-- We need to be able to pack and eval vals for the store
class HasVal a where
    unVal :: Val -> Sem a
    mkVal :: a -> Val
instance HasVal Int where
    unVal (I i) = return i
    unVal _     = fail "Expected Element but found something else" --for users of DSL Int=Element
    mkVal i     = I i
instance HasVal [Int] where
    unVal (S s) = return s
    unVal _     = fail "Expected Set but found something else" -- for users of DSL [Int]=Set
    mkVal s     = S s    

-- ====Evaluation

eval :: HasVal a => Expression a -> Sem a
eval New                    = return []
eval (Insert e s)           = eval e >>= \e' -> eval s >>= return . (e':)
eval (Delete e s)           = eval e >>= \e' -> eval s >>= return . L.filter ((/=)e') 
eval (Variable k)           = read k >>= unVal
eval (Union s1 s2)          = opS s1 s2 L.union
eval (Difference s1 s2)     = opS s1 s2 (L.\\)
eval (Intersection s1 s2)   = opS s1 s2 L.intersect
eval (Integer i)            = return i
eval (Size s)               = eval s >>= return . length
eval (Oper e1 o e2)         = eval e1 >>= \e1' -> eval e2 >>= \e2' -> return $ (op o) e1' e2'
eval ((:=) k e)             = eval e >>= store k . mkVal >>= unVal
eval ((:.) e1 e2)           = eval e1 >> eval e2

opS :: Set -> Set -> ([Int] -> [Int] -> [Int]) -> Sem [Int]
opS s1 s2 f = f <$> eval s1 <*> eval s2       
op :: Op -> (Int -> Int -> Int)
op (:+) = (+)
op (:-) = (-)
op (:*) = (*)

evalExpression :: HasVal a => Expression a -> State -> Either String a
evalExpression expr state = fst $ runExpression expr state

evalExpression' :: HasVal a => Expression a -> Either String a
evalExpression' e = evalExpression e M.empty

runExpression :: HasVal a => Expression a -> State -> (Either String a, State)
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
print' ((:=) k e)           c   = (k : ("=" : print' e c))
print' ((:.) e1 e2)         c   = (print' e1 (";\n" : print' e2 c))

printOpS :: Expression a -> Expression a -> String -> [String] -> [String]
printOpS e1 e2 f c = ("(" : print' e1 (")" : (f : ("(" : print' e2 (")" : c)))))

printOp :: Op -> String
printOp (:+) = "+"
printOp (:-) = "-"
printOp (:*) = "*"

-- Example programs

e1 = Insert (4+5) $ Insert 18 $ Delete 4 $ Insert 4 New
e2 =    "x" := Insert 5 New :. 
        Size (Variable "x")
-- no typechecking on variables :(
e3 =    ("x" := New) :.
        ("y" := Integer 5) :.
        Oper (Variable "x") (:+) (Variable "y") -- use Oper to explicitly force Element