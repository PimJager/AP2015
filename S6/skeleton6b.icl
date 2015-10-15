module skeleton6b

import StdList, StdInt, StdChar, StdMisc, StdClass, StdString, StdFile, StdArray, Data.Maybe, Data.Map, Control.Monad, Data.Tuple, Data.Void, Data.Functor.Identity, Control.Monad.Trans, Data.Func
import qualified Text
from Text import class Text, instance Text String
from Data.Func import $

class print a :: a -> String

instance print Void where print _ = "Void"
instance print String where print s = s
instance print Int where print i = toString i
instance print [a] | print a where print l = 'Text'.join ", " (map print l)

class parse a :: String -> Maybe a

instance parse Void where parse _ = Just Void

instance parse String where
    parse s = let len = size s
              in  Just (if (select s (len-1) == '\n') (s % (0, len - 2)) s) // remove newline
instance parse Int where
	parse s
		# len = size s
		| len > 0
			# s = if (select s (len-1) == '\n') (s % (0, len - 2)) s // remove newline
			# i = toInt s
			| toString i == s
				= Just i
			= Nothing

instance parse [a] | parse a where parse s = foldr (\xs list -> maybe Nothing (\e -> fmap (\l -> [e:l]) list) (parse xs)) (Just []) ('Text'.split "," s)

class iTasksLite a | print a & parse a & TC a

:: Description  :== String
:: StoreID a    :== String
//:: Task a = Task (*TaskState -> *(a, *TaskState)) 
:: TaskT m a    = Task (*TaskState -> m *(a, *TaskState)) | Monad m //basicaly the StateT monad
:: Task a       :== TaskT Identity a
:: *TaskState   = { console :: !*File
                   , store   :: Map String Dynamic
                   }

store_ :: a (StoreID a) (Map String Dynamic) -> Map String Dynamic | TC a
store_ v sid store = put sid (dynamic v) store

retrieve_ :: (StoreID a) (Map String Dynamic) -> a | TC a
retrieve_ sid store = case get sid store of
    Just (a :: a^) = a
    Just _         = abort "type error\n"
    Nothing        = abort "empty store\n"

instance MonadTrans TaskT where
    liftT m = Task $ \st -> m >>= \a -> return (a, st)

instance Functor (TaskT m) | Monad m where
    fmap f t = liftM f t

instance Applicative (TaskT m) | Monad m where
    pure a = Task (\st -> (a, st))
    (<*>) tf t = ap tf t

instance Monad (TaskT m) | Monad m where
    bind t f = Task $ \st -> let (r, st1) = runTask t st in runTask (f r) st1
    bind t f = Task $ \st -> 

runTask :: (Task a) *TaskState -> *(a, *TaskState)
runTask t s = runIdentity o runTaskT t s

runTaskT :: (TaskT m a) *TaskState -> m *(a, *TaskState) | Monad m
runTaskT (Task f) s = f s

eval :: (Task a) *File -> (a, *File) | iTasksLite a
eval t console 
    # (r, {console}) = runTask t {store = newMap, console = console}
    = (r, console)

printT :: a -> Task a | print a
printT a = Task $ \st -> let c = st.console <<< print a in (a, {st & console=c})
println = printT "\n"

readlineT :: Task String
readlineT = Task $ \st -> let (r, c) = freadline st.console in (r, {st & console=c})

viewInformation :: Description a -> Task a | iTasksLite a
viewInformation d a = printT d >>| printT ": " >>| printT a >>= \res -> println >>| return res

enterInformation :: Description -> Task a | iTasksLite a
enterInformation d = printT d >>| printT ": " >>| readlineT >>= \res -> case parse res of
    (Just r)    = return r
    _           = printT "Wrong format, try again:" >>| println >>| enterInformation d

store :: a (StoreID a) -> Task a | iTasksLite a
store val id = Task $ \st -> let str = store_ val id st.store in (val, {st & store = str})

retrieve :: (StoreID a) -> Task a | iTasksLite a
retrieve id = Task $ \st=:{store} -> ((retrieve_ id store), st)

task0 :: Task Int
task0 = return 42

task1 :: Task Int
task1 = viewInformation "The answer is" 42

task2 :: Task Int
task2 =
     enterInformation "Enter the answer"
 >>= viewInformation "The answer is"

task3 :: Task Int
task3 =
        store 1 intStore
    >>| retrieve intStore
where
    intStore :: StoreID Int
    intStore = "intStore"

task3Fail :: Task Int
task3Fail = retrieve intStore
where
    intStore :: StoreID Int
    intStore = "intStore"

task4 :: Task [String]
task4 =
        store [] ideaStore
    >>| addIdea
where
    addIdea =
                      retrieve ideaStore
        >>= \ideas -> viewInformation "All ideas" ideas
        >>|           enterInformation "Enter new idea"
        >>= \idea  -> store (ideas ++ [toString (length ideas+1) +++ ". " +++ idea]) ideaStore
        >>|             retrieve ideaStore
        >>= \ideas1 -> viewInformation "All ideas" ideas1
        >>|           addIdea

    ideaStore :: StoreID [String]
    ideaStore = "ideas"

Start world
 #	(console, world) = stdio world
	console			 = console <<< "Welcome to iTasksLite" <<< "\n\n"
    (r, console)     = eval task3 console
    console          = console <<< "\n" <<< "The result of the task is " <<< print r <<< ".\n"
	(_, world)	     = fclose console world
 = world

