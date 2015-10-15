module skeleton6b

import StdList, StdInt, StdChar, StdMisc, StdClass, StdString, StdFile, StdArray, Data.Maybe, Data.Map, Control.Monad, Data.Tuple, Data.Void, Data.Either, Data.Functor
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

:: Description   :== String
:: StoreID a     :== String
:: Task a        = Task (*TaskState -> *(Either String a, *TaskState))
:: *TaskState    = { console :: !*File
                   , store   :: Map String Dynamic
                   }
:: Exception     :== String

store_ :: a (StoreID a) (Map String Dynamic) -> Map String Dynamic | TC a
store_ v sid store = put sid (dynamic v) store

retrieve_ :: (StoreID a) (Map String Dynamic) -> a | TC a
retrieve_ sid store = case get sid store of
    Just (a :: a^) = a
    Just _         = abort "type error\n"
    Nothing        = abort "empty store\n"

instance Functor Task where
    fmap :: (a -> b) (Task a) -> Task b
    fmap f t = Task $ \st -> let (e, st`) = runTask t st in (f <$> e, st`)

instance Applicative Task where
    pure :: a -> Task a
    pure a = Task $ \st -> (return a, st)
    (<*>) infixl 4  :: (Task (a -> b)) (Task a) -> Task b
    (<*>) tf t = ap tf t

instance Monad Task where
    bind :: (Task a) (a -> Task b) -> Task b
    bind t f = Task $ \st -> let (e, st`) = runTask t st in case e of
        Left s  = (Left s, st`)
        Right a = runTask (f a) st`

runTask :: (Task a) *TaskState -> *(Either String a, *TaskState)
runTask (Task f) s = f s

eval :: (Task a) *File -> (Either String a, *File) | iTasksLite a
eval t console 
    # (r, {console}) = runTask t {store = newMap, console = console}
    = (r, console)
/*evalStr :: (Task a) *File -> (String, *File) | iTasksLite a
evalStr t c = let (r, c) = eval t c in case r of
    (Left ex) = (ex, c)
    (Right a) = (a, c)*/

printT :: a -> Task a | print a
printT a    = Task $ \st -> let c = st.console <<< print a in (return a, {st & console=c})
println     = printT "\n"

readlineT :: Task String
readlineT = Task $ \st -> let (r, c) = freadline st.console in (return r, {st & console=c})

viewInformation :: Description a -> Task a | iTasksLite a
viewInformation d a = printT d >>| printT ": " >>| printT a >>= \res -> println >>| return res

enterInformation :: Description -> Task a | iTasksLite a
enterInformation d = printT d >>| printT ": " >>| readlineT >>= \res -> case parse res of
    (Just r)    = return r
    _           = printT "Wrong format, try again:" >>| println >>| enterInformation d

store :: a (StoreID a) -> Task a | iTasksLite a
store val id = Task (\st -> let str = store_ val id st.store in (return val, {st & store = str}))

retrieve :: (StoreID a) -> Task a | iTasksLite a
retrieve id = Task (\st=:{store} -> ((return $ retrieve_ id store), st))

throw :: Exception -> Task a
throw ex = Task $ \st -> (Left ex, st)

tryCatch :: (Task a) (Exception -> Task a) -> Task a
tryCatch t f = Task $ \st -> case runTask t st of
    (Left ex, st`)  = runTask (f ex) st`
    res             = res

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

taskEx :: Task Int
taskEx = throw "Test exception" >>| return 5

taskRecover :: Task Int
taskRecover = printT "Throwing exception" >>| println 
    >>| (tryCatch (throw "Exception!") (\e -> printT "Recovered from Exception: " >>| printT e >>| println)) 
    >>| return 42

Start world
 #	(console, world) = stdio world
	console			 = console <<< "Welcome to iTasksLite" <<< "\n\n"
    (r, console)     = eval taskRecover console
    console          = case r of 
        (Left ex) = console <<< "\n" <<< "Task threw exception: " <<< ex <<< "\n"
        (Right a) = console <<< "\n" <<< "The result of the task is " <<< print a <<< ".\n"
	(_, world)	     = fclose console world
 = world

