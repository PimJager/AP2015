module assignment10

import StdList, StdInt, StdMisc, Data.Tuple, StdClass, iTasks._Framework.Generic, Text.JSON, 
    Data.Functor, Control.Applicative, Control.Monad, Data.Void, Data.Either

:: TestCase a
    = Is                    a
    | (EqualTo)  infixl 2   a a
    | (LessThen) infixl 2   a a
    | Not                   (TestCase a)
    | (Either)   infixl 2   (TestCase a) (TestCase a)
    | (Chain)    infixl 2   (TestCase a) (TestCase a)

instance * (TestCase a) where
    (*) t1 t2 = t1 Chain t2

testTC :: TestCase a -> Bool 





Start = (5 EqualTo 5) * (3 EqualTo 3)
