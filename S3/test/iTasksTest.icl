module iTasksTest

import iTasks

Start :: *World  -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_ -> test)] world

test :: Task String
test =              enterInformation "What is your name?" []
       >>= \name -> viewInformation "Success!" [] ("Congratulations " +++ name +++ "! You got iTasks running.")

