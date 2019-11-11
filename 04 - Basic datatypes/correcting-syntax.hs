module CorrectingSyntax where

x = (+)

f xs = w `x` 1
     where w = length xs
     
id = \x -> x

-- | Observation: This function should be named f in the exercise.
f2 (a, b) = a
