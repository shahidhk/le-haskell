module Lib
    ( someFunc
    , safeDiv
    , factorial
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc123"

safeDiv x y = 
    let q = div x y
    in if y == 0 then 0 else q

factorial n = if n > 1
              then n * factorial (n - 1)
              else 1

{-
  Shape parser 

  Input: 
    "T:2,3:5,8:9,12" -> Triangle with 3 vertices
  Output:
    Maybe Shape Triangle Point Point Point

  Input:
    "S:1,1:5,5" -> Square with 2 diagonal vertices
  Output:
    Maybe Shape Square Point Point
-}
