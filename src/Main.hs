-- | Main entry point to the application.
module Main where

-- | The main entry point.
main :: IO ()
primes n = [p | p <- [1..n], p `notElem` [a*b | a <- [2..n], b <- [2..n]]]
main = print (primes 25)
