module Main where

import Poly




main :: IO ()
main = putStrLn . show $ normalForm $  PolyFact (PolySum []) (PolySum [(5,2), (6, 1), (7, 0)])

