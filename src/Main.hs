module Main where

import Poly
import CASkellParser
import CASkellEval

import System.Environment



main :: IO () 
main = do
    (fileLoc : _) <- getArgs 
    p <- parseFromFile progParser fileLoc
    case p of         
        Left err   -> putStrLn $ show err 
        Right prog -> do {
            -- putStrLn $ show $ prog ; 
            putStrLn $ unlines $ map show (evalProg prog)
        }
    -- putStrLn "begin"
    -- -- putStrLn . show $ normalForm $  (PolySum [(5,5), (6, 1), (7, 0)]) --
    -- case parseprog "y := (x^2 - 2) * (x^2 - 2) ; z := (x^2 - 2) ; print(y / z) ; print(y % z)" of 
    --     Left err   -> putStrLn $ show err 
    --     Right prog -> do {
    --         putStrLn $ show $ prog ; 
    --         putStrLn $ show (evalProg prog)
    --     }


    -- putStrLn . show $ prog
    -- putStrLn . show $ normalForm $  PolyFact (PolySum []) (PolySum [(5,2), (6, 1), (7, 0)])

f :: Prog -> Polynomial
f (P xs) = ff $ head xs
ff :: Stmt -> Polynomial
ff (Assign _ (Const x)) = x 