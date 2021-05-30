module Main where

import Mparseco

r1 = parse oneChar "qwerty"

r2 = parse naturalNumbers "742abc"

main = do
    putStrLn "Hello Mparseco"
    putStrLn $ "parse r1 = " ++ show r1
    putStrLn $ "parse r2 = " ++ show r2
