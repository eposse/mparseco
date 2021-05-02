module Main where

import Mparseco

r = parse oneChar "qwerty"

main = do
    putStrLn "Hello Mparseco"
    putStrLn $ "parse result = " ++ show r
