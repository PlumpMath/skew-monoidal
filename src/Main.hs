module Main where
import Control.Category.Monoidal.Skew

s =  X' A
t = X' A :-: I :-: X' B
u = I :-: I :-: (X' A :-: I) :-: X' B
v = I :-: u
w = t :-: t

main :: IO ()
main = do
    printTest s
    printTest t
    printTest u
    printTest v
    printTest w

printTest :: Tm -> IO ()
printTest a = do
    let b = nf a
        c = nm a
        d = evalRule c a
    putStrLn $ "Original Object:  " ++ show a
    putStrLn $ "Normal Form    :  " ++ show b
    putStrLn $ "Rewrite Rules  :  " ++ if length (show c) > 100 then (take 95 $ show c) ++ " ..." else show c
    putStrLn $ "Rewriten Object:  " ++ show d
    putStrLn ""
