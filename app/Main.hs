module Main where

import System.Environment (getArgs)

bellTriangle :: [[Integer]]
bellTriangle = iterate nextRow [1]
    where
        nextRow xs = last xs:zipWith (+) xs (nextRow xs)

bellNumbers :: [Integer]
bellNumbers = map head bellTriangle

main :: IO ()
main = do
    n <- read . head <$> getArgs
    putStrLn "Bell numbers: "
    print $ take n bellNumbers
    putStrLn "\nBell triangle:"
    mapM_ print . take n $ bellTriangle