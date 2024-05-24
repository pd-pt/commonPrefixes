import Data.List (isPrefixOf)
import Control.Monad (replicateM)

commonPrefix :: String -> String -> String
commonPrefix xs ys = map fst $ takeWhile (uncurry (==)) $ zip xs ys

solution :: String -> IO ()
solution input = do
    let suffixes = map (\(_, y) -> y) [splitAt i input | i <- [0..length input]]
    let commonPrefixes = map (\s -> commonPrefix s input) suffixes
    let sumOfLengths = foldr (\s acc -> acc + length s) 0 commonPrefixes
    --print suffixes
    --print commonPrefixes
    print sumOfLengths

main :: IO ()
main = do
    n <- readLn :: IO Int
    inputs <- replicateM n getLine
    mapM_ solution inputs

