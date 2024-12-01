import Data.Tree (flatten)
import Data.List (sort, transpose)

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)

main :: IO ()
main = do
    -- fileContents <- readFile "example.txt"
    fileContents <- readFile "input.txt"
    let preparedContents = parseInput fileContents
    print (solvePart1 preparedContents)
    print (solvePart2 preparedContents)

parseInput :: String -> ([Int], [Int])
parseInput x = toTuple $ map sort $ transpose $ map (map read . words) $ lines x

solvePart1 :: ([Int], [Int]) -> Int
solvePart1 x = sum $ map abs $ uncurry (zipWith (-)) x

solvePart2 :: ([Int], [Int]) -> Int
solvePart2 ([], _) = 0
solvePart2 (a:as, bs) = (a * frequency) + solvePart2 (as, bs) where
    frequency = length $ filter (== a) bs