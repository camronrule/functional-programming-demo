-- Write printAMessage here
printAMessage :: Show a => a -> IO ()
printAMessage x = putStrLn (show x)

-- Write division here
division :: Double -> Double -> Maybe Double
division x y
    | y == 0 = Nothing
    | otherwise = Just (x / y)
    
-- Write factorial here
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Write factList here
factList :: Int -> [Int]
factList n = [factorial x | x <- [0..n-1]]

-- Write merge here
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge(x:xs) ys

main :: IO ()
main = do
    printAMessage "TESTING"
    print(division 4 2)
    print(factorial 3)
    print(factList 3)
    print(merge [10,30,50,70] [0,20,40,60])