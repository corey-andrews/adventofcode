import Data.List

goalSum :: Int
goalSum = 2020

recurSumFind :: [Int] -> [Int] -> Int -> [Int]
recurSumFind (x:xs) (y:ys) goal | y < x         = []
                                | x + y == goal = [x,y]
                                | x + y >  goal = recurSumFind (x:xs) ys     goal
                                | x + y <  goal = recurSumFind xs     (y:ys) goal
recurSumFind _ _ _ = []


sumFinder :: [Int] -> [Int]
sumFinder (x:xs) | result == [] = sumFinder xs
                 | otherwise    = x:result
                   where goal   = goalSum - x
                         result = recurSumFind xs (reverse xs) goal
sumFinder _ = []

fold :: Num a => (a -> a -> a) -> [a] -> a
fold f (x:y:ys) = fold f ((f x y):ys)
fold _ [x]      = x
fold _ []       = 1

main::IO ()
main = do
  contents <- getContents
  let intContents = map read (words contents) :: [Int]
  putStrLn (show (fold (*) (sumFinder (sort intContents))))
