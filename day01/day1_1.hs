import Data.List

goalSum :: Int
goalSum = 2020

recurSumFind :: [Int] -> [Int] -> [Int]
recurSumFind (x:xs) (y:ys) | x + y == goalSum = [x,y]
                           | x + y >  goalSum = recurSumFind (x:xs) ys
                           | x + y <  goalSum = recurSumFind xs     (y:ys)
recurSumFind _ _ = []


sumFinder :: [Int] -> [Int]
sumFinder lst = recurSumFind lst (reverse lst)

fold :: Num a => (a -> a -> a) -> [a] -> a
fold f (x:y:ys) = fold f ((f x y):ys)
fold _ [x]      = x
fold _ []       = 1

main::IO ()
main = do
  contents <- getContents
  let intContents = map read (words contents) :: [Int]
  putStrLn (show (fold (*) (sumFinder (sort intContents))))
