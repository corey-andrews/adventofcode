-- |
import Data.List

seatID :: String -> Int
seatID path = calcSeatNum (getRowSeat path (0,0) rowRange seatRange)
  where
    rowRange = 64
    seatRange = 4
    getRowSeat (x:xs) (row,seat) rr sr
      | x == 'F' = getRowSeat xs (row,seat) (rr `div` 2) sr
      | x == 'B' = getRowSeat xs (row+rr,seat) (rr `div` 2) sr
      | x == 'L' = getRowSeat xs (row,seat) rr (sr `div` 2)
      | x == 'R' = getRowSeat xs (row,seat+sr) rr (sr `div` 2)
      | otherwise = (row,seat)
    getRowSeat [] rs _ _ = rs
    calcSeatNum (x, y) = 8 * x + y

main :: IO ()
main = do
  contents <- getContents
  let bPasses = lines contents
  putStrLn (show ([0..1024] \\ (map seatID bPasses)))
