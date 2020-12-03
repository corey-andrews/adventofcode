-- |

treesHit :: [[Bool]] -> (Int,Int) -> Int
treesHit slope path = step (0,0) 0
  where
    slp = slope
    pth  = path
    step start hit | snd start >= length slp =  hit
                   | otherwise = step
                                 ((fst start + fst pth) `mod` (length (slp !! 0)), snd start + snd pth)
                                 (treeCheck start hit)
                                 where
                                   treeCheck strt ht | (slp !! snd strt) !! fst start = ht + 1
                                                     | otherwise = ht

main :: IO ()
main = do
  contents <- getContents
  let boolSlope = map (map (== '#'))  (lines contents)
  putStrLn (show (foldr (*) 1 (map (treesHit boolSlope) [(1,1),(3,1),(5,1),(7,1),(1,2)])))
