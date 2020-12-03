-- |

treesHit :: [[Bool]] -> (Int,Int) -> Int
treesHit slope path = step (0,0) 0
  where
    slp = slope
    pth  = path
    step start hit | snd start == length slp =  hit
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
  putStrLn (show (treesHit boolSlope (3,1)))
