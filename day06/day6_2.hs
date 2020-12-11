-- |

import Data.List
import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let regions = map concat (splitWhen (==[]) (map words (lines contents)))
  let regAns  = map (foldl intersect ['a'..'z']) regions
  putStrLn (show (foldl (+) 0 (map length regAns)))
