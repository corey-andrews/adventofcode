-- |

import Data.List.Split

validPassport :: [(String,String)] -> Bool
validPassport args = length (filter inRequired args)  == 7
  where required = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
        inRequired = (\x -> elem (fst x) required)

preParse :: String -> (String,String)
preParse = (\(x:y:_) -> (x,y)).splitOn ":"

main :: IO ()
main = do
  contents <- getContents
  let fields = map concat (splitWhen (==[]) (map words (lines contents)))
  let preparsed = map (map preParse) fields
  putStrLn (show (length (filter validPassport preparsed)))
