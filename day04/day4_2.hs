-- |

import Data.List.Split
import Data.Char
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

inRangeInclusive :: (Ord a) => (a, a) -> a -> Bool
inRangeInclusive tup value | fst tup <= value && value <= snd tup = True
                           | otherwise                            = False

validPassport :: [(String,String)] -> Bool
validPassport args = length (filter isValid args)  == 7
  where isValid (x,y) | x == "byr" = inRangeInclusive (1920,2002) (read y :: Integer)
                      | x == "iyr" = inRangeInclusive (2010,2020) (read y :: Integer)
                      | x == "eyr" = inRangeInclusive (2020,2030) (read y :: Integer)
                      | x == "hgt" = isLegalHeight (parseHeight y)
                      | x == "hcl" = y =~ colorRegex :: Bool
                      | x == "ecl" = elem y eyeColors
                      | x == "pid" = (length y) == 9 && (foldl (\b c->b && isDigit c) True y)
                      | otherwise  = False
                      where colorRegex = "#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]"
                            eyeColors = ["amb","blu","brn","gry","grn","hzl","oth"]
                            parseHeight = (\x -> (read (filter isDigit x) :: Integer,filter isAlpha x))
                            isLegalHeight  = (\(x,y)->if (y == "cm")
                                               then inRangeInclusive(150, 193) x
                                               else inRangeInclusive(59, 76) x
                                             )

preParse :: String -> (String,String)
preParse = (\(x:y:_) -> (x,y)).splitOn ":"

main :: IO ()
main = do
  contents <- getContents
  let fields = map concat (splitWhen (==[]) (map words (lines contents)))
  let preparsed = map (map preParse) fields
  putStrLn (show (length (filter validPassport preparsed)))
