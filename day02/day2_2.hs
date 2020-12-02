-- |

import Data.List.Split

data ParsedLine = ParsedLine { constraintRange  :: (Int, Int)
                             , constraintSymbol :: Char
                             , password         :: String
                             } deriving (Show)

inRangeInclusive :: (Ord a) => (a, a) -> a -> Bool
inRangeInclusive tup value | fst tup <= value && value <= snd tup = True
                           | otherwise                            = False

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && (not (a && b))

getConstraintSymbol :: String -> Char
getConstraintSymbol str = head str

getConstraintRange :: String -> (Int, Int)
getConstraintRange str = (read head :: Int, read tail :: Int)
                        where head:tail:xs = splitOn "-" str

parseLine :: String -> ParsedLine
parseLine line = ParsedLine { constraintRange = getConstraintRange first
                            , constraintSymbol = getConstraintSymbol second
                            , password         = third
                            }
                 where first:second:third:ns = words line

isValidPass :: ParsedLine -> Bool
isValidPass pl = xor ((password pl) !! ((fst (constraintRange pl)) - 1) == constraintSymbol pl)
                     ((password pl) !! ((snd (constraintRange pl)) - 1) == constraintSymbol pl)

  --             inRangeInclusive (constraintRange pl) occurrences
  --              where occurrences = length $ filter (== (constraintSymbol pl)) (password pl)

main :: IO ()
main = do
  contents <- getContents
  let parsewords = map parseLine (lines contents)
  putStrLn (show (length (filter isValidPass parsewords)))
