import Data.List
import Data.Ord (comparing)
import System.IO

-- Define a data structure to hold country data
data CountryData = CountryData
  { country :: String,
    confirmed :: Int,
    deaths :: Int,
    recovery :: Int,
    active :: Int
  }
  deriving (Show)

-- Function to parse a line of CSV into a CountryData object
parseLine :: String -> Maybe CountryData
parseLine line = case split ',' line of
  [c, con, d, r, a] -> Just $ CountryData c (read con) (read d) (read r) (read a)
  _ -> Nothing

-- Function to split a string by a given delimiter
split :: Char -> String -> [String]
split delim s = case dropWhile (== delim) s of
  "" -> []
  s' -> w : split delim s''
    where
      (w, s'') = break (== delim) s'

-- Function to read a CSV file and parse its contents into a list of CountryData
-- read the header!!!!!!!!!! (estavamos pulando a primeira linha, dando numero inferior na soma dos active)
readCSV :: FilePath -> IO [CountryData]
readCSV filePath = do
  contents <- readFile filePath 
  return $ map parseMaybeLine (lines contents) 
  where
    parseMaybeLine line = case parseLine line of
      Just cd -> cd
      Nothing -> error $ "Invalid line: " ++ line

main :: IO ()
main = do
  -- Read input from the user
  input <- getLine
  let [n1, n2, n3, n4] = map read (words input) :: [Int]

  -- Read the CSV file
  countryData <- readCSV "dados.csv"

  -- Calculate and print the sum of active cases for countries with confirmed cases >= n1
  let activeSum = sum [active cd | cd <- countryData, confirmed cd >= n1]
  print activeSum

  -- Find the top N2 countries by active cases and then sort the top N3 of them by confirmed cases
  let topN2ActiveCountries = take n2 $ sortBy (flip (comparing active)) countryData
  let sortedByConfirmed = take n3 $ sortBy (comparing confirmed) topN2ActiveCountries
  let deathsSum = sum [deaths cd | cd <- sortedByConfirmed]
  print deathsSum

  -- Find the top N4 countries by confirmed cases and print their names sorted alphabetically
  let topN4ConfirmedCountries = take n4 $ sortBy (flip (comparing confirmed)) countryData
  let sortedNames = sort [country cd | cd <- topN4ConfirmedCountries]
  mapM_ putStrLn sortedNames
