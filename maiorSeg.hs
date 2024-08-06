import Data.List (unfoldr)

maxCrescente :: [Int] -> Int
maxCrescente = snd . foldl f (([], 0), 0)
    where
        f ((segAtual, lenAtual), maxLen) x
            | null segAtual || x > last segAtual = ((segAtual ++ [x], lenAtual + 1), max maxLen (lenAtual + 1))
            | otherwise = (([x], 1), maxLen)

main :: IO ()
main = do
    input <- getLine
    let values = unfoldr (parseValue . dropWhile (== ' ')) input
    let result = maxCrescente values
    print result

parseValue :: String -> Maybe (Int, String)
parseValue "" = Nothing
parseValue str = case reads str of
    [(val, rest)] -> Just (val, rest)
    _ -> Nothing
