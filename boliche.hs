--Raphael Monteiro Consoni Bonaccorsi - 12563366
--Diogo Gianezi de Melo - 12563522

type PinCount = Int
type BowlingFrame = [PinCount]

getPinCounts :: IO [PinCount]
getPinCounts = do
  map read . words <$> getLine

partitionFrames :: [PinCount] -> [BowlingFrame]
partitionFrames [] = []
partitionFrames pinCounts = partitionFrames' pinCounts 1
  where
    partitionFrames' :: [PinCount] -> Int -> [BowlingFrame]
    partitionFrames' [] _ = []
    partitionFrames' (10 : rest) frameNumber
      | frameNumber < 10 = [10] : partitionFrames' rest (frameNumber + 1)
      | otherwise = [[10, head rest, rest !! 1]]
    partitionFrames' (first : second : rest) frameNumber
      | frameNumber == 10 = [[first, second] ++ take 1 rest]
      | first + second == 10 = [first, second] : partitionFrames' rest (frameNumber + 1)
      | otherwise = [first, second] : partitionFrames' rest (frameNumber + 1)
    partitionFrames' pinCounts _ = [pinCounts]

calculateFrameScore :: [BowlingFrame] -> Int
calculateFrameScore [] = 0
calculateFrameScore ([strike] : rest) = strike + strikeExtraScore rest + calculateFrameScore rest
calculateFrameScore ([first, second] : rest)
  | first + second == 10 = first + second + spareExtraScore rest + calculateFrameScore rest
  | otherwise = first + second + calculateFrameScore rest
calculateFrameScore (frame : rest) = sum frame + calculateFrameScore rest

strikeExtraScore :: [BowlingFrame] -> Int
strikeExtraScore (first : second : _) = sum (take 2 (first ++ second))
strikeExtraScore (first : _) = sum (take 2 first)
strikeExtraScore _ = 0

spareExtraScore :: [BowlingFrame] -> Int
spareExtraScore (first : _) = head first
spareExtraScore _ = 0

displayFrame :: BowlingFrame -> String
displayFrame [10] = "X _"
displayFrame [first, second]
  | first + second == 10 = show first ++ " /"
  | otherwise = show first ++ " " ++ show second
displayFrame [10, first, second]
  | first == 10 && second == 10 = "X X X"
  | first == 10 = "X X " ++ displayRoll second
  | first + second == 10 = "X " ++ show first ++ " /"
  | otherwise = "X " ++ show first ++ " " ++ show second
displayFrame [first, second, third]
  | first + second == 10 && third == 10 = show first ++ " / X"
  | first + second == 10 = show first ++ " / " ++ show third
  | otherwise = show first ++ " " ++ show second ++ " " ++ show third
displayFrame pinCounts = unwords (map show pinCounts)

displayRoll :: PinCount -> String
displayRoll 10 = "X"
displayRoll pinCount = show pinCount

main :: IO ()
main = do
  pinCounts <- getPinCounts
  let frames = partitionFrames pinCounts
  putStrLn $ displayFramesAndScore frames (calculateFrameScore frames)

displayFramesAndScore :: [BowlingFrame] -> Int -> String
displayFramesAndScore frames totalScore =
  unwords (map (\frame -> displayFrame frame ++ " |") (init frames)) ++ " " ++ displayFrame (last frames) ++ " | " ++ show totalScore