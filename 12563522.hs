--diogo gianezi de melo n12563522
--Raphael Monteiro Consoni Bonaccorsi n12563366


-- Função usada para verificar se um número é primo
isPrime :: Int -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..(floor $ sqrt $ fromIntegral n)]




-- Função para encontrar o próximo número primo maior ou igual a x
nextPrime :: Int -> Int
nextPrime x | isPrime x = x
            | otherwise = nextPrime (x + 1)
            
            

-- Função pricipal para encontrar o comprimento do maior intervalo entre dois números primos consecutivos
primeIntervalLength :: Int -> Int -> Int
primeIntervalLength x y | x >= y = 0
                         | otherwise = let p1 = nextPrime x
                                           p2 = nextPrime (p1 + 1)
                                       in if p2 <= y
                                          then max (p2 - p1) (primeIntervalLength p2 y)
                                          else 0

main :: IO ()
main = do
    inputX <- getLine
    inputY <- getLine
    let x = read inputX
        y = read inputY
    if x >= y
        then putStrLn "x deve ser menor que y!"
        else do
            let maxLength = primeIntervalLength x y
            print maxLength
