module Main where
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Control.Monad.Random (evalRandIO, getRandomR)
import System.Exit (exitWith, exitFailure, exitSuccess)


{-
  Этот скрипт получает большие целые числа которые делятся на
  2,3,5,7,11,13,17,19,23,...  и другие простые числа. Основание подбирается случайно
  в диапазоне  указанном в константах
  цель: получить большие случайные числа - кратные 2,3,5,7,11,13,17,19,23,...
-}

-- Тип для представления интервала (константа)
data RandomRange = RandomRange
    { randomMin :: Integer
    , randomMax :: Integer
    } deriving (Show, Eq)

customRange :: RandomRange
customRange = RandomRange 1000 9999  -- диапазон случайных значений

getRandomMonad ::IO Integer
getRandomMonad = evalRandIO $ getRandomR ((randomMin customRange), (randomMax customRange))

--  вычислить случайное a число  для  (a ^ r - a)  где  r не делит a
calcDivNumber :: Integer -> Integer -> Integer
calcDivNumber r base = 
                       let rs = base in
                           rs ^ r - rs -- малая теорема ферма 

--  преобразование к бинарному формату
binaryString :: Integer ->  String
binaryString n = showIntAtBase 2 intToDigit n ""

-- проверяем что бы основание не делило степень в малой теореме ферма 
loopFunction :: Integer -> IO Integer
loopFunction power1 = do
            rn <- getRandomMonad
            if  rn `mod` power1 /= 0
                then
                    return  rn
                else do
                    loopFunction power1  -- рекурсия пока не подберется нужное число

-- напечатаем результат 
printFunction :: Integer -> Integer -> Integer -> IO ()
printFunction i power1 randomNumber = do 
               let result = calcDivNumber power1 randomNumber
               putStrLn $ "N:" ++ show(i) ++ "number ::" ++ show (result) ++ "; binary :: " ++ binaryString result ++ " "
               
-- стандартная проверка входного параметра показателя на простоту               
isPrime :: Integer -> Bool
isPrime n
    | n <= 1    = False
    | n == 2    = True
    | even n    = False
    | otherwise = not (any (\d -> n `mod` d == 0) [3, 5 .. limit])
    where limit = floor (sqrt (fromIntegral n))
                 
main :: IO ()
main = do
    putStrLn "Генерация кратного простому числу большого целого числа"
    putStrLn "Введите простое число (2,3,5,7 или 11) это степень числа, по условию:"
    input <- getLine
    let power1 = read input :: Integer
    -- цикл
    if (not (isPrime power1))
    then  
      putStrLn "показатель должен быть простым и положительным числом!"
    else  
      mapM_ (\i -> do
         randomNumber  <- loopFunction power1
         printFunction i power1 randomNumber
         )[1..10]
    putStrLn "=== end ==="
   
