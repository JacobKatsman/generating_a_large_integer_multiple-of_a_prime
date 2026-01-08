module Main where
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Control.Monad.Random (evalRandIO, getRandomR)
--import Data.List (intercalate)

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

-- в бинарном формате
getRandomMonad ::IO Integer
getRandomMonad = evalRandIO $ getRandomR ((randomMin customRange), (randomMax customRange))

--  вычислить случайное a число  для  (a ^ r - a)  где  a не делит r
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
                    
main :: IO ()
main = do
    putStrLn "Генерация кратного простому числу большого целого числа"
    putStrLn "Введите простое число (2,3,5,7 или 11) это степень числа, по условию:"
    input <- getLine
    let power1 = read input :: Integer
    -- цикл 
    mapM_ (\i -> do
         randomNumber  <- loopFunction power1
         if (power1  < 2) || (power1  > 11)
         then
          putStrLn $ "степень должна быть простым числом из списка и положительным числом!"
         else 
          printFunction i power1 randomNumber  -- игнорируем возвращаемое значение
         )[1..10]
    putStrLn "=== end ==="
   
