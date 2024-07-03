{-# LANGUAGE InstanceSigs #-}
import Data.Char
import Data.List
import Data.Foldable

scalarProd :: Num a => [a] -> [a] -> a
scalarProd xs ys = sum [x*y | x <- xs, y <- ys]

isPerfect :: Integer -> Bool
isPerfect a = a*2 == sum [x | x <- [1..a], a `mod` x == 0]

initials :: [String] -> String
initials ns = concat [(take 1 n) ++ "." | n <- ns]

countLowerCase :: String -> Int
countLowerCase text = length [ c | c <- text, c `elem` ['a'..'z']]

data Clock = Tick Int | Tock Int | Alarm deriving (Show)

instance Eq Clock where
  (==) :: Clock -> Clock -> Bool
  Tick c1 == Tick c2 = c1 == c2
  Tock c1 == Tock c2 = c1 == c2
  Tick c1 == Tock c2 = c1 == c2
  Tock c1 == Tick c2 = c1 == c2
  Alarm == Alarm = True
  Alarm == Tick c = False
  Tick c == Alarm = False
  Alarm == Tock c = False
  Tock c == Alarm = False
  

identicalClock :: Clock -> Clock -> Bool
identicalClock Alarm Alarm = True
identicalClock (Tick x) (Tick y) = x == y
identicalClock (Tock x) (Tock y) = x == y
identicalClock _ _ = False

countDown :: Int -> [Clock]
countDown n
 | n == 0 = [Alarm]
 | even n = Tick n : countDown (n-1)
 | odd n  = Tock n : countDown (n-1)
 
sumEveryEvenThird :: [Int] -> Int
sumEveryEvenThird xs = sum [ xs !! (i-1) | i <- [1..(length xs)], i `mod` 3 == 0, even (xs !! (i-1))]

kLines :: String -> [Int]
kLines text = [ i | i <- [1..(length linesT)], containsK (linesT !! (i-1))]
  where
   linesT = lines text
   containsK line = not $ null [ 1 | c <- line, c == 'k']
  

{--isomorph1 :: Maybe (Bool, a) -> Either a (Maybe a)
isomorph1 Nothing = Right Nothing
isomorph1 (Just (Left x)) = Left x
isomorph1 (Just (Right x)) = Right (Just x)

isomorph2 :: Either a (Maybe a) -> Maybe (Bool, a)
isomorph2 Right Nothing = Nothing
isomorph2 Left (Just x) = Just x
isomorph2 --}

stern :: Int -> [Int]
stern = undefined

{--stern :: Int -> [Int]
stern 0 = [0]
stern 1 = 1 : stern 0
stern n = if even n 
              then stern (n `div` 2) : stern (n-1)
			  else (stern ((n-1) `div` 2) + stern (((n-1) `div` 2)+1)) : stern (n-1)
--}
data Fuel = MHEV | HEV | PHEV | EV | Gasoline | Diesel deriving (Eq, Show)
evs :: [(String, String,Fuel)] -> [String]
evs xs = [ tipus | (rendsz, tipus, fuel) <- xs, valid rendsz, fuel == EV]
  where
    valid (rendsz::String) =  (length rendsz) == 8
                             && (length [1 | i <- [0..3], isLetter (rendsz !! i)] == 4) 
                             && (rendsz !! 4) == '-'
                             && (length [1 | i <- [5..7], (rendsz !! i) `elem` ['0'..'9']] == 3)