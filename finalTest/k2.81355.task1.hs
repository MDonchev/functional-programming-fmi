import Data.List

toList :: Int -> [Int]
toList 0 = []
toList n = (n `mod` 10) : toList (n `div` 10)

findBiggest :: [Int] -> [Int]
findBiggest lst
	|incresing lst = lst
	|otherwise = findBiggest (init lst)
	where
		incresing :: [Int] -> Bool
		incresing (_:[]) = True
		incresing (x:y:ys) =
			(x < y) && incresing (y:ys)

toNumber :: [Int] -> Int
toNumber [] = 0
toNumber (x:xs) =
	x*(10 ^ (length xs)) + toNumber xs

reverseOrdSuff :: Int -> Int
reverseOrdSuff n =
	toNumber (findBiggest (toList n))
main :: IO()
main = do
	print (reverseOrdSuff 37563)
	print (reverseOrdSuff 32763)
	print (reverseOrdSuff 32567) 
	print (reverseOrdSuff 32666)
