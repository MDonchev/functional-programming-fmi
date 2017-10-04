import Data.List

toList :: Int -> Int -> [Int]
toList a b =
	if (a>b) 
		then []
		else a : toList (a+1) b
inverseFun :: (Int->Int) -> (Int->Int) -> Int -> Int -> Bool
inverseFun f g a b =
	(map (\x -> f (g x)) (toList a b) == (toList a b)) && (map (\x -> g (f x)) (toList a b) == (toList a b))
main :: IO()
main = do
	print (inverseFun (\x -> x+1) (\x -> x-1) 5 10)
	print (inverseFun (\x -> x*x) (\x -> x^3) 0 1)
	print (inverseFun (\x -> x+1) (\x -> x+2) 0 1)