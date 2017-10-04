main :: IO()
main = do
	print (sumUnique [[1,2,3,2], [-4,-4], [5]])
	print (sumUnique [[1,2,3,2,1], [-4,5,-4], [5,7]])


sumUnique :: [[Int]] -> Int
sumUnique [] = 0
sumUnique lst = 
	sumLst (head (map unique lst)) + sumUnique (tail (map unique lst))
		where
			unique :: [Int] -> [Int]
			unique lst =
				filter (\ x -> (count x lst) == 1) lst
					where
						count :: Int->[Int]->Int
						count _ [] = 0
						count n (x:xs) =
							if (n == x)
							then (1 + count n xs)
							else count n xs
			sumLst :: [Int] -> Int
			sumLst [] = 0
			sumLst (x:xs) =
				x + sumLst xs