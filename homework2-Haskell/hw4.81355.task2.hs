matrixInfList :: [[a]] -> [a]
matrixInfList matrix =
	endlessRepeat (transpose' matrix)
	where
		endlessRepeat :: [a] -> [a]
		endlessRepeat lst = cycle lst
		transpose' :: [[a]] -> [a]
		transpose' [] = []
		transpose' lst =
			(head (transpose1 lst)) ++ transpose' (tail (transpose1 lst))
			where
				transpose1 :: [[a]] -> [[a]]
				transpose1 mat1 =
					if (null (head mat1))
					then []
					else (map head mat1) : transpose1 (map tail mat1)
	
main :: IO()
main = do
	print (take 5 (matrixInfList [[1,2], [3,4], [5,6]]))
	print (take 10 (matrixInfList [[1,2], [3,4], [5,6]]))
	print (take 15 (matrixInfList [[1,2], [3,4], [5,6]]))
	print (take 20 (matrixInfList [[1,2], [3,4], [5,6]]))
