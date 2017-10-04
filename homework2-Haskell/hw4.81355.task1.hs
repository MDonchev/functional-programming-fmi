keepsInside :: [[Int]] -> (Int -> Int) -> [[Int]]
keepsInside mat1 f =
	filter (\llst -> areAllIn llst) (transpose1 mat1)
	where
		areAllIn :: [Int] -> Bool
		areAllIn lst =
			filter (\x -> not (elem x lst)) (map f lst) == []
		transpose1 :: [[Int]] -> [[Int]]
		transpose1 mat1 =
			if (null (head mat1))
			then []
			else (map head mat1) : transpose1 (map tail mat1)

main :: IO()
main = do 
	print(keepsInside [[1,0,5],[-1,0,2]] (\x -> x^2))
	print(keepsInside [[1,0,5],[-1,0,2]] (\x -> 2))
	print(keepsInside [[1,0,5],[-1,0,2]] (\x -> 5))
	print(keepsInside [[1,0,5],[-1,0,2]] (\x -> x*2))