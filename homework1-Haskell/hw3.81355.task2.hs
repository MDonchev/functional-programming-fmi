main :: IO()
main = do 
	let lib = [("SICP",1996),
		("Learn You a Haskell for Great Good",2011),
		("Real World Haskell",2008),
		("Programming in Haskell", 2011)] in do
	print (findUniques lib)
	print (longestTitleYear lib)


--a)
findUniques :: [(String,Int)] -> [String]
findUniques [] = []
findUniques lst =
	map fst (filter (\ x -> countYear x lst == 1) lst)
	where
		countYear :: (String,Int) -> [(String,Int)] -> Int
		countYear _ [] = 0
		countYear book lst =
			if (snd book == snd (head lst))
			then 1+ countYear book (tail lst)
			else countYear book (tail lst)
--b)
longestTitleYear :: [(String,Int)] -> Int
longestTitleYear lst = 
	snd (longestName lst)
	where
		longestName :: [(String,Int)] -> (String,Int)
		longestName lst
			| null lst = ("",0)
			| lengthStr (fst (head lst)) >= lengthStr (fst (longestName (tail lst))) = head lst
			| otherwise = longestName (tail lst)
			where
				lengthStr :: String -> Int
				lengthStr [] = 0
				lengthStr (s:str) =
					1+ lengthStr str
