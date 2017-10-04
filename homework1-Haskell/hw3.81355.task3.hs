main :: IO()
main = do
	print (extractRLE [('m',1), ('i',1), ('s',2), ('i',1),
					   ('s',2), ('i',1), ('p',2), ('i',1)])
	print (extractRLE [('A',6), ('b',5), ('c',9)])
	print (extractRLE [(2,4), (3,1), (4,5)])
	print (compressRLE [2,2,2,2,3,4,4,4,4,4])
	print (compressRLE "mississippi")
	print (compressRLE "AAAAAAbbbbbccccccccc")
	print (compressRLE "")
	print (getRLE [('m',1), ('i',1), ('s',2), ('i',1), ('s',2), ('i',1), ('p',2), ('i',1)] 0)
	print (getRLE [('m',1), ('i',1), ('s',2), ('i',1), ('s',2), ('i',1), ('p',2), ('i',1)] 1)
	print (getRLE [('m',1), ('i',1), ('s',2), ('i',1), ('s',2), ('i',1), ('p',2), ('i',1)] 2)
	print (getRLE [('m',1), ('i',1), ('s',2), ('i',1), ('s',2), ('i',1), ('p',2), ('i',1)] 3)
	print (getRLE [('m',1), ('i',1), ('s',2), ('i',1), ('s',2), ('i',1), ('p',2), ('i',1)] 4)
--a)
extractRLE :: (Eq a) => [(a, Int)] -> [a]
extractRLE [] = []
extractRLE (pair:rest) =
	extractPair pair ++ extractRLE rest
	where
		extractPair :: (a,Int) -> [a]
		extractPair (_,0) = []
		extractPair (sym,n) =
			sym : extractPair (sym,(n - 1))
--b)
compressRLE :: (Eq a) => [a] -> [(a,Int)]
compressRLE [] = []
compressRLE (x:xs) =
	(x,(countX x xs) + 1) : compressRLE (removed x xs)
	where
	countX :: (Eq a) => a -> [a] -> Int
	countX _ [] = 0
	countX y (s:ss) =
		if (y == s)
		then 1 + countX y ss
		else 0
	removed :: (Eq a) => a -> [a] -> [a]
	removed _ [] = []
	removed x lst =
		if (x == head lst)
		then removed x (tail lst)
		else lst
--c)
getRLE :: (Eq a) => [(a,Int)] -> Int -> a
getRLE lst x = 
	if (x < snd (head lst))
	then fst (head lst)
	else getRLE (tail lst) (x - snd (head lst))










