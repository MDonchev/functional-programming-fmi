import Data.List

canCompose :: [String] -> Bool
canCompose [] = True
canCompose fs =
	canComposeHelp (typesReversed fs)
	where
		typesReversed :: [String] -> [[String]]
		typesReversed funcs = 
			reverse (map words funcs)
		canComposeHelp :: [[String]] -> Bool
		canComposeHelp (_:[]) = True
		canComposeHelp (str:strLeft) =
			last str == head (head strLeft) && canComposeHelp strLeft
main :: IO()
main = do
	print (canCompose ["A -> B"])
	print (canCompose [])
	print (canCompose ["A -> B", "A -> B"])
	print (canCompose ["[Int] -> Int", "[String] -> [Int]"])
	print (canCompose ["String -> Int","[Int] -> Int", "[String] -> [Int]"])
	print (canCompose ["Int -> String","[Int] -> Int", "[String] -> [Int]"])