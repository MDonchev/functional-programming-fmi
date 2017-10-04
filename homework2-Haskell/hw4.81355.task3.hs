data Node = Node Int [Int]
type Graph = [Node]
{-
	Представяме графа, като списък от алгебричен тип Node, представящ един връх.
	В конструктура на този тип се задават стойността на връха (Int по условие),
	както и списък от съседите на съответния връх (също сме ги приели като Int).
-}
isSumReachable :: Graph -> Int -> Int -> Bool
isSumReachable gr node n
	|n < 0 = False
	|n == 0 = True
	|nodeNeighbours == [] = False
	|otherwise = 
		reachableFromNeighbours (map (\ x -> isSumReachable gr x (n - node)) nodeNeighbours)
		where
			reachableFromNeighbours :: [Bool] -> Bool
			reachableFromNeighbours [] = False
			reachableFromNeighbours (x:xs) = x || reachableFromNeighbours xs
			(Node inf nodeNeighbours) = 
				findNode gr node
				where
					findNode :: Graph -> Int -> Node
					findNode ((Node x neighbours):gs) y =
						if (x == y) then (Node x neighbours) else findNode gs y
gr :: Graph
gr = [Node 1 [2,3,4],
	  Node 2 [5],
	  Node 3 [6],
	  Node 4 [5],
	  Node 5 [6],
	  Node 6 []]
main :: IO()
main = do
	print (isSumReachable gr 2 7)
	print (isSumReachable gr 1 8)
	print (isSumReachable gr 1 10)
	print (isSumReachable gr 6 6)
	print (isSumReachable gr 6 3)