
data BTree = Empty | Node Int BTree BTree
--Check for console
treeList :: BTree -> [Int]
treeList Empty = []
treeList (Node node left right) = treeList left ++ [node] ++ treeList right

mirrorBST :: BTree -> BTree
mirrorBST Empty = Empty
mirrorBST (Node node left right) = 
	(Node node (mirrorBST right) (mirrorBST left))

main :: IO()
main = do
	let tr = (Node 1
				(Node 2 
					(Node 5 Empty Empty)
					 Empty)
				(Node 3 
					(Node 7 Empty Empty)
					(Node 6 Empty Empty)
					)
				)
	print (treeList tr)
	print (treeList (mirrorBST tr))