import Data.List
type Measurement = (Int,Double)

sortMin :: Measurement -> Measurement -> Double -> Ordering
sortMin (name1, pr1) (name2, pr2) avg
  | abs (pr1 - avg) < abs (pr2 - avg) = LT
  | abs (pr1 - avg) > abs (pr2 - avg) = GT
  | abs (pr1 - avg) == abs (pr2 - avg) = compare name1 name2
average :: [Measurement] -> Double
average prods =
	sum (map snd prods) / fromIntegral (length prods)
closestToAverage :: [Measurement] -> Int
closestToAverage prods =
	fst (head (sortBy (\ ord1 ord2 -> sortMin ord1 ord2 (average prods)) prods))

main :: IO()
main = do
	print (closestToAverage [(5,18.5),(15,13.2),(10,16.8),(11,22.8),(3,14.6)])
	print (closestToAverage [(1,23.6),(6,24.2),(11,24.2),(16,21.2),(21,23.8),(26,26.5),(31,24.5)])