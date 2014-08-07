type Point = (Int, Int)

data Node = Node { pt :: Point, prev :: Point, dist :: Int } deriving (Show)

-- given an n-element list like this: [a,b,c,d], form an n+1 element nested list like this: [[a], [a,b], [b,c], [c,d],[d]]
pairs :: [a] -> [[a]]
pairs xs = [head xs] : zipWith (\x y -> [x,y]) xs (tail xs) ++ [[last xs]]

-- take a point, a weight and a list containing either one or two nodes and return a new successor node
calcNode :: ((Point, Int), [Node]) -> Node
calcNode ((newPt, weight), [Node pt prev dist]) = Node newPt pt (dist+weight)
calcNode ((newPt, weight), [Node ptl prevl distl, Node ptr prevr distr])
    | distl > distr = Node newPt ptl (distl+weight)
    | otherwise = Node newPt ptr (distr+weight)
calcNode _  = error "expected either one or two predecessor nodes"

-- given a row number, a list of weights and a list of nodes representing the previous row, compute a list of nodes
calcRow :: Int -> [Int] -> [Node] -> [Node]
calcRow r ws ns = 
    let pts = map (\c -> (r, c)) [0..(length ws) - 1] 
        ptwts = zip pts ws
    in map calcNode . zip ptwts $ (pairs ns)

-- given a list of rows of weights and all previous rows of nodes, return either a list with one more row of nodes
-- if we have more weights, otherwise just the current list of rows of nodes.
calcRows :: [[Int]] -> [[Node]] -> [[Node]]
calcRows [] ns = ns
calcRows (row:rows) ns =
    let prev = last ns -- previous row of nodes
        r = (fst . pt . head $ prev) + 1
        newN = calcRow r row prev
        newNs = ns ++ [newN]
    in calcRows rows newNs

calcAllNodes :: [[Int]] -> [[Node]]
calcAllNodes (row:rows) = calcRows rows [[Node (0,0) (0,0) (head row)]] 


rows = [[75],
        [95,64],
        [17,47,82],
        [18,35,87,10],
        [20,04,82,47,65],
        [19,01,23,75,03,34],
        [88,02,77,73,07,63,67],
        [99,65,04,28,06,16,70,92],
        [41,41,26,56,83,40,80,70,33],
        [41,48,72,33,47,32,37,16,94,29],
        [53,71,44,65,25,43,91,52,97,51,14],
        [70,11,33,28,77,73,17,78,39,68,17,57],
        [91,71,52,38,17,14,91,43,58,50,27,29,48],
        [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
        [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]] :: [[Int]]