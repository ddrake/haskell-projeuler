import Data.List

-- it took some time and is definitely ugly, but it works!
-- I think there should be a way to eliminate the outer concat somehow...
-- for three elements, we're doing zipwith f(x,y) [1,2,3] [ [[2,3],[3,2]], [[1,3],[3,1]], [[1,2],[2,1]] ]
-- where f(x,y) is taking x and consing it with each element of the corresponding sublist so
-- for example f(1,[[2,3],[3,2]]) -> [[1,2,3], [1,3,2]]
perms :: [Int] -> [[Int]]
perms [] = []
perms [a] = [[a]]
perms lst = concat $ zipWith (\x ps -> map (\p -> x:p) ps) lst (foldr (\x a -> (perms (lst \\ [x])):a) [] lst)

millionthPerm = head . drop 999999 $ perms [0..9]
-- > [2,7,8,3,9,1,5,4,6,0]
