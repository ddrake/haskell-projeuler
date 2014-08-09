import Control.Applicative
import qualified Data.Set as S

properDivisors :: Int -> [Int]
properDivisors n = 1 : filter (\x -> rem n x == 0) [2..n `div` 2]

abundants :: Int -> [Int]
abundants bound = filter abundant [2..bound]
	where abundant n = sum (properDivisors n) > n

abundantSums bound = S.fromList $ (+) <$> abunds <*> abunds
	where abunds = abundants bound

nonAbundantSums :: Int -> S.Set Int
nonAbundantSums bound = (S.fromList [1..bound]) S.\\ (S.filter (<= bound) (abundantSums bound))
