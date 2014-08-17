import Numeric
import Data.Char
import Data.List

maxExpsForBase :: Integer -> Integer -> [Integer]
maxExpsForBase base u = 
  let pairs = map (\x -> (x, periodForNumber (intInvertedForBase base) x)) . nonTrivialsForBase base $ u
  in map fst . filter (\(n, p) -> p == n-1) $ pairs

-- Similar to intInvertedDecimal, but works for any base
intInvertedForBase :: Integer -> Integer -> Integer -> [Char]
intInvertedForBase base p n = 
  let result = (base^p) `div` n 
  in showIntAtBase base intToDigit result ""

-- Given a power of 10 and a number, return the list of digits obtained by dividing
-- the number by the power of 10.
intInvertedDecimal :: Integer -> Integer -> [Char]
intInvertedDecimal p n = 
  let p' = fromIntegral p
  in show . div (10^p') $ n  

-- Given a list that may be periodic, return Just the period or Nothing
-- The periodic pattern must appear in full at least twice.
period :: Eq(a) => [a] -> Maybe Integer
period xs = 
  let lmax = fromIntegral $ length xs `div` 2
      period' n mx xs 
        | n > mx = Nothing
        | otherwise = if isPeriodicWith n xs then Just n else period' (n+1) mx xs
  in period' 1 lmax xs

-- a list is periodic with period n if when partitioned into n-element sub-lists, 
-- its groups of that period 'match' according to the definition below
isPeriodicWith :: Eq(a) => Integer -> [a] -> Bool
isPeriodicWith n xs =
  let (g:gs) = groupsOf n xs
  in all (matches g) gs

periodForNumber :: (Integer -> Integer -> [Char]) -> Integer -> Integer
periodForNumber f n = 
    let periodForNumber' p n = 
          case period (f (10^p) n) of 
              Nothing -> periodForNumber' (p+1) n
              (Just per) -> per
    in periodForNumber' 1 n

maxPeriod :: (Integer -> Integer -> [Char]) -> Integer -> (Integer, Integer)
maxPeriod f u = 
  let pairs = map (\x -> (x, periodForNumber f x)) $ nonTrivials u
  in maximumBy (\(_,p1) (_,p2) -> compare p1 p2) $ pairs 


-- break a list into sub-lists of length n.  
-- The last element in the result may contain less than n elements
groupsOf :: Eq(a) => Integer -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = 
  let n' = fromIntegral n
  in (take n' xs) : groupsOf n (drop n' xs)

-- say that two lists 'match' if they match at the start and at
-- all subsequent corresponding indices until we reach the end of the second list.
matches :: Eq(a) => [a] -> [a] -> Bool
matches xs ys = xs == ys || lys < lxs && take lys xs == ys
  where (lxs, lys) = (length xs, length ys)


nonTrivials :: Integer -> [Integer]
nonTrivials u = [x | x <- [3..(u-1)], x `rem` 2 /= 0, x `rem` 5 /= 0]

nonTrivialsForBase :: Integer -> Integer -> [Integer]
nonTrivialsForBase base u = 
  let divs = nub $ divisors base
      prd x = all (\z -> rem x z /= 0)
  in [x | x <- [2..(u-1)], prd x divs]

divisors :: Integer -> [Integer]
divisors n = 
  let divisors' _ 1 = []
      divisors' start n = 
        let (q, r) = quotRem n start
        in if r == 0 then start:divisors' start q
           else divisors' (start+1) n
  in divisors' 2 n

-- Just for checking that the MaxEps are primes
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x:(sieve . filter (\y -> y `rem` x /= 0) $ xs)
