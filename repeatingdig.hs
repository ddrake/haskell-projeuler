import Numeric
import Data.Char
import Data.List

-- Numbers n less than 500 such that the repeating decimal expansion of 1/n has length n-1
-- 7,17,19,23,29,47,59,61,97,109,131,149,167,193,229,233,257,263,269,313,337,367,379,383,389,419,433,461,487,491,499
-- Not finding any with longer relative expansion lengths

-- Conjecture 1: For any given number n, the length of the repeating decimal expansion of 1/n cannot exceed n-1
-- Define 'MaxExps' to be { n <- N | the repeating decimal expansion of 1/n has length equal to n-1 }
-- Conjecture 2: MaxExps is a subset of the primes (for reference):
-- 2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,
-- 151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,
-- 311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,
-- 479,487,491,499

-- Question 1: Suppose that Conjectures 1 and 2 are true in base 10 (decimal), 
-- are they true for other bases?
-- Question 2: If the answer to Question 1 is 'Yes', does the set of MaxEps depend on the base?
-- For example, if we instead consider the octal expansion of 1/n, would we obtain the same set?
-- Question 3: If Conjecture 2 is true, doesn't that provide us with a way of finding primes 
--   which doesn't involve any factoring?

-- I think the answer to Question 1 is 'Yes', but the answer to Question 2 is 'No'.
-- Here is an example:

-- Instead of dividing a large power of 10 by our number, let's divide it by a large power of 8
-- and then display the result in base 8...
-- Checking a lot of non-primes, didn't result in any MaxEps, and not seeing any 
-- with expansion > n-1 .. so it looks like the answer to Question 1 is 'Yes'

-- How about Question 2, though...

intInvertedForBase :: Integer -> Integer -> Integer -> [Char]
intInvertedForBase base p n = 
  let result = (base^p) `div` n 
  in showIntAtBase base intToDigit result ""



nsWithLongest :: (Integer -> Integer -> [Char]) -> Integer -> ([Integer], Integer)
nsWithLongest f u = 
  let (up, xs) = initialUpperPower f 1 (nonTrivials u)
      lp = up `div` 2
      maxes = maxExpansionPeriod f lp up xs
      Just pd = period . f up . head $ maxes
  in (maxes, pd)


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

-- Given a previous power of 10, a current power of 10 and an upper bound, 
-- find a subset of the list of integers less than the bound
-- such that the decimal expansions of the multiplicative inverses 
-- of the elements of the subset have the maximal period.
maxExpansionPeriod :: (Integer -> Integer -> [Char]) -> Integer -> Integer -> [Integer] -> [Integer]
maxExpansionPeriod f lp up xs =
  if lp == up-1 then xs
  else let p = lp + (up - lp + 1) `div` 2
           nps = nonPeriodicFromListFor f p xs
           in if length nps == 0 then maxExpansionPeriod f lp p xs
              else maxExpansionPeriod f p up nps

initialUpperPower :: (Integer -> Integer -> [Char]) -> Integer -> [Integer] -> (Integer, [Integer])
initialUpperPower f p xs =
  let nps = nonPeriodicFromListFor f p xs
  in if length nps == 0 then (p, xs)
     else initialUpperPower f (2*p) nps
-- We know that the multiplicative inverse of any integer has a repeated decimal expansion
-- but if we don't use a large enough power of 10 to divide the number by, no
-- period will appear.  So to find numbers whose inverses have long periods,
-- we can filter out those whose periods have been identified by the given power.
-- Any multiple of 2 or 5 repeats with period 1 (either zeros or nines) 
-- so we don't need to consider them.
--nonPeriodicFor :: Int -> Int -> [Integer]
--nonPeriodicFor u p = let
--  nonTrivials = [x | x <- [3..(u-1)], x `rem` 2 /= 0, x `rem` 5 /= 0]
--  in nonPeriodicFromListFor p nonTrivials

-- Given a power of 10 and a list to check, return any numbers in the list
-- whose multiplicative inverses  
nonPeriodicFromListFor :: (Integer -> Integer -> [Char]) -> Integer -> [Integer] -> [Integer]
nonPeriodicFromListFor f p xs = filter (\x -> (period $ f p x) == Nothing) xs

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

-- break a list into sub-lists of length n.  
-- The last element in the result may contain less than n elements
groupsOf :: Eq(a) => Integer -> [a] -> [[a]]
groupsOf n [] = []
groupsOf n xs = 
  let n' = fromIntegral n
  in (take n' xs) : groupsOf n (drop n' xs)

-- for the purposes of finding repeated decimal expansions,
-- say that two lists 'match' if they match at the start and at
-- all subsequent corresponding indices until we reach the end of the second list.
-- It appears to be the case (needs proof) that all non-trivial multiplicative inverses
-- begin their pattern with the first non-zero value that appears in the quotient.  
-- This is not the case for e.g. 1/16, which has decimal expansion 0.06250000 (or 0.06249999)
matches :: Eq(a) => [a] -> [a] -> Bool
matches xs ys = xs == ys || lys < lxs && take lys xs == ys
  where (lxs, lys) = (length xs, length ys)

-- Just for checking that the MaxEps are primes
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x:(sieve . filter (\y -> y `rem` x /= 0) $ xs)


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


maxExpsForBase :: Integer -> Integer -> [Integer]
maxExpsForBase base u = 
  let pairs = map (\x -> (x, periodForNumber (intInvertedForBase base) x)) . nonTrivialsForBase base $ u
  in map fst . filter (\(n, p) -> p == n-1) $ pairs