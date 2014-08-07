import Control.Monad
import Data.List

-- factor a number into a list of primes, possibly including the number itself
divisorsProc :: Int -> Int -> [Int]
divisorsProc n p
    | n <= p = [n]
    | otherwise = 
        let (q,r) = divMod n p
        in if r == 0 then p:divisorsProc q p else divisorsProc n (p+1)

-- get the products of the power set of the set of prime factors
-- ensuring that the number itself is excluded.
properDivisors :: Int -> [Int]
properDivisors n = 
    let divs = divisorsProc n 2
        final = last divs
        smaller = if final == n then init divs else divs
        subsets = filterM (\x -> [True, False]) smaller
        products = map product . nub $ subsets
    in products \\ [n]

-- predicate accepting a list containing the sum of proper divisors for each index, 
-- a maximum index, and a number to check for amicability
amicable :: [Int] -> Int -> Int -> Bool
amicable ds limit n = 
    let m = ds !! n
        n' = if m <= limit then ds !! m else (-1)
    in m /= n && n' == n

-- return all the amicable numbers up to the specified limit
amicables :: Int -> [Int]
amicables limit = 
    let ds = map sum . map properDivisors $ [0..limit]
    in tail . tail . filter (\n -> amicable ds limit n) $ [0..limit]

