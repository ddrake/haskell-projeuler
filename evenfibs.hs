import Control.Applicative
import qualified Data.Set as S

sumEvenfibs :: Integer
sumEvenfibs = sum . takeWhile (<4000000) . filter even . map fst . iterate (\(p2, p1) -> (p1, p2 + p1)) $ (0,1)

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x:sieve [y | y <- xs, y `mod` x /= 0]

-- divide out all factors of a given factor from a number
-- not that useful...
divideOut :: Integer -> Integer -> Integer
divideOut n f
  | n <= f = n
  | n `mod` f == 0 = divideOut (n `div` f) f 
  | otherwise = divideOut n (f+1)


largestPrimeFactor :: Integer -> Integer
largestPrimeFactor = last . factorize 2

factorize :: Integer -> Integer -> [Integer]
factorize start n
  | n <= start = [n]
  | otherwise =
    let (q, r) = divMod n start
    in if r == 0 then start : factorize start q
       else factorize (start + 1) n


isPal :: Integer -> Bool
isPal n = (==) <*> reverse $ show n

--digits n = map (read . (\x -> [x])) . show $ n

largestPalindromeProduct :: Integer
largestPalindromeProduct = maximum . filter isPal $ [x*y | x <- [1..999], y <- [1..999]]


-- a horrible algorithm!
smallestDivisibleByAllTo :: Integer -> Integer
smallestDivisibleByAllTo n = head . filter (divisibleByAllTo n) $ [n*(n-1) ..]
  where divisibleByAllTo n x = all (\y -> x `mod` y == 0) [1..n]

smallestDivisibleByAllTo' :: Integer -> Integer
smallestDivisibleByAllTo' n = lcmList [1..n]

lcmList :: [Integer] -> Integer
lcmList = foldr1 (\x a -> lcm x a) 

-- closed form solution
sumSquareDifference :: Integer -> Integer
sumSquareDifference n = n * (n + 1) * (3*n*n - n - 2) `div` 12

-- performs ok to 100000
sumSquareDifference' :: Integer -> Integer
sumSquareDifference' n = (sum [1..n]) ^ 2 - (sum . map (^2) $ [1..n])

-- a bit slow, but works
nthPrime :: Integer -> Integer
nthPrime n = head . drop (fromIntegral (n-1)) . sieve $ [2..]

-- not used..
digitGroup :: Integer -> Integer -> Integer -> Integer
digitGroup st ct n =
  let tenCt = 10^ct
      shifted = n `div` 10^st
  in shifted - ((shifted `div` tenCt) * tenCt)

-- get the digits of a number to a list of integers
toDigits :: Integer -> [Integer]
toDigits n = map (read . (\c -> [c])) . show $ n

-- problem 8
-- get the largest product of k consecutive digits of a number n
largestProduct :: Int -> Integer -> Integer
largestProduct k n = 
  let digits = toDigits n
      dc = length digits
      groups = map (\d -> take k . drop d $ digits) [0..dc-k-1]
      prods = map product groups
  in maximum prods

-- 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

-- problem 9
specialTripletProduct :: Integer
specialTripletProduct = head $ [x*y*z | x <- [1..800], y <- [1..x], z <- [1..y], x*x == y*y + z*z, x+y+z == 1000]

primesBelow2M :: [Integer]
primesBelow2M = 
  let primes = 2:sieveSundaram 1000000
  in  init $ primes

sumOfPrimesBelow2M :: Integer
sumOfPrimesBelow2M = sum primesBelow2M


-- wow! like 8.26 million primes in 20 seconds!
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 
  let k = floor $ sqrt ((fromIntegral $ 1+2*n) - 1) / 2
      toRemove = S.fromList [i+j+2*i*j | j <- [1..k], i <- [1..j]]
      remaining = (S.fromList [1..n]) S.\\ toRemove
  in map (\x -> x*2 + 1) . S.toList $ remaining


letterCount :: Int -> Int
letterCount n = 
  let singleMap = [(1,length "one"),(2,length "two"),(3,length "three"),(4,length "four"),(5,length "five"),(6,length "six"),
                   (7,length "seven"),(8,length "eight"),(9,length "nine")]
      teenMap =   [(10,length "ten"),(11,length "eleven"),(12,length "twelve"),(13,length "thirteen"),(14,length "fourteen"),
                   (15,length "fifteen"),(16,length "sixteen"),(17,length "seventeen"),(18,length "eighteen"),(19,length "nineteen")]
      tenMap  =   [(2,length "twenty"),(3,length "thirty"),(4,length "forty"),(5,length "fifty"),(6,length "sixty"),
                   (7,length "seventy"),(8,length "eighty"),(9,length "ninety")]
      hundred = length "hundred"
      thousands = n `div` 1000
      hundreds = (n - thousands*1000) `div` 100
      rest = n - thousands*1000 - hundreds*100
      tens = if rest >= 20 then rest `div` 10 else 0
      teens = if rest < 20 && rest > 9 then rest else 0
      singles = if rest >= 20 || rest <=9 then rest - 10*tens else 0
      thousandCt = if thousands == 1 then length "onethousand" else 0
      hundredsCt = case lookup hundreds singleMap of 
        Just n -> n + 7 
        Nothing -> 0
      tensCt = case lookup tens tenMap of 
        Just n -> n
        Nothing -> 0
      teenCt = case lookup teens teenMap of 
        Just n -> n
        Nothing -> 0
      singleCt = case lookup singles singleMap of 
        Just n -> n
        Nothing -> 0
      andCt = if hundredsCt > 0 && (tensCt > 0 || teenCt > 0 || singleCt > 0) then 3 else 0
  in thousandCt + hundredsCt + tensCt + teenCt + singleCt + andCt

oneToThousandCt = sum . map letterCount $ [1..1000]

