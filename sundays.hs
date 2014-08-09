
-- get the numbers of the days which are the first of a month
-- where jan 1, 1901 is day number 1
allFirstsOfMonths :: [Integer]
allFirstsOfMonths = firstsOfMonths 1900 1 1

firstsOfMonths :: Integer -> Integer -> Integer -> [Integer]
firstsOfMonths 2001 1 _ = []
firstsOfMonths year month dayNum = 
  let month' = if month == 12 then 1 else month + 1
      year' = if month == 12 then year + 1 else year
      next = dayNum+(getDaysInMonth year month)
  in next:(firstsOfMonths year' month' next)

getDaysInMonth :: Integer -> Integer -> Integer
getDaysInMonth year month = 
  if month `elem` [9,4,6,11] 
    then 30
    else if month /= 2 
            then 31
            else if year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0) 
              then 29
              else 28

isSunday :: Integer -> Bool
isSunday dayNum = dayNum `mod` 7 == 0

firstSundays :: [Integer]
firstSundays = filter (\d -> isSunday d && d > 365) allFirstsOfMonths

