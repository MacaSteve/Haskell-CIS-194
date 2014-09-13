-- Exercise 1
toDigits	:: Integer -> [Integer]
toDigits n		= reverse (toDigitsRev n)

toDigitsRev	:: Integer -> [Integer]
toDigitsRev n
	| n <= 0	= []
	| otherwise	= [n `mod` 10] ++ toDigitsRev (n `div` 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x	= reverse (doubleEveryOtherFront (reverse x))

doubleEveryOtherFront :: [Integer] -> [Integer]
doubleEveryOtherFront []			= []
doubleEveryOtherFront (x:[])		= [x]
doubleEveryOtherFront (x:y:zs)	= x : (y * 2) : doubleEveryOtherFront zs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits []		= 0
sumDigits (x:xs)	= sum (toDigits x) + sumDigits(xs)

-- Exercise 4
validate :: Integer -> Bool
validate n	= sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ 		= [(a, b)]
hanoi n a b c 		= 
	let
		step1 = hanoi (n - 1) a c b
		step2 = hanoi 1 a b c
		step3 = hanoi (n - 1) c b a
	in
		step1 ++ step2 ++ step3
