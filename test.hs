sumOfDigits :: Int -> Int
sum1DOfDigits :: Int -> Int
sumOfDigits n
	| n<=9 = n
	| otherwise = sumOfDigits (div n 10) + mod n 10
sum1DOfDigits n
	| (sumOfDigits n) <= 9 = sumOfDigits n
	| otherwise = sum1DOfDigits (sumOfDigits n)