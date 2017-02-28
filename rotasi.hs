rotasi :: Int -> [Int] -> [Int]
rotasi n l
	| n == 0 = l
	| otherwise = 
		rotasi (n-1) (tail l) ++ [head l]