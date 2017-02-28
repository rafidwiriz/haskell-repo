gabungAngka :: [Int] -> [(Int,Int)]
gabungAngka l
	| length l == 2 = [((head l),(head (tail l)))]
	| otherwise = [((head l),(head (tail l)))] ++ gabungAngka (tail l)