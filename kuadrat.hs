kuadrat :: [Int] -> [Int]
kuadrat l
	| null l = []
	| otherwise = [(head l) * (head l)] ++ kuadrat (tail l)