ekstrakTanda :: [Int] -> [Int]
ekstrakTanda l
	| null l = []
	| otherwise =
		if (head l) > 0 then
			[1] ++ ekstrakTanda (tail l)
		else if (head l) < 0 then
			[-1] ++ ekstrakTanda (tail l)
		else
			[0] ++ ekstrakTanda (tail l)