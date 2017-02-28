jumlahAMPM :: [Int] -> (Int,Int)
jumlahAM :: [Int] -> Int
jumlahPM :: [Int] -> Int
jumlahAM m
	| (length m == 1) =
		if (head m >= 0) && (head m <= 11) then
			1
		else
			0
	| otherwise =
		(if (head m >= 0) && (head m <= 11) then 1 else 0) + jumlahAM (tail m)
jumlahPM m
	| (length m == 1) =
		if (head m >= 12) && (head m <= 23) then
			1
		else
			0
	| otherwise =
		(if (head m >= 12) && (head m <= 23) then 1 else 0) + jumlahPM (tail m)
jumlahAMPM m = ((jumlahAM m),(jumlahPM m))