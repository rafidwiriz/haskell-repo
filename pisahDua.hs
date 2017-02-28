pisahDua :: [Int] -> Int -> ([Int],[Int])
pisahDua l n
	| null l = ([],[])
	| otherwise =
		let (l1,l2) = pisahDua (tail l) n in
			if (head l) <= n then
				([head l] ++ l1,l2)
			else
				(l1,[head l] ++ l2)