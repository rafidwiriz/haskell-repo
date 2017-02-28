-- Menggabungkan Dua List Berurutan Membesar - mergeList(li1,li2)
-- Definisi dan Spesifikasi
mergeList :: [Int] -> [Int] -> [Int]
	{- mergeList(li1,li2) menghasilkan list of integer yang merupakan hasil penggabungan li1 dan li2, dan tetap terurut membesar.
	Prekondisi: li1 dan li2 adalah list terurut membesar dan mungkin kosong. -}
isOneElmt :: [Int] -> Bool
	-- isOneElmt(l) true jika list of integer l hanya
	-- mempunyai satu elemen
-- Realisasi	
mergeList l1 l2
	| isOneElmt l1 && isOneElmt l2 = -- Basis
		if (head l1) <= (head l2) then
			[head l1] ++ [head l2]
		else
			[head l2] ++ [head l1]
	| (not (isOneElmt l1)) && isOneElmt l2 = -- Basis
		if (head l2) <= (head l1) then
			[head l2] ++ l1
		else
			[head l1] ++ mergeList (tail l1) l2
	| isOneElmt l1 && (not (isOneElmt l2)) = -- Basis
		if (head l1) <= (head l2) then
			[head l1] ++ l2
		else
			[head l2] ++ mergeList l1 (tail l2)
	| otherwise = -- Rekurens
		if (head l1) <= (head l2) then
			[head l1] ++ mergeList (tail l1) l2
		else
			[head l2] ++ mergeList l1 (tail l2)
isOneElmt l = length l == 1
-- Contoh Aplikasi
-- *Main> mergeList [1,3,5,7,9] [2,4,6,8,10]
-- [1,2,3,4,5,6,7,8,9,10]
-- *Main> mergeList [1,4,15,23,66,72] [2,5,10,12,19,20,24,32,33]
-- [1,2,4,5,10,12,15,19,20,23,24,32,33,66,72]	