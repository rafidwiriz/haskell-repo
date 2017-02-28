-- Menghapus Elemen Ke-n - delNthElmt(n,m)
-- Definisi dan Spesifikasi
delNthElmt :: Int -> [Int] -> [Int]
	{- delNthElmt(n,m) menghasilkan list yang berasal dari list m, namun dengan pengurangan elemen ke-n. -}
-- Realisasi
delNthElmt n m
	| n == 1 = -- Basis
		if (length m == 1) then
			[]
		else
			(tail m)
	| otherwise = -- Rekurens
		[head m] ++ (delNthElmt (n-1) (tail m))
-- Contoh Aplikasi
-- *Main> delNthElmt 3 [1,10,4,13,12,5,19]
-- [1,10,13,12,5,19]
-- *Main> delNthElmt 5 [1,10,4,13,12,5,19]
-- [1,10,4,13,5,19]