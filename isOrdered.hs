-- List Berurutan - isOrdered(m)
-- Definisi dan Spesifikasi
isOrdered :: [Int] -> Bool
	{- isOrdered(l) menghasilkan true jika elemen-elemen pada l terurut membesar, false jika tidak.
	Prekondisi: l tidak kosong. -} 
isOneElmt :: [Int] -> Bool
	-- isOneElmt(l) true jika list of integer l hanya
	-- mempunyai satu elemen 
-- Realisasi
isOrdered l
	| isOneElmt l = -- Basis
		True
	| otherwise = -- Rekurens
		if (head l) <= (head (tail l)) then
			isOrdered (tail l)
		else
			False
isOneElmt l = length l == 1
-- Contoh Aplikasi
-- *Main> isOrdered [1,2,3,4,5,6,7,8,9,10]
-- True
-- *Main> isOrdered [1,3,4,5,7,8,10]
-- True
-- *Main> isOrdered [3,4,5,67,9,10]
-- False