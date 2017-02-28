-- Jumlah Faktor dari X - factorOfX(x,m)
-- Definisi dan Spesifikasi
factorOfX :: Int -> [Int] -> Int
	{- factorOfX(x,m) menghasilkan nilai banyaknya elemen dalam list m yang merupakan faktor dari x. -}
-- Realisasi
factorOfX x m
	| (length m == 1) = -- Basis
		if mod x (head m) == 0 then
			1
		else
			0
	| otherwise = -- Rekurens
		if mod x (head m) == 0 then
			1 + (factorOfX x (tail m))
		else
			factorOfX x (tail m)
-- Contoh Aplikasi
-- *Main> factorOfX 20 [1,2,10,15,13,19,12]
-- 3
-- *Main> factorOfX 15 [2,4,3,5,10,15,3]
-- 4