-- Apakah Elemen Depan Kedua List Sama? - isEqFront(m,n)
-- Definisi dan Spesifikasi
isEqFront :: [Char] -> [Char] -> Bool
	{- isEqFront(m,n) mengeluarkan nilai true jika potongan awal list n mengandung list m (dengan panjang dan urutan karakter yang sama). Banyaknya elemen n selalu lebih dari atau sama dengan m. -}
-- Realisasi
isEqFront m n
	| (null m) = False -- Basis
	| (length m) == 1 = -- Basis
		if (head m) == (head n) then
			True
		else
			False
	| not (null m) && (null n) = False -- Basis
	| otherwise = -- Rekurens
		if (head m) == (head n) then
			isEqFront (tail m) (tail n)
		else
			False
-- Contoh Aplikasi
-- *Main> isEqFront ['a','b','c'] ['a','b','c','d']
-- True
-- *Main> isEqFront ['a','b','c'] ['a,'b']
-- False