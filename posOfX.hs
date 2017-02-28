-- Posisi X - posOfX(e,lc)
-- Definisi dan Spesifikasi
posOfX :: Char -> [Char] -> Int
	{- posOfX(e,lc) menghasilkan sebuah bilangan integer yang menyatakan posisi e pada list of character lc. Jika e bukan elemen dari lc, fungsi akan menghasilkan 0.
	Prekondisi: lc memiliki elemen unik. -}
isEmpty :: [Char] -> Bool
	-- isEmpty(l) true jika list of elemen l kosong
isMember :: Char -> [Char] -> Bool
	-- isMember(x,l) true jika x adalah elemen list l
-- Realisasi
posOfX e lc
	| isEmpty lc = 0 -- Basis
	| otherwise = -- Rekurens
		if (last lc) == e then
			(length lc)
		else
			posOfX e (init lc)
isEmpty l = null l
isMember x l =
	if (isEmpty l) then False -- Basis
	else if (head l) == x then True
	else (isMember x (tail l)) -- Rekurens
-- Contoh Aplikasi
-- *Main> posOfX 'x' ['a','d','u','x','c','y','b']
-- 4
-- *Main> posOfX 'x' ['a','d','u','c','y','b']
-- 0