-- Apakah List Elemen Unik? - isUnique(l)
-- Definisi dan Spesifikasi
isUnique :: [Char] -> Bool
	{- isUnique(lc) menghasilkan true jika lc adalah list dengan elemen unik, yaitu tidak ada elemen pada lc yang muncul lebih dari 1 kali.
	Prekondisi: l tidak kosong.-}
isEmpty :: [Char] -> Bool
	-- isEmpty(l) true jika list of elemen l kosong
isOneElmt :: [Char] -> Bool
	-- isOneElmt(l) true jika list of integer l hanya
	-- mempunyai satu elemen 
isMember :: Char -> [Char] -> Bool
	-- isMember(x,l) true jika x adalah elemen list l
-- Realisasi
isUnique l
	| isOneElmt l = True -- Basis
	| otherwise = -- Rekurens
		if isMember (head l) (tail l) then
			False
		else
			isUnique (tail l)
isEmpty l = null l
isOneElmt l = length l == 1
isMember x l =
	if (isEmpty l) then False -- Basis
	else if (head l) == x then True
	else (isMember x (tail l)) -- Rekurens
-- Contoh Aplikasi
-- *Main> isUnique ['a','b','c','d','e','f','g']
-- True
-- *Main> isUnique ['a','b','c','d','e','e','g']
-- False