-- Penjumlahan Isi List - sumIsiList(l)
-- Definisi dan Spesifikasi
sumIsiList :: [Int] -> Int
	{- sumIsiList(l) menghasilkan nilai hasil penjumlahan seluruh elemen dari list l.
	Prekondisi: l tidak kosong. -}
isOneElmt :: [Int] -> Bool
	-- isOneElmt(l) true jika list of integer l hanya
	-- mempunyai satu elemen 
-- Realisasi
sumIsiList l
	| isOneElmt l = head l -- Basis
	| otherwise = (head l) + (sumIsiList (tail l)) -- Rekurens
isOneElmt l = length l == 1
-- Contoh Aplikasi
-- *Main> sumIsiList [1,2,3,4,5,6,7,8,9,10]
-- 55
-- *Main> sumIsiList [3,2,5,4,7,23,44,54]
-- 142
