-- Deret Segitiga - deretSegitiga(n)
-- Definisi dan Spesifikasi
deretSegitiga :: Int -> Int
	{- deretSegitiga(n) menghasilkan bilangan ke-n pada sebuah deret segitiga. -}
-- Realisasi
deretSegitiga n =
	if (n==1) then -- Basis-1
		1
	else -- Rekurens
		deretSegitiga (n-1) + n
-- Contoh Aplikasi
-- deretSegitiga 2
-- deretSegitiga 10
