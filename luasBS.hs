-- Luas Bujur Sangkar - luasBS(x)
-- Definisi dan Spesifikasi
luasBS :: Int -> Int
	{- luasBS(x) menghasilkan nilai luas sebuah bujur sangkar dengan panjang sisi x. Penghitungan dilakukan dengan melakukan rekursi. -}
-- Realisasi
luasBS x =
	if (x==1) then -- Basis-1
		1
	else -- Rekurens
		luasBS (x-1) + (2 * x) - 1
-- Contoh Aplikasi
-- luasBS 2
-- luasBS 10