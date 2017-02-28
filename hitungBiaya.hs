-- Biaya Opname - hitungBiaya(k,h,s,a)
-- Definisi dan Spesifikasi
hitungBiaya :: Int -> Int -> Int -> Bool -> Int
-- Realisasi
hitungBiaya k h s a =
	if (a == True) then
		if (k == 1) then
			(div (750000 * 80 * h) 100) + (div (1500000 * 90 * s) 100)
		else if (k == 2) then
			(div (600000 * 80 * h) 100) + (div (1500000 * 90 * s) 100)
		else
			(div (400000 * 80 * h) 100) + (div (1500000 * 90 * s) 100)
	else
		if (k == 1) then
			(750000 * h) + (1500000 * s)
		else if (k == 2) then
			(600000 * h) + (1500000 * s)
		else
			(400000 * h) + (1500000 * s)
-- Contoh Aplikasi
-- *Main> hitungBiaya 1 2 1 True
-- 2550000
-- *Main> hitungBiaya 1 2 1 False
-- 3000000