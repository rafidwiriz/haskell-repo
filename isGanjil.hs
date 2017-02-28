-- Apakah Bilangan Ganjil - isGanjil(n)
-- Definisi dan Spesifikasi
isGanjil :: Int -> Bool
	{- isGanjil(n) mengeluarkan nilai true jika elemen n bernilai ganjil. Nilai ganjil yang dimaksud adalah jika n dikurangi 2 terus menerus hingga mencapai nilai 1. -}
-- Realisasi
isGanjil n =
	if (n==1) then -- Basis-1
		True
	else if (n==2) then -- Basis-2
		False
	else -- Rekurens
		isGanjil (n-2)
-- Contoh Aplikasi
-- isGanjil 4
-- isGanjil 21