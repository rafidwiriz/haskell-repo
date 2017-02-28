-- Nilai Tengah - nilaiTengah(a,b,c)
-- Definisi dan Spesifikasi
nilaiTengah :: Int -> Int -> Int -> Int
	-- nilaiTengah(a,b,c) menghasilkan nilai diantara maksimum dan minimum
max3 :: Int -> Int -> Int -> Int
	-- max3(a,b,c) menghasilkan nilai maksimum
min3 :: Int -> Int -> Int -> Int
	-- min3(a,b,c) menghasilkan nilai minimum
-- Realisasi
max3
	| (a>b) && (a>c) = a
	| (b>a) && (b>c) = b
	| (c>a) && (c>b) = c
min3
	| (a<b) && (a<c) = a
	| (b<a) && (b<c) = b
	| (c<a) && (c<b) = c
nilaiTengah a b c = a+b+c-(max3+min3)
-- Contoh Aplikasi
-- nilaiTengah 2 10 5
-- nilaiTengah 3 4 12
