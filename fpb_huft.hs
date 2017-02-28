-- FPB versi 2 - fpb(a,b)
-- Deskripsi dan Spesifikasi
fpb :: Int -> Int -> Int
	{- fpb(a,b) mengeluarkan nilai hasil dari FPB antara a dan b dengan menggunakan metode nilai minimum. -}
min2 :: Int -> Int -> Int
	{- min2(a,b) mengeluarkan nilai minimum antara a atau b. -}
fpbMin :: Int -> Int -> Int -> Int
	{- fpbMin(a,b,x) mengeluarkan nilai dari FPB antara a dan b, dengan metode membagi habis a dan b dengan x. Jika tidak habis dibagi, maka pembagian dilakukan kembali dengan nilai x-1, dan seterusnya. -}
-- Realisasi
min2 a b
	| a<b = a
	| otherwise = b
fpb a b =
	let x = min2 a b in
		fpbMin a b x
fpbMin a b x =
	if ((mod a x)==0) && ((mod b x)==0) then -- Basis-0
		x
	else -- Rekurens
		fpbMin a b (x-1)
-- Contoh Aplikasi
-- fpb 32 10
-- fpb 10 5
