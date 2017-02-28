-- Membagi List - splitAlternate(lc)
-- Definisi dan Spesifikasi
splitAlternate :: [Char] -> ([Char],[Char])
	{- splitAlternate(lc) menghasilkan dua buah list, misalnya l1 dan l2. l1 berisi semua elemen lc pada posisi ganjil, l2 berisi semua elemen l pada posisi genap.
	Prekondisi: lc tidak kosong. -}
splitAlternateOdd :: Int -> [Char] -> [Char]
	{- splitAlternateOdd(n,lc) menghasilkan list berupa elemen-elemen list lc yang berada pada posisi ganjil. Posisi ganjil adalah jika n tidak habis dibagi oleh 2.
	Prekondisi: lc tidak kosong. -}
splitAlternateEven :: Int -> [Char] -> [Char]
	{- splitAlternateEven(n,lc) menghasilkan list berupa elemen-elemen list lc yang berada pada posisi genap. Posisi genap adalah jika n habis dibagi oleh 2.
	Prekondisi: lc tidak kosong. -}
konsDot :: [Char] -> Char -> [Char]
	-- konsDot(l,x) menghasilkan sebuah list dari l (list of
	-- elemen) dan x (sebuah elemen),
	-- dengan x sebagai elemen terakhir: l • x -> l' 
isOneElmt :: [Char] -> Bool
	-- isOneElmt(l) true jika list of integer l hanya
	-- mempunyai satu elemen
-- Realisasi
splitAlternate lc =
	let n = (length lc) in
		(splitAlternateOdd n lc, splitAlternateEven n lc)
splitAlternateOdd n lc
	| isOneElmt lc = -- Basis
		[last lc]
	| otherwise = -- Rekurens
		if mod n 2 == 1 then
			konsDot (splitAlternateOdd (n-1) (init lc)) (last lc)
		else
			splitAlternateOdd (n-1) (init lc)
splitAlternateEven n lc
	| isOneElmt lc = -- Basis
		[]
	| otherwise = -- Rekurens
		if mod n 2 == 0 then
			konsDot (splitAlternateEven (n-1) (init lc)) (last lc)
		else
			splitAlternateEven (n-1) (init lc)
konsDot l x = l ++ [x]
isOneElmt l = length l == 1
-- Contoh Aplikasi
-- *Main> splitAlternate ['a','b','c','d','e','f','g','h','i','j']
-- ("acegi","bdfhj")