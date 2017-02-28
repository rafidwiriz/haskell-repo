-- List Ganjil - filterGanjil(li)
-- Definisi dan Spesifikasi
filterGanjil :: [Int] -> [Int]
	{- filterGanjil(li) menghasilkan sebuah list yang berisi elemen-elemen bilangan ganjil dari list li.
	Prekondisi: li mungkin kosong. -}
isEmpty :: [Int] -> Bool
	-- isEmpty(l) true jika list of elemen l kosong
konsDot :: [Int] -> Int -> [Int]
	-- konsDot(l,x) menghasilkan sebuah list dari l (list of
	-- elemen) dan x (sebuah elemen),
	-- dengan x sebagai elemen terakhir: l • x -> l' 
-- Realisasi
filterGanjil li
	| isEmpty li = [] -- Basis
	| otherwise = -- Rekurens
		if mod (last li) 2 == 1 then
			konsDot (filterGanjil (init li)) (last li)
		else
			filterGanjil (init li)
isEmpty l = null l
konsDot l x = l ++ [x]
-- Contoh Aplikasi
-- *Main> filterGanjil [1,2,3,4,5,6,7,8,9,10]
-- [1,3,5,7,9]
-- *Main> filterGanjil [22,32,44,12,23,20,1334578,2012]
-- [23]