-- Membagi List - splitList(li)
-- Definisi dan Spesifikasi
splitList :: [Int] -> ([Int],[Int]) 
	{- splitList(li) menghasilkan 2 list of integer, list pertama memuat bilangan positif dan 0 yang merupakan elemen dari li (dengan urutan kemunculan yang tidak berubah), sedangkan list kedua memuat bilangan negatif elemen li.
	Prekondisi: li mungkin kosong. -}
splitListPos :: [Int] -> [Int]
	{- splitListPos(li) menghasilkan list berisi elemen bilangan positif dan nol dari list li.
	Prekondisi: li mungkin kosong. -}
splitListNeg :: [Int] -> [Int]
	{- splitListNeg(li) menghasilkan list berisi elemen bilangan negatif dari list li.
	Prekondisi: li mungkin kosong. -}
isEmpty :: [Int] -> Bool
	-- isEmpty(l) true jika list of elemen l kosong
konso :: Int -> [Int] -> [Int]
	-- konso(x,l) menghasilkan sebuah list dari x (sebuah
	-- elemen) dan l (list of elemen),
	-- dengan x sebagai elemen pertama: x o l -> l'
-- Realisasi
splitList li = (splitListPos li,splitListNeg li)
splitListPos li
	| isEmpty li = [] -- Basis
	| otherwise = -- Rekurens
		if head li >= 0 then
			konso (head li) (splitListPos (tail li))
		else
			splitListPos (tail li)
splitListNeg li
	| isEmpty li = [] -- Basis
	| otherwise = -- Rekurens
		if head li < 0 then
			konso (head li) (splitListNeg (tail li))
		else
			splitListNeg (tail li)
konso x l = [x] ++ l
isEmpty l = null l
-- Contoh Aplikasi
-- *Main> splitList [1,-2,-3,4,5,6,-7,8,-9,10]
-- ([1,4,5,6,8,10],[-2,-3,-7,-9])