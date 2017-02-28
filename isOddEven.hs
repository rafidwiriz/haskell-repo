-- Mutual Recursion - isOdd(x) dan isEven(x)
-- Definisi dan Spesifikasi
isOdd :: Int -> Bool
	{- isOdd(x) mengeluarkan nilai true jika elemen x bernilai ganjil. Nilai ganjil yang dimaksud adalah nilai isOdd(x) sama dengan nilai isOdd(1) (mutual recursion). -}
isEven :: Int -> Bool
	{- isEven(x) mengeluarkan nilai true jika elemen x bernilai genap. Nilai genap yang dimaksud adalah nilai isEven(x) sama dengan nilai isOdd(1) (mutual recursion). -}
-- Realisasi
isOdd x = if ((x==0) || (x < 0)) then False else (isEven (x))&& (not(isOdd(x-1)))
isEven x = if ((x==1) || ( x< 0)) then False else (isOdd x) && (not(isEven(x-1)))

-- Contoh Aplikasi
-- isOdd 3
-- isOdd 2
-- isEven 4
