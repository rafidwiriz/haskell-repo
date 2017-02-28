-- Jumlah Digit - sumOfDigits(x)
-- Definisi dan Spesifikasi
sumOfDigits :: Int -> Int
	{- sumOfDigits(x) menghasilkan nilai penjumlahan digit-digit sebuah angka x yang nilainya bisa positif maupun negatif. -}
-- Realisasi
sumOfDigits x
	| abs x == 0 = -- Basis-0
		0
	| otherwise = -- Rekurens
		sumOfDigits (div (abs x) 10) + mod (abs x) 10
-- Contoh Aplikasi
-- sumOfDigits 234
-- sumOfDigits (-45)