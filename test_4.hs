-- Jumlah Uang Sen - jumlahSen(q,d,n,p)
-- Definisi dan SPesifikasi
jumlahSen :: Int -> Int -> Int -> Int -> (Int,Int)
	{- jumlahSen(q,d,n,p) menghasilkan jumlah uang (dollar,sen) yang
	ada jika pengguna memiliki beberapa uang koin quarter, dime, nickel,
	dan penny -}
-- Realisasi
jumlahSen q d n p = (div ((q*25)+(d*10)+(n*5)+p) 100,mod ((q*25)+(d*10)+(n*5)+p) 100)
-- Contoh Aplikasi
-- jumlahSen 10 5 13 40
-- jumlahSen 13 12 4 30
