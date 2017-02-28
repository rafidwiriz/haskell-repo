-- Apakah Date Valid - isDateValid(d,m,y)
-- Definisi dan Spesifikasi
isDateValid :: Int -> Int -> Int -> Bool
	{- isDateValid(d,m,y) mengembalikan nilai true jika d, m, y
	membentuk date yang valid. Definisi date yang valid adalah jika
	elemen hari (d) bernilai antara 1 dan 31, tergantung pada bulan dan
	apakah tahun kabisat atau bukan, elemen bulan (m) bernilai antara 1
	dan 12, dan elemen tahun (y) bernilai lebih dari antara 0 dan 99 -}
isLeapYear :: Int -> Int
	-- isLeapYear menghasilkan mod dari tahun dibagi dengan 4
-- Realisasi
isLeapYear y = mod y 4
isDateValid d m y
	| isLeapYear y == 0 = (y>=0) && (y<=99) && (m>=1) && (m<=12) && (d>=1) && ((((m==1) || (m==3) || (m==5) || (m==7) || (m==8) || (m==10) || (m==12)) && (d<=31)) || (((m==4) || (m==6) || (m==9) || (m==11)) && (d<=30)) || ((m==2) && (d<=29)))
	| otherwise = (y>=0) && (y<=99) && (m>=1) && (m<=12) && (d>=1) && ((((m==1) || (m==3) || (m==5) || (m==7) || (m==8) || (m==10) || (m==12)) && (d<=31)) || (((m==4) || (m==6) || (m==9) || (m==11)) && (d<=30)) || ((m==2) && (d<=28)))
-- Contoh Aplikasi
-- isDateValid 29 2 4
-- isDateValid 10 1 13
