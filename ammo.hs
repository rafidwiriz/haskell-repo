-- Definisi dan Spesifikasi
isEmpty :: [<type_elemen>] -> Bool
	-- isEmpty(l) true jika list of elemen l kosong
konso :: <type_elemen> -> [<type_elemen>] -> [<type_elemen>]
	-- konso(x,l) menghasilkan sebuah list dari x (sebuah
	-- elemen) dan l (list of elemen),
	-- dengan x sebagai elemen pertama: x o l -> l'
konsDot :: [<type_elemen>] -> <type_elemen> -> [<type_elemen>]
	-- konsDot(l,x) menghasilkan sebuah list dari l (list of
	-- elemen) dan x (sebuah elemen),
	-- dengan x sebagai elemen terakhir: l • x -> l' 
isOneElmt :: [<type_elemen>] -> Bool
	-- isOneElmt(l) true jika list of integer l hanya
	-- mempunyai satu elemen 
nbElmt :: [<type_elemen>] -> Int
	-- NbElmt(l) menghasilkan banyaknya elemen list, nol
	-- jika list kosong 
isMember :: <type_elemen> -> [<type_elemen>] -> Bool
	-- isMember(x,l) true jika x adalah elemen list l
copy :: [<type_elemen>] -> [<type_elemen>]
	-- copy(l) menghasilkan list yang identik dengan list
	-- asal
isEqual :: [<type_elemen>] -> [<type_elemen>] -> Bool
	-- isEqual(l1,l2) true jika semua elemen list l1 sama
	-- dengan l2: sama urutan dan sama nilainya 
konkat :: [<type_elemen>] -> [<type_elemen>] -> [<type_elemen>]
	-- konkat(L1,L2) menghasilkan konkatenasi 2 list,
	-- dengan list l2 "sesudah" list l1 
elmtKeN :: Int -> [<type_elemen>] -> <type_elemen>
	-- elmtKeN(n,l) mengembalikan elemen ke-n dari list l
	-- Prekondisi: n>=0 dan n<=jumlah elemen l 
isXElmtkeN :: <type_elemen> -> Int -> [<type_elemen>] -> Bool
	-- isXElmtkeN(x,n,l) menghasilkan true jika elemen ke-n dari
	-- sebuah list l adalah x
-- Realisasi
isEmpty l = null l
konso x l = [x] ++ l
konsDot l x = l ++ [x]
isOneElmt l = length l == 1
nbElmt l =
	if (isEmpty l) then 0 -- Basis
	else 1 + (nbElmt (tail l)) -- Rekurens
isMember x l =
	if (isEmpty l) then False -- Basis
	else if (head l) == x then True
	else (isMember x (tail l)) -- Rekurens
copy l =
	if (isEmpty l) then [] -- Basis
	else (konso (head l) (copy (tail l)))  -- Rekurens
isEqual l1 l2
	| (isEmpty l1) && (isEmpty l2) = True -- Basis
	| (isEmpty l1) && not (isEmpty l2) = False -- Basis
	| not (isEmpty l1) && (isEmpty l2) = False -- Basis
	| not (isEmpty l1) && not (isEmpty l2) = -- Recc
		((head l1)==(head l2) && (isEqual (tail l1) (tail l2)))
konkat l1 l2 =
	if (isEmpty l1) then l2 -- Basis
	else -- Rekurens
		(konso (head l1) (konkat (tail l1) l2))
elmtKeN n l =
	if (n == 1) then (head l) -- Basis
	else -- Rekurens
		(elmtKeN (n-1) (tail l))
isXElmtkeN x n l =
	if (elmtKeN n l) == x then
		True
	else
		False