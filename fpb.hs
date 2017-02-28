fpb :: Int -> Int -> Int
fpbmin :: Int -> Int -> Int -> Int
min2 :: Int -> Int -> Int
--haha
min2 a b 	
	| (a<b) = a
	| otherwise = b
fpb m n =
	let x = min2 m n in 
		fpbmin m n x
fpbmin a b x = if ((mod a x == 0) && ( mod b x==0)) then x
		else 
		fpbmin a b (x-1)
