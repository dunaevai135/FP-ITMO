module MyFilter (lineFindMax, findMax) where
import MyProducts
import MyArray

lineFindMax x = 
	maximum (
		(map (rightProduct4 x) (take getaSize [0..])) ++ 
		(map (diagonalProduct4 x) (take getaSize [0..])) ++ 
		(map (botomProduct4 x) (take getaSize [0..])) ++ 
		(map (leftdiagonalProduct4 x) (take getaSize [0..]))
	)

findMax = 
	maximum (map lineFindMax (take getaSize [0..]))
