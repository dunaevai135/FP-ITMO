module MyProducts (rightProduct4, diagonalProduct4, botomProduct4, leftdiagonalProduct4) where
import MyArray

apstractProduct n x y dx dy = foldl (\p i-> p * (getaAt (x+dx*i) (y+dy*i))) 1 [1..n]

rightProduct4 x y = apstractProduct 4 x y 0 1
diagonalProduct4 x y = apstractProduct 4 x y 1 1
botomProduct4 x y = apstractProduct 4 x y 1 0
leftdiagonalProduct4 x y = apstractProduct 4 x y 1 (-1)

