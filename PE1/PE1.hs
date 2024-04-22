module PE1 where


findInList :: (Eq a) => [a] -> [a] -> [Int]

helper [] _ _ x = x
helper (a:b) (c:d) n x  | null (a:b) = x
                              | a == c = helper b d (n+1) (x ++ [n])
                              | a /= c = helper (a:b) d (n+1) x

findInList (a:b) (c:d) = helper (a:b) (c:d) 0 []

append a [] = [a]
append a (x:xs) = x : append a xs

order matrix = let rows = length matrix
                   col = length (head matrix)
                   mat = [[(matrix !! i !! j, (i, j)) | i <- [0..(rows-1)], j <- [0..(col-1)], i + j == k] | k <- [0..rows+col-2]]
                in concat mat


help :: (Eq a1, Eq t1, Num t1, Num t2) => [(a1, (a2, b1))] -> [(a1, (a3, b2))] -> t2 -> [[(a3, b2)]] -> [(a3, b2)] -> t1 -> t1 -> [[(a3, b2)]]
help [] _ _ x y _ _ = append y x
help ((a,(c,d)):b) ((e,(f,g)):h) n x y num temp | a == e && num == 0 = help b h (n+1) (append y x) [(f,g)] (temp-1) temp
                                                | a == e && num /= 0 = help b h (n+1) x (y ++ [(f,g)]) (num-1) temp
                                                | a /= e = help ((a,(c,d)):b) h (n+1) x y num temp
                                                | null ((a,(c,d)):b) && num == 0 = append y x
                                                | otherwise = x

findInMatrix :: (Eq a) => [[a]] -> [[a]] -> [[(Int, Int)]]

findInMatrix (a:b) (c:d) = help (order (a:b)) (order (c:d)) 0 [] [] (length (head (a:b))) (length (head (a:b)))
