finalGrade :: [Int] -> [Int] -> Int
finalGrade grades weights
	| length grades == 1 = head grades
	| otherwise = divide(((head grades * head weights) + productof((tail grades),(tail weights))),(head weights + sumof(tail weights)))

sumof :: [Int] -> Int
sumof list = head list + (sumof(tail list))

productof :: ([Int], [Int]) -> Int
productof (list1, list2)
	| length list1 == 1 = (head list1) * (head list2)
	| otherwise = (head list1) * (head list2) + productof(tail list1, tail list2)

divide :: (Int,Int) -> Int
divide (top, bottom) = div top bottom
