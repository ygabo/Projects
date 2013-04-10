{- Yelnil Gabo, 70179064, g3d6 -}

{- 1 -}
myremoveduplicates :: (Eq a) => [a] -> [a]
myremoveduplicates list1
	| null list1 = []
	| elem (head list1) (tail list1) = myremoveduplicates (tail list1)
	| otherwise = (head list1) : myremoveduplicates (tail list1)

myremoveduplicates_pm :: (Eq a) => [a] -> [a]
myremoveduplicates_pm [] = []
myremoveduplicates_pm (x:xs)
	| elem x xs = myremoveduplicates_pm xs
	| otherwise = x : myremoveduplicates_pm xs

{- 2 -}
myintersection :: (Eq a) => [a] -> [a] -> [a]
myintersection list1 list2
	| null list1 = []
	| null list2 = []
	| elem (head list1) list2 = (head list1):myintersection (tail list1) list2
	| otherwise = myintersection (tail list1) list2 

myintersection_pm :: (Eq a) => [a] -> [a] -> [a]
myintersection_pm [] _ = []
myintersection_pm _ [] = []
myintersection_pm (x:xs) (y:ys)
	| elem x (y:ys) = x:myintersection_pm xs (y:ys)
	| otherwise = myintersection_pm xs (y:ys) 

{- 3 -}
mynthtail :: Int -> [a] -> [a]
mynthtail x list1
	| x == 0 = list1
	| null list1 = []
	| otherwise = if x > 0 then mynthtail (x-1) (tail list1) else []

mynthtail_pm :: Int -> [a] -> [a]
mynthtail_pm _ [] = []
mynthtail_pm 0 (x:xs) = (x:xs)
mynthtail_pm y (x:xs) = mynthtail_pm (y-1) xs

{- 4 -}
mylast :: (Eq a) => [a] -> [a]
mylast list1
	| null list1 = []
	| null (tail list1) = list1
	| otherwise = mylast (tail list1)

mylast_pm :: (Eq a) => [a] -> [a]
mylast_pm [] = []
mylast_pm (x:xs)
	| null xs = [x]
	| otherwise = mylast_pm xs

{- 5 -}
myappend_pm :: [a] -> [a] -> [a]
myappend_pm [] list2 = list2
myappend_pm (x:xs) list2 = x:(myappend_pm xs list2)

myreverse :: [a] -> [a]
myreverse list1
	| null list1 = []
	| null (tail list1) = list1
	| otherwise = myappend_pm (myreverse (tail list1)) [(head list1)]

myreverse_pm :: [a] -> [a]
myreverse_pm [] = []
myreverse_pm (x:xs)
	| null xs = (x:xs)
	| otherwise = myappend_pm (myreverse_pm xs) [x]

{- 6 -}
myreplaceall :: (Eq a) => a -> a -> [a] -> [a]	
myreplaceall x y list1
	| null list1 = []
	| y == (head list1) = x:(myreplaceall x y (tail list1))
	| otherwise = (head list1):(myreplaceall x y (tail list1))

myreplaceall_pm :: (Eq a) => a -> a -> [a] -> [a]	
myreplaceall_pm _ _ [] = []
myreplaceall_pm x y (z:zs)
	| y == z = x:(myreplaceall_pm x y zs)
	| otherwise = z:(myreplaceall_pm x y zs)

{- 7 -}
myordered list
	| null list = True
	| null (tail list) = True
	| (head list) <= (head(tail list)) = myordered (tail list)
	| otherwise = False

myordered_pm [] = True
myordered_pm (x:[]) = True
myordered_pm (x:xs)
	| x <= (head xs) = myordered_pm (xs)
	| otherwise = False
