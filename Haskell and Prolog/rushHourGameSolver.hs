{- Yelnil Gabo, 70179064, g3d6 -}
import List


-------------------------------------------
-- main function
rush_hour :: [String] -> IO()
rush_hour start = putStr (displayResults (reverse (getPath [start] [] [])))
--rush_hour start =  ( length (getPath [start] [] []))


-------------------------------------------
-- This function gets the path from the starting state(unexplored) to the 
-- goal state. This function returns that path.
getPath :: [[String]] -> [[String]] -> [[String]] -> [[String]]
getPath unexplored path explored
	| null unexplored		    = []
	| isGoal (head unexplored) == True  = (head unexplored):path
	| elem (head unexplored) explored   = getPath (tail unexplored) path explored
	| (not (null result))		    = result
	| otherwise			    = getPath (tail unexplored) path ((head unexplored):explored)
		where result                = getPath (generateStates (head unexplored)) ((head unexplored):path) ((head unexplored):explored)


-------------------------------------------
-- Check if goal achieved
isGoal :: [String] -> Bool
isGoal [] = False
isGoal x
	| drop 4 ( head ( drop 2 x ) ) == "XX" = True
	| otherwise                            = False


-------------------------------------------
-- get states possible from current state
generateStates :: [String] -> [[String]]
generateStates [] = []
generateStates current = concat [getLeftMoveStates current, getDownMoveStates current, getRightMoveStates current, getUpMoveStates current ]


-------------------------------------------
-- get all states possible where things moved left
getLeftMoveStates :: [String] -> [[String]]
getLeftMoveStates current
	| null current = []
	| otherwise = getLeftHelper current [] 6

getLeftHelper :: [String] -> [[String]] -> Int -> [[String]]
getLeftHelper current list row
	| row == 0                           = list
	| null current                       = list
	| null (head (drop (row-1) (moveLeftThisRow current row))) = getLeftHelper current list (row-1) 
	| otherwise                          = getLeftHelper current ((moveLeftThisRow current row):list) (row-1)

-- get all states possible where things moved right
getRightMoveStates :: [String] -> [[String]]
getRightMoveStates current
	| null current = []
	| otherwise = getRightHelper current [] 6

getRightHelper :: [String] -> [[String]] -> Int -> [[String]]
getRightHelper current list row
	| row == 0                            = list
	| null current                        = list
	| null (head (drop (row-1) (moveRightThisRow current row) ))= getRightHelper current list (row-1) 
	| otherwise                           = getRightHelper current ((moveRightThisRow current row):list) (row-1)

-- get all states possible where things moved up
getUpMoveStates :: [String] -> [[String]]
getUpMoveStates current
	| null current = []
	| otherwise = getUpHelper current [] 6

getUpHelper :: [String] -> [[String]] -> Int -> [[String]]
getUpHelper current list row
	| row == 0                           = list
	| null current                       = list
	| null (head (drop (row-1) (moveLeftThisRow (transpose (current)) row))) = getUpHelper current list (row-1) 
	| otherwise                          = getUpHelper current (transpose (moveLeftThisRow (transpose (current)) row):list) (row-1)

-- get all states possible where things moved up
getDownMoveStates :: [String] -> [[String]]
getDownMoveStates current
	| null current = []
	| otherwise = getDownHelper current [] 6

getDownHelper :: [String] -> [[String]] -> Int -> [[String]]
getDownHelper current list row
	| row == 0                           = list
	| null current                       = list
	| null (head (drop (row-1) (moveRightThisRow (transpose (current)) row))) = getDownHelper current list (row-1) 
	| otherwise                          = getDownHelper current (transpose (moveRightThisRow (transpose (current)) row):list) (row-1)


-------------------------------------------
-- move thing, indicated, towards left on the row provided
moveLeftThisRow :: [String] -> Int -> [String]
moveLeftThisRow list row
	| null list = []
	| row == 1  = ((moveLeft (head list)):(tail list))
	| otherwise = ((head list):(moveLeftThisRow (tail list) (row-1)))

-- move right on the row provided
moveRightThisRow :: [String] -> Int -> [String]
moveRightThisRow list row
	| null list = []
	| row == 1  = (moveRight (head list)):(tail list)
	| otherwise = (head list):(moveRightThisRow (tail list) (row-1))


-------------------------------------------
-- moves a thing, on the line, to the left
-- edit string to do move, eg [---xx--] -> [--xx---]
moveLeft :: String -> String
moveLeft line
	| null line          = []
	| not (x == '+')     = (replaceStringwithAnother line ((getPos line x) - 2) (generateObjectString x (getLength line x))) 
	| otherwise          = []
		where x = getThingtoMoveLeft line

-- moves a thing, on the line, to the right
moveRight :: String -> String
moveRight line = reverse (moveLeft (reverse line))


-------------------------------------------
-- get position of thing y -- [123456]
getPos :: String -> Char -> Int 
getPos line y
	| null line         = -1
	| not (elem y line) = -1 
	| (head line) == y  = 1
	| otherwise         = 1 + getPos (tail line) y 

-- get length of thing y
getLength :: String -> Char -> Int 
getLength line y
	| (head line) == y && (null (tail line))       = 1
	| (head line) == y && (head (tail line)) /= y  = 1
	| (head line) == y && (head (tail line) == y)  = 1 + getLength (tail line) y 
	| otherwise                                    = getLength (tail line) y

-- traverse till char of interest is head of tail. eg [firstpartofstring]:[-xx---]
-- then replace with given substring the thing of interest. eg. "-xx" with "xx-"
replaceStringwithAnother :: String -> Int -> String -> String
replaceStringwithAnother orig pos other
	| pos == 0  = other ++ (drop (length other) orig )
	| otherwise = (head orig):(replaceStringwithAnother (tail orig) (pos-1) other)

-- cons up a list with x characters, of x length and append a "-" at end
generateObjectString :: Char -> Int -> String
generateObjectString char length
	| length == 0 = ['-']
	| length > 0  = char:(generateObjectString char (length-1))


-------------------------------------------
-- return character of truck/car that can be moved left
getThingtoMoveLeft :: String -> Char
getThingtoMoveLeft [] = '+'
getThingtoMoveLeft (x:(y:ys))
	| null ys                                          = '+'
	| (x == '-' && (y == (head ys)) && not (y == '-')) = y
	| otherwise                                        = getThingtoMoveLeft (y:ys)

-------------------------------------------
-- displays the path towards the goal
displayResults :: [[String]] -> String
displayResults states
	| states == [] = []
	| otherwise = (displayHelpler (head states)) ++ (displayResults (tail states))

-- displayResults helper, converts array of strings to one big string
displayHelpler :: [String] -> String
displayHelpler state
	| state == [] = "\n"
	| otherwise = (head state) ++ ("\n" ++ (displayHelpler (tail state)))
