import System.IO
import Data.List
import Data.Maybe

-- dane testowe
trs :: [[Int]]
trs = [[2,1], [4,2]]
rss :: [[Int]]
rss = [[3], [2, 2], [2, 2], [3], [1], [1], [1], [1], [2], [3]]
css :: [[Int]]
css = [[2, 1], [4, 2], [1, 7], [4], [2], []]
bss :: [[Block]]
bss = [[Block Nothing 2 [0,1,2,3,4,5,6],Block Nothing 1 [3,4,5,6,7,8,9]],
	   [Block Nothing 4 [0,1,2,3],Block Nothing 2 [5,6,7,8]],
	   [Block Nothing 1 [0,1],Block Nothing 7 [2,3]],
	   [Block Nothing 4 [0,1,2,3,4,5,6]],
	   [Block Nothing 2 [0,1,2,3,4,5,6,7,8]],[Empty]]

bss' :: [[Block]]
bss' = [[Block (Just 1) 2 [0,1,2,3,4,5,6],Block (Just 9) 1 [3,4,5,6,7,8,9]],
		[Block (Just 0) 4 [0,1,2,3],Block (Just 8) 2 [5,6,7,8]],
		[Block (Just 0) 1 [0,1],Block (Just 3) 7 [2,3]],
		[Block (Just 0) 4 [0,1,2,3,4,5,6]],
		[Block (Just 1) 2 [0,1,2,3,4,5,6,7,8]],[Empty]]

b :: Block
b = Block (Just 2) 2 [0,1,2,3,4,5,6]
bs :: [Block]
bs = [Block (Just 2) 2 [0,1,2,3,4,5,6],Block(Just 5) 1 [3,4,5,6,7,8,9]]

xss =  [[False,True,True,True,False,False],
		[True,True,False,True,True,False],
		[True,True,False,True,True,False],
		[False,True,True,True,False,False],
		[False,False,True,False,False,False],
		[False,False,True,False,False,False],
		[False,False,True,False,False,False],
		[False,False,True,False,False,False],
		[False,True,True,False,False,False],
		[True,True,True,False,False,False]]

-- typy danych
type Length = Int
type Candidates = [Int]
type Box = Int

-- abstrakcyjny typ dnych reprezentujący pojedyńczy blok
data Block = Empty | Block (Maybe Box) Length Candidates deriving (Show)

-- tworzy listę list bloków na podstawie wejściowych prametrów, każda lista to 
-- jedna kolumna (ma tyle elementów ile bloków ma być w kolumnie)
-- rss - rzędy, css - kolumny
makeAllBlocks :: [[Int]] -> [[Int]]-> [[Block]]
makeAllBlocks rss css  = [makeBlocks rss cs | cs <- css]

-- tworzy listę bloków w kolumnie na podstawie wejściowych prametrów
-- rss - lista list wskazówek rzędów, cs - lista wskazówek kolumny
makeBlocks :: [[Int]] -> [Int]-> [Block]
makeBlocks rss [] = [Empty]
makeBlocks rss cs = [makeBlock n rss cs  | n <- init [0..length cs]]
-- tworzenie pojedynczego bloku (Nothing, długość, dostępne pozycje), przez 
-- spawdzenie oganiczeń od innych bloków w kolumnie
-- n - indeks aktualnie przetwazanego bloku, 
-- cs - wartości dla jednej kolumny,
-- rss - wszystkie rzędy 
makeBlock :: Box -> [[Int]] -> [Int] -> Block
makeBlock n rss cs = Block Nothing len candList
						where 
							candList       = trim $ genCands upper lower rss
							trim     	   = reverse . drop (len - 1) . reverse
							len            = cs !! n
							(upper, lower) = splitAtIdx n cs


-- wyszukuje pozycje na których można umieścić blok
-- nie uwzględnia długości bloku (to jest w makeBlock)
genCands :: [Int] -> [Int] -> [[Int]] -> [Int]
genCands upper lower rss = (init [0..length rss]) \\ outsideCands
							where outsideCands = lowerCands lower rss 
											     ++ upperCands upper

-- wyszukuje pola, na których nie może zostać umieszczony blok ze względu na 
-- inne bloki znjdujące się poniżej/powyżej
upperCands :: [Int] -> [Int]
upperCands upper = init [0..sum upper + length upper]

lowerCands :: [Int] -> [[Int]] -> [Int]
lowerCands lower rss = init [rssLen - (sum lower + length lower)..rssLen]
							where rssLen = length rss

-- dzieli listę na dwie części względem aktulnie sprawdzanej pozycji 
-- o indeksie n
splitAtIdx :: Int -> [a] -> ([a], [a])
splitAtIdx n cs = (upper, lower)
				where
					upper  = take n cs 
					lower  = drop (n + 1) cs

-- sprawdza czy dana lista jest podsekwencją innej listy
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []       _                 = True
isSubsequenceOf _        []                = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b
-- backtracking
backtrack :: [[Block]] -> [[Block]] -> [[Bool]]
backtack  []       _        = undefined
backtrack (xs:xss) (ys:yss) = undefined

-- sprawdza czy zapełnienie wierszy nie łamie reguł wynikających ze wskazówek
-- xss - lista pól: True - zajęte pole, False - puste
-- rss - lista list ze wskazówkami dla wierszy
checkRows :: [[Bool]] -> [[Int]] -> Bool
checkRows xss rss =  allTrue $ zipWith isSubsequenceOf (map groupRows xss) rss 
					where
						groupRows xs = [length x | x <- group xs, allTrue x]
						allTrue = all (==True)

-- wygenerowanie planszy
genBoard :: [[Block]] -> Int -> [[Bool]]
genBoard xss h = transpose $ map (\xs -> genResCol xs [] h) xss

-- wygenerowanie kolumny do planszy wynikowej
genResCol :: [Block] -> [Bool] -> Int-> [Bool]
genResCol []      ys _  = ys
genResCol [Empty] _  h  = replicate h False
genResCol xs      [] h  = genResCol xs (replicate h False) h
genResCol (x:xs)  ys h  = genResCol xs (insertBlock ys x) h 

-- zaznacz blok w kolumnie wynikowej
insertBlock :: [Bool] -> Block -> [Bool]			
insertBlock xs y = insertListAt xs block $ pos y
				where
					block  = replicate (len y) True
					len (Block box n cands) = n
					pos (Block box n cands) = fromJust box 

-- naspisuje fragment listy pod indeksem n inną listą podaną jako argument
insertListAt :: [a] -> [a] -> Int -> [a]
insertListAt [] ys _ = ys
insertListAt xs [] _ = xs
insertListAt xs ys n | n >= 0         = before ++ ys ++ after
					 | n < 0          = error "Negative index"
					 | n >= length xs = error "Index outside the list"
					where
						before = take n xs
						after  = drop (n + length ys) xs
