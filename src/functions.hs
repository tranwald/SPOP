import System.IO
import Data.List

-- sprawdza warunki gry po wierszach (porównanie wczytanych wartości z polami 
-- zajmowanymi przez bloki)
checkRows :: [[Bool]] -> [[Int]] -> Bool
checkRows xss rss = (map checkRow xss) == rss
					where checkRow xs = [length x | x <- group xs,
													all (==True) x]

tests = [[True, True, False, False, True, False, False], 
	  [True, True, True, True, False, True, True]]

tests' = [[True, True, False, False, True, True, False], 
	  [True, True, True, True, False, False, True]]


-- testowe listy r - rzędy, c - kolumny
trs :: [[Int]]
trs = [[2,1], [4,2]]
rss :: [[Int]]
rss = [[3], [2, 2], [2, 2], [3], [1], [1], [1], [1], [2], [3]]
css :: [[Int]]
css = [[2, 1], [4, 2], [1, 7], [4], [2]]


type Length = Int
type Candidates = [Int]
type Box = Int
data Block = Block (Maybe Box) Length Candidates deriving (Show)

-- tworzy listę list bloków na podstawie wejściowych prametrów, każda lista to 
-- jedna kolumna (ma tyle elementów ile bloków ma być w kolumnie)
-- rss - rzędy, css - kolumny
makeBlocks :: [[Int]] -> [[Int]]-> [[Block]]
makeBlocks rss css  = [[makeBlock n rss cs  | n <- init [0..length cs]] | cs <- css]

-- tworzenie pojedynczego bloku (Nothing, długość, dostępne pozycje), przez 
-- spawdzenie oganiczeń od innych bloków w kolumnie
-- n - indeks aktualnie przetwazanego bloku, 
-- cs - wartości dla jednej kolumny,
-- rss - wszystkie rzędy 
makeBlock :: Box -> [[Int]] -> [Int] -> Block
makeBlock n rss cs = Block Nothing len candidates
						where 
							candidates     = trim $ genCands upper lower rss
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
