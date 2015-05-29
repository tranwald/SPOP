
module Main where

import Data.List (group, inits)
import System.IO

main :: IO()
main = do
		fileName <- readFileName
		if null fileName
			then do
				putStrLn "No file to read. Program terminates"
				return ()
			else do
				(rowsClues, colsClues) <- loadClues fileName
				putStrLn $ show rowsClues
				putStrLn $ show colsClues
				printRows rss

-- wczytaj nazwę pliku ze wskazówkami
-- zwraca nazwę pliku
readFileName :: IO String
readFileName = do
				putStrLn "Please provide the name of the file with clues: "
				fileName <- getLine
				return fileName

-- wczytaj i zwróć wskazówki dla kolumn i wierszy
-- fileName - nazwa pliku do wczytania
loadClues :: FilePath -> IO ([[Int]], [[Int]])
loadClues fileName = do
					inh <- openFile fileName ReadMode
					rawRowsClues <- hGetLine inh
					rawColsClues <- hGetLine inh
					hClose inh
					let rowsClues = read rawRowsClues :: [[Int]]
					    colsClues = read rawColsClues :: [[Int]]
					return (rowsClues, colsClues)

-- sprawdza czy dana lista jest podsekwencją innej listy
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

-- sprawdza czy zapełnienie wierszy nie łamie reguł wynikających ze wskazówek
-- xss - lista pól: True - zajęte pole, False - puste
-- rss - lista list ze wskazówkami dla wierszy
checkRows :: [[Bool]] -> [[Int]] -> Bool
checkRows xss rss =  allTrue $ zipWith isSubsequenceOf (map groupRows xss) rss 
					where 
						groupRows xs = [length x | x <- group xs, allTrue x]
						allTrue = all (==True)

-- wypisz na stdout odpowiedni znak w zależności czy pole jest zajęte czy nie
printBox :: Bool -> IO ()
printBox x = if x then putChar '\9608' else putChar '\9617'


-- mapuje printBox na wszystkie wiersze
printRows :: [[Bool]] -> IO ()
printRows = mapM_ printRow
			where printRow xs = do
								  mapM_ printBox xs
								  putChar '\n'

rss :: [[Bool]]
rss = [[True, True, False, False, True, False, False], 
	  [True, True, True, True, False, True, True]]

rss' :: [[Bool]]
rss' = [[True, True, False, False, True, True, False], 
	  [True, True, True, True, False, False, True]]

rss'' :: [[Bool]]
rss'' = [[True, True, False, False, False, False, False], 
	  [True, True, True, True, False, True, True]]

rs :: [[Int]]
rs = [[2,1], [4,2]]



r :: [[Int]]
r = [[3], [2, 2], [2, 2], [3], [1], [1], [1], [1], [2], [3]]

c :: [[Int]]
c = [[2, 1], [4, 2], [1, 7], [4], [2]]