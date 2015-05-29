
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
				printRows xss

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

-- wypisz na stdout odpowiedni znak w zależności czy pole jest zajęte czy nie
printBox :: Bool -> IO ()
printBox x = if x then putChar '\9608' else putChar '\9617'


-- mapuje printBox na wszystkie wiersze
printRows :: [[Bool]] -> IO ()
printRows = mapM_ printRow
			where printRow xs = do
								  mapM_ printBox xs
								  putChar '\n'

rss = [[True, True, False, False, True, False, False], 
	  [True, True, True, True, False, True, True]]

rss' = [[True, True, False, False, True, True, False], 
	  [True, True, True, True, False, False, True]]

rss'' = [[True, True, False, False, False, False, False], 
	  [True, True, True, True, False, True, True]]

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

