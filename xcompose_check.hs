import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import Data.List (sort, sortBy, intercalate)
import Data.Ord (comparing)

xComposeSystemPath = return $ "/usr/share/X11/locale/en_US.UTF-8/Compose"
xComposeUserPath = do
	homeDir <- getHomeDirectory
	return $ homeDir </> ".XCompose"

type Edge = (String, Trie)
data Trie = Node [Edge] Int deriving Show

buildTrie rawTree =
	foldl trieInsert (Node [] 0) rawTree
	where
		trieInsert (Node edges cnt) [] = Node edges (cnt + 1)
		trieInsert (Node [] cnt) (token:tokens) = Node [(token, trieInsert (Node [] 0) tokens)] cnt
		trieInsert (Node ((label, t):edges) cnt) (token:tokens)
			| label == token = Node ((label, trieInsert t tokens):edges) cnt
			| otherwise = 
				let (Node edges' cnt') = trieInsert (Node edges cnt) (token:tokens) in
				Node ((label, t):edges') cnt'

trieErrors trie =
	map (\(suberrors, path) -> (suberrors, reverse path)) $ traverse trie []
	where
		traverse trie path =
			case trie of
				Node [] 1 -> []
				Node [] _ -> [(0, path)]
				Node edges 0 -> concatMap (\(label, subtrie) -> traverse subtrie (label:path)) edges
				Node edges _ -> [(sum (map (numChildren . snd) edges), path)]
		numChildren (Node edges cnt) = cnt + sum (map (numChildren . snd) edges)

readCompose path = do
	s <- readFile path
	let entries' = [takeWhile (\word -> head word /= '#') $ words line | line <- lines s]
	let entries = [line | line <- entries', not $ null line]
	let rawTree = [takeWhile (\word -> head word /= ':') line | line <- entries, not ("include" `elem` line)]
	return rawTree

main = do
	systemPath <- xComposeSystemPath
	userPath <- xComposeUserPath
	printf "System XCompose path: %s\n" systemPath
	printf "User XCompose path: %s\n\n" userPath
	systemCompose <- readCompose systemPath
	userCompose <- readCompose userPath
	let trie = buildTrie (systemCompose ++ userCompose)
	let errors = trieErrors trie
	printf "Errors:\n%s\n"
		$ unlines 
		$ map (\(children, path) -> printf "%d: %s" children (intercalate " " path))
		$ sortBy (comparing fst)
		$ filter (\(children, path) -> children /= 0) errors
	let (Node edges cnt) = trie
	printf "Roots:\n%s\n" (intercalate "\n" $ map ((++) "    ") $  sort $ map fst edges)
