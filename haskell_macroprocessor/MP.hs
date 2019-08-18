module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


-- Returns a list of items whose associated string matches the search string
lookUp :: String -> [(String, a)] -> [a]
lookUp sym list
  = [out | (x, out) <- list, x == sym]

-- Breaks a string into individual separators and words
split :: [Char] -> String -> (String, [String])
split _ ""    = ("", [""])
split a b@(x:xs)
  | elem x a  = (x:a', "":b')
  | otherwise = (a', (x:y):ys)
  where
    (y: ys)   = b'
    (a', b')  = split a xs

-- Combines individual separators and words to form a string
combine :: String -> [String] -> [String]
combine "" ys = ys
combine a@(x:xs) b@(y:ys)
  = y : [x] : combine xs ys

-- Takes the contents of a file, where each line is a string, and returns a 
-- pair of keywords and definitions
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs input
  = [(m, n)] ++ getKeywordDefs (tail input)
  where
    m      = head b
    p      = combine (tail a) (tail b)
    n      = concat p
    (a, b) = split " " (head input)

-- Replaces the keywords in a text file with their keyvalues from the info file
expand :: FileContents -> FileContents -> FileContents
expand text info
  = concat (combine x (map (`replaceWord` kwrdDefs) y))
  where
    (x, y)   = split separators text
    (a, b)   = split "\n" info
    kwrdDefs = getKeywordDefs b
-- Helper function which replaces one word with its definition
replaceWord :: String -> KeywordDefs -> String
replaceWord y kwrdDefs
  | y == ""    = []
  | elem '$' y = head (lookUp y kwrdDefs)
  | otherwise  = y

-- Extension of the expand function (doesn't work)
expandExtended :: FileContents -> FileContents -> FileContents
expandExtended text info
  = concat ([expand text sen ++ "-----\n" | sen <- info'])
  where
    (_, info') = split "#" info

main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expandExtended t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")

