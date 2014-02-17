module BPython.Lexer (Token(..), TokenPos, lexer) where

import Data.Char (isSpace)
import Data.List (intersperse, unfoldr)
import Text.Regex.PCRE

type TokenPos = (Token, (Int, Int))

-- We don't actually need exact values of literals => they all contain plain token strings
data Token = NewLine | Indent | Dedent | Keyword String | ID String | Operator String |
				Delimiter String | StringLiteral String | IntegerLiteral String | FloatingLiteral String | InvalidIndent | ErrorToken String
	deriving (Show, Eq)

---------------------------
-- Lines and indentation --
---------------------------
isOpenBracket :: Char -> Bool
isOpenBracket c = c `elem` ['(', '[', '{']

isClosedBracket :: Char -> Bool
isClosedBracket c = c `elem` [')', ']', '}']

bracketBalance :: String -> Integer
bracketBalance = sum . map value
	where
		value char
			| isClosedBracket char = -1
			| isOpenBracket char = 1
			| otherwise = 0

-- Throws away empty lines, processes line joinings, adds line numbers
-- TODO: Check whether line ending with "\ #..." is valid
processLines :: [String] -> [(Int, String)]
processLines = joinLines . removeEmptyLines . zipWith (,) [1..] . removeComments
	where
		-- XXX: Breaks string literals containing #
		removeComments :: [String] -> [String]
		removeComments = map (takeWhile (/= '#'))

		removeEmptyLines :: [(Int, String)] -> [(Int, String)]
		removeEmptyLines = filter (not . (all isSpace . snd))

		needsJoining :: String -> (Bool, Maybe String)
		needsJoining s
			| last s == '\\'		= (True, Just $ init s)
			| bracketBalance s > 0	= (True, Just s)
			| otherwise				= (False, Nothing)

		joinLines :: [(Int, String)] -> [(Int, String)]
		joinLines ((nx, x) : (ny, y) : ys) = case needsJoining x of
									(True, (Just s)) -> joinLines ((nx, (s ++ y)) : ys)	
									(False, Nothing) -> (nx, x) : joinLines ((ny, y) : ys)
		joinLines xs = xs

-- Adds indentation counts to lines
addIndentation :: [(Int, String)] -> [(Int, Int, String)]
addIndentation = map extractIndentation
	where
		expandTabs = expandTabs' 0
						where
							expandTabs' n (c:cs)
								| c == ' ' = expandTabs' (n + 1) cs
								| c == '\t' = n + (8 - n `mod` 8) + expandTabs' 0 cs
							expandTabs' n "" = n
		extractIndentation (n, s) =  let (whitespace, rest) = span isSpace s
										in
											(n, expandTabs whitespace, rest)

addNumbers :: [(a, b)] -> [(Int, a, b)]
addNumbers = zipWith addNum [1..]
	where
		addNum num (x, y) = (num, x, y)

------------------
-- Tokenization --
------------------
keywords :: [String]
keywords = ["False","class","finally","is","return","None","continue","for","lambda",
			"try","True","def","from","nonlocal","while","and","del","global","not",
			"with","as","elif","if","or","yield","assert","else","import","pass","break",
			"except","in","raise"]

regexID :: String
regexID = "^([[:alpha:]]|_)([[:alnum:]]|_)*"

regexKeyword :: String
regexKeyword = "^(" ++ concat (intersperse "|"  (map (++"\\b") keywords)) ++ ")"

isQuote :: Char -> Bool
isQuote '\'' = True
isQuote '"' = True
isQuote _ = False

isStringLiteral :: String -> Bool
isStringLiteral "" = False
isStringLiteral (p:c:_) 
	| p `elem` ['r', 'R'] = isQuote c
isStringLiteral (c:_) = isQuote c

-- TODO: Allow only ', ", ''' and """
processStringLiteral' :: Bool -> String -> (Token, String)
processStringLiteral' raw (q : cs) = let
									(quotes, rest) = span (== q) cs
									mx = length quotes + 1
									process  qn _ _ [] 
										| mx /= qn = (ErrorToken $ "unmatched " ++ [q], "")
									process qn token esc s@(~(c:cs))
										| mx == qn = (StringLiteral ((if raw then ('r':) else id) $ reverse token), s)  -- ugly
										| c == q = process (if esc then qn else qn + 1) (c : token) False cs
										| c == '\\' && not raw = process 0 (c : token) True cs
										| c == '\n' && mx == 1 = (ErrorToken $ "newline in short string", "")
										| otherwise = process 0 (c : token) False cs
								in
									if mx `mod` 2 == 0
										then (StringLiteral (q:quotes), rest) -- empty string
										else process 0 (q : quotes) False rest

processStringLiteral :: String -> (Token, String)
processStringLiteral s@(c:cs)
	| c `elem` ['r', 'R'] = processStringLiteral' True cs
	| otherwise = processStringLiteral' False s -- Maybe add another error checking here?

regexIntegerLiteral :: String
regexIntegerLiteral = "^(0[oO][0-7]+|0[xX][0-9a-fA-F]+|0[bB][01]+|0+|[1-9][[:digit:]]*)"

regexFloatingPointLiteral :: String
regexFloatingPointLiteral = let
								intpart = "([[:digit:]]+)"
								fraction = "(\\.[[:digit:]]+)"
								exponent = "([eE][+-]?[[:digit:]]+)"
								pointfloat = "((" ++ intpart ++ "?" ++ fraction ++ ")|(" ++ intpart ++ "\\.))"
								exponentfloat = "((" ++ intpart ++ "|" ++ pointfloat ++ ")" ++ exponent ++ ")"
							in
								"^(" ++ exponentfloat ++ "|" ++ pointfloat ++ ")"

regexOperator1 :: String
regexOperator1 = "^(//|<<|>>|<=|>=|[~<>]|==|!=)"

regexOperator2 :: String
regexOperator2 = "^(\\*\\*)|([+\\-*%&/|^])"

regexDelimiter :: String
regexDelimiter = "^([()[\\]{},;.:@=]|(([+\\-*/%&|^]|//|>>|<<|\\*\\*)=))"

extractToken :: [(String -> Bool, String -> (Token, String))] -> String -> Maybe (Token, String)
extractToken _ "" = Nothing
extractToken [] s = Just (ErrorToken s, "")
extractToken ((p, cons) : ps) cs
	| p cs	= Just $ cons cs
	| otherwise = extractToken ps cs

tokenize :: [(String -> Bool, String -> (Token, String))] -> String -> [Token]
tokenize ps = unfoldr (extractToken ps . dropWhile isSpace)

wrapRegex :: String -> (String -> Token) -> (String -> Bool, String -> (Token,String))
wrapRegex regex cons = ((=~ regex), (\s -> case s =~ regex of ("", tokenString, rest) -> (cons tokenString, rest)))

tokenMap :: [(String -> Bool, String -> (Token, String))]
tokenMap = [(wrapRegex regexFloatingPointLiteral FloatingLiteral), (wrapRegex regexIntegerLiteral IntegerLiteral),
			(wrapRegex regexKeyword Keyword), (isStringLiteral, processStringLiteral), (wrapRegex regexID ID),
			(wrapRegex regexOperator1 Operator), (wrapRegex regexDelimiter Delimiter), (wrapRegex regexOperator2 Operator)]

-----------
-- Lexer --
-----------

-- TODO: Add proper error handling
inferIndentationTokens :: [(Int, Int, [Token])] -> [TokenPos]
inferIndentationTokens = process [0]
	where
		addLineNumbers n tokens = map (\t -> (t, (n, n))) tokens
		process :: [Int] -> [(Int, Int, [Token])] -> [TokenPos]
		process s [] = replicate (length $ takeWhile (> 0) s) (Dedent, (-1, -1)) -- TODO: Different n
		process [] ts = process [0] ts
		process s@(i:is) ((n, ind, tokens) : ts)
			| ind > i 	= addLineNumbers n (Indent : tokens) ++ (NewLine, (n, n)) : process (ind : s) ts
			| ind == i 	= addLineNumbers n tokens ++ (NewLine, (n, n)) : process s ts
			| ind < i	= let
								(dropped, s'@(i':is')) = span (> ind) s
								dcount = length dropped
							in
								if i' == ind 
									then addLineNumbers n (replicate dcount Dedent ++ tokens) ++ (NewLine, (n, n)) : process s' ts
									else [(InvalidIndent, (n, n))]

concatenateStringLiterals :: [TokenPos] -> [TokenPos]
concatenateStringLiterals (((StringLiteral s1), n) : ((StringLiteral s2), _) : ls) = concatenateStringLiterals $ ((StringLiteral $ s1 ++ s2), n) : ls
concatenateStringLiterals (l:ls) = l : concatenateStringLiterals ls
concatenateStringLiterals [] = []

applyThird :: (c -> d) -> (a, b, c) -> (a, b, d)
applyThird f (x, y, z) = (x, y, f z)

lexer :: [String] -> [TokenPos]
lexer = concatenateStringLiterals . inferIndentationTokens . map (applyThird (tokenize tokenMap)) . addIndentation . processLines
