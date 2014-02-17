import System.IO
import qualified BPython.AST as AST
import Data.Map (fromList)
import Data.List (isPrefixOf)
import Control.Monad (when)

import BPython.Lexer
import Text.ParserCombinators.Parsec
import BPython.GenericParser

import BPython.Profile
import BPython.ProfileCommon
import BPython.ImperativeProfiles
-- import qualified BPython.FunctionalCore as FC

import System.Environment (getArgs)
import System.Process (readProcess)

imperativeCore = Profile isForbiddenInCore checkTypes (fromList coreFunctions)
imperativeExtended = Profile isForbiddenInExtended checkTypes (fromList advancedFunctions)
imperativeAdvanced = Profile isForbiddenInAdvanced checkTypes (fromList advancedFunctions)
-- functionalCore = Profile (const True) FC.checkTypes (fromList coreFunctions)

profiles :: [(String, Profile)]
profiles = [("ImperativeCore", imperativeCore), ("ImperativeExtended", imperativeExtended),
	("ImperativeAdvanced", imperativeAdvanced)]

parseFile :: String -> String -> Either ParseError AST.ASTLoc
parseFile filename = parse file_input filename . lexer . lines

checkCode :: Profile -> AST.ASTLoc -> Either CheckError ()
checkCode prof ast = 
	let nodes = AST.extractNodes (forbiddenExtractor prof) ast in
	if null nodes
		then (typechecker prof) (typemap prof) ast
		else throwUnlocError $ ForbiddenConstructs nodes

analyzeFile :: Profile -> FilePath -> IO Bool
analyzeFile profile filename = do
	input_file <- openFile filename ReadMode
	text <- hGetContents input_file
	ret <- case parseFile filename text of
		(Left parseErr) -> (putStrLn $ "Parsing error : " ++ show parseErr) >> return False
		(Right ast) -> case checkCode profile ast of
						(Left checkError) -> (putStrLn $ "Check error : " ++ show checkError) >> return False
						(Right _) -> return True
	hClose input_file
	return ret

runPython :: FilePath -> [String] -> IO ()
runPython filename args = do
	output <- readProcess "python3" args "" -- TODO: Get python commands somehow
	putStrLn output

processArgs :: [String] -> Maybe (String, String, [String])
processArgs args
	| null after = Nothing
	| null others = Nothing
	| otherwise = Just (tail (dropWhile (/= '=') profArg), last others, others)
	where
		(before, after@(~(profArg:after'))) = span (not . isPrefixOf "--profile=") args
		others :: [String]
		others = before ++ after'

main = do
	args <- getArgs
	case processArgs args of
		Nothing -> putStrLn "Error: Need to specify profile (--profile=<ProfileName>) and input file"
		(Just (profileName, filename, otherArguments)) -> case lookup profileName profiles of
			(Just profile) -> do
				success <- analyzeFile profile filename
				when success (runPython filename otherArguments)
			Nothing -> putStrLn $ "Error: Unknown profile: " ++ profileName
