module BPython.GenericParser where

import BPython.ProfileCommon
import Text.ParserCombinators.Parsec hiding (newline, State)
import Text.ParserCombinators.Parsec.Pos (newPos)
import BPython.Lexer
import BPython.PyType
import qualified BPython.AST as AST

-- TODO: Generate those using Template Haskell
isID :: Token -> Bool
isID (ID _) = True
isID _ = False

isStringLiteral :: Token -> Bool
isStringLiteral (StringLiteral _) = True
isStringLiteral _ = False

isIntegerLiteral :: Token -> Bool
isIntegerLiteral (IntegerLiteral _) = True
isIntegerLiteral _ = False

isFloatingLiteral :: Token -> Bool
isFloatingLiteral (FloatingLiteral _) = True
isFloatingLiteral _ = False

isBooleanLiteral :: Token -> Bool
isBooleanLiteral (Keyword "True") = True
isBooleanLiteral (Keyword "False") = True
isBooleanLiteral _ = False

isOperator :: String -> Token -> Bool
isOperator s (Operator s') = s == s'
isOperator _ _ = False

isDelimiter :: String -> Token -> Bool
isDelimiter s (Delimiter s') = s == s'
isDelimiter _ _ = False

isKeyword :: String -> Token -> Bool
isKeyword s (Keyword s') = s == s'
isKeyword _ _ = False

isIndent :: Token -> Bool
isIndent Indent = True
isIndent _ = False

isDedent :: Token -> Bool
isDedent Dedent = True
isDedent _ = False

isNewLine :: Token -> Bool
isNewLine NewLine = True
isNewLine _ = False

pytoken :: (Token -> Bool) -> GenParser TokenPos () TokenPos
pytoken tf = token show (\(_, (l1, l2)) -> newPos "" l1 0) (\t@(x,_) -> if tf x then Just t else Nothing)

operator :: String -> GenParser TokenPos () TokenPos
operator = pytoken . isOperator

delimiter :: String -> GenParser TokenPos () TokenPos
delimiter = pytoken . isDelimiter

keyword :: String -> GenParser TokenPos () TokenPos
keyword = pytoken . isKeyword

extractDataFromToken :: Token -> String
extractDataFromToken (Operator op) = op
extractDataFromToken (Delimiter del) = del
extractDataFromToken (Keyword key) = key
extractDataFromToken (ID id) = id

identifier :: PyParser
identifier = do
	(ID id, loc) <- pytoken isID
	return (AST.ID id, loc)

indent :: PyParser
indent = do
	(_, loc) <- pytoken isIndent
	return (AST.Void, loc)

dedent :: PyParser
dedent = pytoken isDedent >>= \(_, loc) -> return (AST.Void, loc)

newLine :: PyParser
newLine = pytoken isNewLine >>= \(_, loc) -> return (AST.Void, loc)

literal :: PyParser
literal = choice [stringLiteral, integerLiteral, floatingLiteral, booleanLiteral]

stringLiteral :: PyParser
stringLiteral = pytoken isStringLiteral >>= \(_, loc) -> return (AST.Literal PyString, loc)

integerLiteral :: PyParser
integerLiteral = pytoken isIntegerLiteral >>= \(_, loc) -> return (AST.Literal PyInt, loc)

floatingLiteral :: PyParser
floatingLiteral = pytoken isFloatingLiteral >>= \(_, loc) -> return (AST.Literal PyFloat, loc)

booleanLiteral :: PyParser
booleanLiteral = pytoken isBooleanLiteral >>= \(_, loc) -> return (AST.Literal PyBool, loc)

primary :: PyParser
primary = choice [try call, try methodcall, try subscription, try slicing, try atom]  -- XXX: Will probably go wrong on subscription!!!

atom :: PyParser
atom = choice [identifier, literal, enclosure]

subscription :: PyParser
subscription = do
	list <- try atom -- XXX: Should be primary, but leads to infinite recursion (check out chainl1)
	delimiter "["
	index <- expr
	last <- delimiter "]"
	returnASTLoc (AST.Index list index) list last

slicing :: PyParser
slicing = do
	list <- try atom -- XXX: See @subscription
	delimiter "["
	start <- expr
	delimiter ":"
	end <- expr
	last <- delimiter "]"
	returnASTLoc (AST.Slice list start end) list last

enclosure :: PyParser
enclosure = list_display <|> parenth_form

list_display :: PyParser
list_display = do
	lb <- delimiter "["
	elements <- sepBy expr $ delimiter ","
	rb <- delimiter "]"
	returnASTLoc (AST.List elements) lb rb

parenth_form :: PyParser
parenth_form = do
	lb <- delimiter "("
	inner <- expr
	rb <- delimiter ")"
	return inner

-- TODO: Different type for method calls, so that 1.print() would not be possible
methodcall :: PyParser
methodcall = do
	id <- identifier
	delimiter "."
	method <- identifier
	delimiter "("
	args <- sepBy expr $ delimiter ","
	delim <- delimiter ")"
	returnASTLoc (AST.FunctionCall method (id:args)) id delim

call :: PyParser
call = do
	id <- identifier
	delimiter "("
	args <- sepBy expr $ delimiter ","
	delim <- delimiter ")"
	returnASTLoc (AST.FunctionCall id args) id delim


power :: PyParser
power = do
	left <- primary
	pwr <- optionMaybe (operator "**" >> u_expr)
	case pwr of
				(Just right) -> returnASTLoc (AST.BinaryOp "**" left right) left right
				Nothing -> return left 

-- TODO: Add bracketed expresssions

u_expr :: PyParser
u_expr = try unary_op <|> power
	where
		unary_op = do
						(Operator op, loc1) <- choice $ map operator ["+", "-", "~"]
						arg@(_, loc2) <- u_expr
						return (AST.UnaryOp op arg, AST.mergeLocs loc1 loc2)

-- XXX: Breaks associavity, though for our purposes it should not be a problem
makeBinaryOpParser ::
	PyParser -> 					-- left side of the expression
	GenParser TokenPos () TokenPos ->		-- possible operators
	PyParser
makeBinaryOpParser lp ops = let 
								parser = do
									left <- lp
									mbop <- optionMaybe ops
									case mbop of
												(Just (op, _)) -> parser >>= (\right -> returnASTLoc (AST.BinaryOp (extractDataFromToken op) left right) left right)
												Nothing -> return left
							in
								parser

m_expr :: PyParser
m_expr = makeBinaryOpParser u_expr $ choice [operator "*", delimiter "//", operator "/", operator "%"]

a_expr :: PyParser
a_expr = makeBinaryOpParser m_expr $ (operator "+" <|> operator "-") 

comparison :: PyParser
comparison = do
	left <- a_expr
	mbop <- optionMaybe comp_operator
	case mbop of
				(Just (op,_)) -> a_expr >>= (\right -> returnASTLoc (AST.BinaryOp (extractDataFromToken op) left right) left right)
				Nothing -> return left 
		

comp_operator :: GenParser TokenPos () TokenPos
comp_operator = choice $ map operator $ ["<", ">", "==", ">=", "<=", "!="]

or_test :: PyParser
or_test = makeBinaryOpParser and_test $ keyword "or"

and_test :: PyParser
and_test = makeBinaryOpParser not_test $ keyword "and"

not_test :: PyParser
not_test = not_test' <|> comparison
			where
				not_test' = do
								keyword "not"
								arg@(_, loc) <- not_test'
								return $ (AST.UnaryOp "not" arg, loc)
	

expr :: PyParser
expr = or_test

stmt :: PyParser
stmt = (simple_stmt >>= \ast -> newLine >> return ast) <|> compound_stmt

expr_stmt :: PyParser
expr_stmt = expr

simple_stmt :: PyParser
simple_stmt =  try assignment_stmt <|> return_stmt <|> break_stmt <|> continue_stmt <|> expr_stmt

break_stmt :: PyParser
break_stmt = do
	kw <- keyword "break"
	return (AST.Break, snd kw)

continue_stmt :: PyParser
continue_stmt = do
	kw <- keyword "continue"
	return (AST.Continue, snd kw)

return_stmt :: PyParser
return_stmt = do
	keyword "return"
	ret <- expr
	return $ (AST.Return ret, snd ret)

assignment_stmt :: PyParser
assignment_stmt = do
	id <- target
	delimiter "="
	rvalue <- expr
	returnASTLoc (AST.Assignment id rvalue) id rvalue

target :: PyParser
target = choice [try subscription, identifier]

compound_stmt :: PyParser
compound_stmt = if_stmt <|> while_stmt <|> for_stmt <|> funcdef
	
if_stmt :: PyParser
if_stmt = do
	keyword "if"
	if_clause <- cond_pair
	pairs <- many (keyword "elif" >> cond_pair)
	else_clause <- option Nothing (keyword "else" >> delimiter ":" >> suite >>= (return . Just))
	return (AST.IfThenElse (if_clause : pairs) else_clause, foldl AST.mergeLocs (snd $ snd if_clause) (map (snd . snd) pairs))
		where
			cond_pair = do
				cond <- expr
				delimiter ":"
				code <- suite
				return $ (cond, code)
	

while_stmt :: PyParser
while_stmt = do
	keyword "while"
	cond <- expr
	delimiter ":"
	code <- suite
	returnASTLoc (AST.While cond code) cond code

for_stmt :: PyParser
for_stmt = do
	keyword "for"
	id@(AST.ID var, _) <- identifier
	keyword "in"
	collection <- expr
	delimiter ":"
	body <- suite
	returnASTLoc (AST.For var collection body) id body

-- TODO: Check associativity rules for >> and <|> in Haskell Report
-- TODO: Check out *> in Applicative [pure = return, <*> = ap?]
suite :: PyParser 
suite = (stmt >>= \s -> newLine >> return s) <|> do
	newLine
	indent
	ss <- many1 stmt
	dedent
	returnASTLoc (AST.Sequence ss) (head ss) (last ss)

file_input :: PyParser
file_input = do
	ss <- many1 stmt
	eof
	return (AST.Sequence ss, snd (last ss))

funcdef :: PyParser
funcdef = do
	def <- keyword "def"
	(AST.ID name, _) <- identifier
	delimiter "("
	args <- sepBy identifier $ delimiter ","
	delimiter ")"
	delimiter ":"
	code <- suite
	returnASTLoc (AST.FunctionDefinition name [id | (AST.ID id, _) <- args] code) def code

