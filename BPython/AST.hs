module BPython.AST where

import BPython.PyType
import Data.Maybe (maybeToList)

-- FIXME : AnyFunction is a debug hack


data AST = ID String | Literal PythonType | UnaryOp String ASTLoc | BinaryOp String ASTLoc ASTLoc | 
			FunctionCall ASTLoc [ASTLoc] | Assignment ASTLoc ASTLoc | IfThenElse [(ASTLoc, ASTLoc)] (Maybe ASTLoc) | 
			While ASTLoc ASTLoc | Sequence [ASTLoc] | FunctionDefinition String [String] ASTLoc | Return ASTLoc |
			List [ASTLoc] | Index ASTLoc ASTLoc | Slice ASTLoc ASTLoc ASTLoc | Break | Continue | For String ASTLoc ASTLoc | Void
	deriving Show

type ASTLoc = (AST, (Int, Int))

mergeLocs :: (Int, Int) -> (Int, Int) -> (Int, Int)
mergeLocs (x, y) (x', y') = (x `min` x', y `max` y')

extractNodes :: (AST -> Bool) -> ASTLoc -> [ASTLoc]
extractNodes p al@(ast, loc)
	| p ast = al : extractNodes' ast
	| otherwise = extractNodes' ast
	where
		extractNodes' (UnaryOp _ arg) = extractNodes p arg
		extractNodes' (BinaryOp _ l r) = extractNodes p l ++ extractNodes p r
		extractNodes' (FunctionCall fun args) = extractNodes p fun ++ concatMap (extractNodes p) args
		extractNodes' (Assignment lvalue rvalue) = extractNodes p lvalue ++ extractNodes p rvalue
		extractNodes' (IfThenElse pairs elseclause) = concatMap (\(c, b) -> extractNodes p c ++ extractNodes p b) pairs ++ concat (maybeToList (fmap (extractNodes p) elseclause))
		extractNodes' (While cond body) = extractNodes p cond ++ extractNodes p body
		extractNodes' (Sequence stmts) = concatMap (extractNodes p) stmts
		extractNodes' (FunctionDefinition _ _ body) = extractNodes p body
		extractNodes' (Return arg) = extractNodes p arg
		extractNodes' (List args) = concatMap (extractNodes p) args
		extractNodes' (Index list index) = extractNodes p list ++ extractNodes p index
		extractNodes' (Slice list start end) = extractNodes p list ++ extractNodes p start ++ extractNodes p end
		extractNodes' _ = []
