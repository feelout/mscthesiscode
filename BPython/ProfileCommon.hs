module BPython.ProfileCommon where

import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (newline, State)
import Text.Parsec.Pos
import BPython.Lexer
import Data.List (intersperse)
import BPython.PyType
import qualified BPython.AST as AST
import qualified Data.Map as M

type PyParser = GenParser TokenPos () AST.ASTLoc
type TypeMap = M.Map String PythonType

assocListToMap :: Ord k => [(k, [v])] -> M.Map k [v]
assocListToMap = foldl (flip $ uncurry (M.insertWith (++))) M.empty

data CheckErrorData = 
	OpTypeMismatch String [PythonType] |
	OperatorNotFound String |
	IDNotFound String |
	FuncTypeMismatch String PythonType [PythonType] |
	NotAFunction String |
	AssignmentTypeMismatch PythonType PythonType |
	IfNonBooleanCondition PythonType |
	WhileNonBooleanCondition PythonType |
	AssignmentToBinding String |
	NoReturnInFunction String |
	DifferentReturnTypes String | 
	NoReturnInBranch |
	UnificationFailure PythonType PythonType | 
	InvalidOpReturnTypeExpected String PythonType | 
	InvalidFunctionCallType String PythonType PythonType | 
	ForbiddenConstructs [AST.ASTLoc] |
	NonHomogeneousList [PythonType] | 
	ForNotACollection PythonType |
	ForInvalidIndexType PythonType PythonType | 
	InvalidIndexValue PythonType |
	InvalidIndexTarget PythonType |
	GenericError String 

data CheckError = LocatedError CheckErrorData (Int, Int) | UnlocatedError CheckErrorData

instance Error CheckError where
	strMsg = UnlocatedError . GenericError

instance Show CheckError where
	show (LocatedError err (l1,l2)) = show l1 ++ "-" ++ show l2 ++ " : " ++ show err
	show (UnlocatedError err) = show err

instance Show CheckErrorData where
	show (OpTypeMismatch op ts) = "Invalid argument types for <" ++ op ++ "> : " ++ show ts 
	show (OperatorNotFound op) = "Operator <" ++ op ++ "> not found"
	show (IDNotFound id) = "Identifier <" ++ id ++ "> not found"
	show (FuncTypeMismatch nm exp actual) = "Invalid argument types for function <" ++ nm ++ 
		"> : Expected " ++ show exp ++ "; Actual " ++ show actual
	show (NotAFunction id) = "Identifier <" ++ id ++ "> is not a function"
	show (AssignmentTypeMismatch exp actual) = "Can't assign value of type " ++ show actual ++ 
		" to a variable of type " ++ show exp
	show (IfNonBooleanCondition t) = "If-then-else condition must be boolean, not " ++ show t
	show (WhileNonBooleanCondition t) = "While cycle condition must be boolean, not " ++ show t
	show (AssignmentToBinding id) = "Binding <" ++ id ++ "> was already assigned to"
	show (NoReturnInFunction fn) = "Function <" ++ fn ++ "> is missing return clause"
	show (DifferentReturnTypes fn) = "Function <" ++ fn ++ "> has different return types"
	show (NoReturnInBranch) = "Function branch is missing return clause"
	show (UnificationFailure t1 t2) = "Can't unify " ++ show t1 ++ " and " ++ show t2
	show (InvalidFunctionCallType fn expected actual) = "Funcion " ++ fn ++ " return type is " ++
		show actual ++ ", not " ++ show expected
	show (NonHomogeneousList ts) = "List elements have different types : " ++ show ts
	show (InvalidIndexValue t) = "Index type must be PyInt, not " ++ show t
	show (InvalidIndexTarget t) = "Can not index type " ++ show t
	show (ForNotACollection t) = "for-loop over a type " ++ show t ++ ", which is not a collection (list or range)"
	show (ForInvalidIndexType actual needed) = "Invalid index type: " ++ show actual ++ " for a collection with element type " ++ show needed
	show (ForbiddenConstructs cons) = "Forbidden constructs : \n" ++ (concat $ intersperse "\n" $ map (("*" ++) . showASTLocHeader) cons)

showASTLocHeader :: AST.ASTLoc -> String
showASTLocHeader (ast, loc) = showConstruct ast ++ showPlace loc

showConstruct :: AST.AST -> String
showConstruct (AST.ID s) = "Variable " ++ s
showConstruct (AST.Literal t) = show t ++ "-literal"
showConstruct (AST.UnaryOp op _) = show op ++ " operator"
showConstruct (AST.BinaryOp op _ _) = show op ++ " operator"
showConstruct (AST.FunctionCall _ _) = "Function call"
showConstruct (AST.Assignment lhs _) = "Assignment to " ++ showConstruct (fst lhs)
showConstruct (AST.IfThenElse _ _) = "If statement"
showConstruct (AST.While _ _) = "While loop"
showConstruct (AST.Sequence _) = "Block"
showConstruct (AST.FunctionDefinition name _ _) = "Definition of function " ++ name
showConstruct (AST.Return _) = "Return statement"
showConstruct (AST.List _) = "List"
showConstruct (AST.Index _ _ ) = "List indexing"
showConstruct (AST.Slice _ _ _) = "List slicing"
showConstruct AST.Break = "Break statement"
showConstruct AST.Continue = "Continue statement"
showConstruct (AST.For _ _ _) = "For loop"

showPlace :: (Int, Int) -> String
showPlace (t, b)
	| t == b = " at line " ++ show t
	| otherwise = " at lines " ++ show t ++ "-" ++ show b

throwLocError loc err = throwError $ LocatedError err loc
throwUnlocError err = throwError $ UnlocatedError err

returnASTLoc :: AST.AST -> (a, (Int, Int)) -> (b, (Int, Int)) -> PyParser
returnASTLoc ast (_, loc1) (_, loc2) = return (ast, AST.mergeLocs loc1 loc2)

----------------
-- Primitives --
----------------
pureStringMethods :: [(String, PythonType)]
pureStringMethods = [("capitalize", PyFun [PyString] PyString),
	("count", PyFun [PyString, PyString, PyInt, PyInt] PyInt),
	("endswith", PyFun [PyString, PyString] PyBool),
	("find", PyFun [PyString, PyString, PyInt, PyInt] PyInt),
	("lower", PyFun [PyString] PyString),
	("upper", PyFun [PyString] PyString),
	("replace", PyFun [PyString, PyString, PyString] PyString),
	("strip", PyFun [PyString] PyString) ] ++ [(nm, PyFun [PyString] PyBool) | nm <- prednames]
	where
		prednames = ["isalnum", "isalpha", "isdecimal", "isdigit", "islower", "isnumeric",
			"isprintable", "isspace", "isupper"]

sequenceFunctions :: [(String, PythonType)]
sequenceFunctions = [("len", PyForAll "x" $ PyForAnyOf "y" [PyString, PyList (PyVar "x"), PyRange] (PyFun [PyVar "y"] PyInt)),
	("sum", PyForAnyOf "x" [PyInt, PyFloat] $ PyForAnyOf "y" [PyList (PyVar "x"), PyRange] $ PyFun [PyVar "y"] (PyVar "x")), 
	-- <list> should also take lists and strings, but then the return type changes and
	-- our type system does not allow such dependencies
	("list", PyFun [PyRange] (PyList PyInt)), 
	("range", PyFun [PyInt, PyInt, PyInt] PyRange)]

ioAndTypeFunctions = [("print", PyFun [PyAny] PyVoid), ("input", PyFun [] PyString),
			  ("int", PyFun [PyString] PyInt), ("float", PyFun [PyString] PyFloat),
			  ("str", PyFun [PyAny] PyString)]

additionalFunctions = [("append", PyForAll "x" $ PyFun [PyList (PyVar "x"), (PyVar "x")] PyVoid),
	("reverse", PyForAll "x" $ PyFun [PyList (PyVar "x")] PyVoid),
	("replace", PyFun [PyString, PyString, PyString] PyString),
	("split", PyFun [PyString] PyString)]

coreFunctions = ioAndTypeFunctions ++ pureStringMethods
advancedFunctions = coreFunctions ++ sequenceFunctions ++ additionalFunctions
