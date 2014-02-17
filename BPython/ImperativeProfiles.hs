module BPython.ImperativeProfiles where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Lazy
import BPython.Lexer
import BPython.ProfileCommon
import BPython.PyType
import Data.Maybe (isJust, fromMaybe)
import Data.List (nub, intersect)
import qualified BPython.AST as AST
import qualified Data.Map as M

isForbiddenInExtended :: AST.AST -> Bool
isForbiddenInExtended (AST.Return _) = True
isForbiddenInExtended (AST.FunctionDefinition _ _ _) = True
isForbiddenInExtended (AST.For _ _ _) = True
isForbiddenInExtended _ = False 

isForbiddenInCore :: AST.AST -> Bool
isForbiddenInCore (AST.Return _) = True
isForbiddenInCore (AST.FunctionDefinition _ _ _) = True
isForbiddenInCore (AST.List _) = True
isForbiddenInCore (AST.Index _ _) = True
isForbiddenInCore (AST.Slice _ _ _) = True
isForbiddenInCore AST.Break = True
isForbiddenInCore AST.Continue = True
isForbiddenInCore (AST.For _ _ _) = True
isForbiddenInCore _ = False

isForbiddenInAdvanced :: AST.AST -> Bool
isForbiddenInAdvanced (AST.FunctionDefinition _ _ _) = True
isForbiddenInAdvanced (AST.Return _) = True
isForbiddenInAdvanced _ = False

-----------------
-- Unification --
-----------------

possibleTypes :: PythonType -> [PythonType]
possibleTypes PyAny = [PyInt, PyFloat, PyString, PyBool, PyVoid]
possibleTypes (PyAnyOf ts) = ts
possibleTypes t = [t]

makeType :: [PythonType] -> PythonType
makeType [t] = t
makeType ts@(_:_) = PyAnyOf (nub ts)

unifyAll :: [PythonType] -> Maybe PythonType
unifyAll = foldM unify PyAny

--- TODO: matchFunc probly should be a part of unify

--- XXX: Probably will not work for higher-level functions (just needs to rename bound variables)
--- TODO: Make sure that return type is processed properly
matchFunc :: PythonType -> [PythonType] -> Maybe PythonType
matchFunc f args = do
	ctx' <- matchArgs ctx args arg_ts 
	return $ PyFun (map (subst ctx') arg_ts) (subst ctx' ret_t)
	where
		populateContext ctx (PyForAll x t) = populateContext (M.insert x PyAny ctx) t
		populateContext ctx (PyForAnyOf x ts t) = populateContext (M.insert x (PyAnyOf ts) ctx) t -- typevars here do not work
		populateContext ctx f@(PyFun _ _) = (ctx,  f)

		(ctx, (PyFun arg_ts ret_t)) = populateContext M.empty f

		matchArgs ctx [] [] = Just ctx
		matchArgs _ (_:_) [] = Nothing
		matchArgs _ [] (_:_) = Nothing
		matchArgs ctx (a:as) (fa:fas) = do
			(t, ctx') <- match ctx a fa
			matchArgs ctx' as fas

		subst ctx (PyVar x) = (M.!) ctx x
		subst ctx (PyList t) = PyList $ subst ctx t
		subst ctx (PyFun args ret) = PyFun (map (subst ctx) args) (subst ctx ret)
		subst ctx t = t

match :: Context -> PythonType -> PythonType -> Maybe (PythonType, Context)
match ctx (PyVar _) (PyVar _) = Nothing
match ctx t (PyVar x) = case (M.lookup x ctx) of
	(Just t') -> do
					(t'', ctx'') <- match ctx t' t
					return (t'', M.insert x t'' ctx'')
	Nothing -> Just (t, M.insert x t ctx)
match ctx (PyVar x) t = match ctx t (PyVar x)
match ctx (PyList t1) (PyList t2) = do
	(t, ctx') <- match ctx t1 t2
	return (PyList t, ctx')
match ctx (PyFun args1 ret1) (PyFun args2 ret2) = do
	(args, ctx') <- matchArgs ctx [] args1 args2
	match ctx' ret1 ret2
	where
		matchArgs ctx ts [] [] = Just (reverse ts, ctx)
		matchArgs ctx ts (x:xs) (y:ys) = do
			(t, ctx') <- match ctx x y
			matchArgs ctx' (t : ts) xs ys
		matchArgs _ _ _ _ = Nothing
match ctx PyAny t = return (t, ctx)
match ctx (PyAnyOf ts) t =	let 
								matchings = [pr | t' <- ts, let m = match ctx t' t, isJust m, let (Just pr) = m]
							in
								if null matchings then Nothing else Just $ head matchings
match ctx t1 t2
	| t1 == t2 = Just (t1, ctx)
	| otherwise = Nothing

unify :: PythonType -> PythonType -> Maybe PythonType
unify t1 t2 = fmap fst $ match M.empty t1 t2

type Context = M.Map String PythonType

--------------
-- Checkers --
--------------
--
-- XXX: Add (+) for lists
binOpTypes :: M.Map String [(PythonType, PythonType)]
binOpTypes = assocListToMap $ genericNumEntries ++ intEntries ++ boolEntries ++ [("+", [(PyString, PyString), (PyList PyAny, PyList PyAny)])]
	where
		numTs = [PyInt, PyFloat]
		genericNumOps = ["+", "-", "*", "/", "<", ">", "==", "!=", ">=", "<=", "**"]
		genericNumEntries = [(op, [(t1, t2) | t1 <- numTs, t2 <- numTs]) | op <- genericNumOps]
		intOps = ["//", "%"]
		intEntries = [(op, [(PyInt, PyInt)]) | op <- intOps]
		boolOps = ["and", "or", "==", "!="]
		boolEntries = [(op, [(PyBool, PyBool)]) | op <- boolOps]

-- ! Must be already typechecked
binOpResultType :: String -> (PythonType, PythonType) -> PythonType
binOpResultType op (t1, t2)
	| t1 == PyBool 	= PyBool
	| op `elem` ["<", "<=", "==", "!=", "=>", ">"] = PyBool
	| t1 == PyFloat || t2 == PyFloat = PyFloat
	| op == "/"			= PyFloat
	| otherwise			= PyInt

type CheckMonad = ErrorT CheckError (State TypeMap)

typecheckBinaryOp :: String -> (PythonType, PythonType) -> CheckMonad PythonType
typecheckBinaryOp op ts@(t1, t2) = 
	case (M.lookup op binOpTypes) of
	 	(Just allTs) -> if ts `elem` allTs
								then return $ binOpResultType op ts
								else throwUnlocError $ OpTypeMismatch op [t1, t2]
	 	Nothing -> throwUnlocError $ OperatorNotFound op

typecheck :: AST.ASTLoc -> CheckMonad PythonType
typecheck (AST.ID s, loc) = lift get >>= (\tm -> case M.lookup s tm of
						(Just t) -> return t
						Nothing -> throwLocError loc $ IDNotFound s)

typecheck (AST.Literal t, _) = return t

typecheck (AST.UnaryOp op arg, loc) = do
	arg_t <- typecheck arg
	if (arg_t == PyBool) && (op == "not") || (arg_t `elem` [PyInt, PyFloat])
		then return $ arg_t
		else throwLocError loc $ OpTypeMismatch op [arg_t]

typecheck (AST.BinaryOp op left right, loc) =  do
	left_t <- typecheck left
	right_t <- typecheck right
	typecheckBinaryOp op (left_t, right_t) -- XXX: Pass the location, and import the new binary op system

typecheck (AST.FunctionCall name@(AST.ID nm, fun_loc) args, loc) = do
	arg_ts <- mapM typecheck args
	name_t <- typecheck name
	if isFunction name_t
		then case matchFunc name_t arg_ts of
				(Just (PyFun _ t)) -> return t
				Nothing -> throwLocError loc $ FuncTypeMismatch nm name_t arg_ts
		else throwLocError loc $ NotAFunction nm
	where
		isFunction (PyFun _ _) = True
		isFunction (PyForAll _ t) = isFunction t
		isFunction (PyForAnyOf _ _ t) = isFunction t
		isFunction _ = False

typecheck (AST.Assignment name@(AST.ID nm, _) rvalue, loc) = do
	rvalue_t <- typecheck rvalue
	tm <- lift get
	case (M.lookup nm tm) of
		(Just name_t) -> if name_t == rvalue_t
							then return PyVoid
							else throwLocError loc (AssignmentTypeMismatch name_t rvalue_t)
		Nothing -> do
						lift $ modify (M.insert nm rvalue_t)
						return PyVoid

typecheck (AST.Assignment lvalue rvalue, loc) = do
	lvalue_t <- typecheck lvalue
	rvalue_t <- typecheck rvalue
	if lvalue_t == rvalue_t
		then return PyVoid
		else throwLocError loc (AssignmentTypeMismatch lvalue_t rvalue_t)

typecheck (AST.IfThenElse cond_pairs else_clause, loc) = do
			mapM checkCondPair cond_pairs
			case else_clause of
				(Just clause) -> typecheck clause
				Nothing -> return PyVoid
			return PyVoid
	where
		checkCondPair (cond, clause) = do
			cond_t <- typecheck cond
			typecheck clause
			if cond_t == PyBool
				then return PyVoid
				else throwLocError (snd cond) $ IfNonBooleanCondition cond_t

typecheck (AST.While cond clause, _) = do
	cond_t <- typecheck cond
	typecheck clause
	if cond_t == PyBool
		then return PyVoid
		else throwLocError (snd cond) $ WhileNonBooleanCondition cond_t

typecheck (AST.List members, loc) = do
	member_ts <- mapM typecheck members
	case unifyAll member_ts of
		(Just t) -> return (PyList t)
		Nothing -> throwLocError loc $ NonHomogeneousList member_ts

typecheck (AST.Index list index, loc) = do
	list_t <- typecheck list
	index_t <- typecheck index
	when (index_t /= PyInt) (throwLocError loc $ InvalidIndexValue index_t)
	case list_t of
		(PyList t) -> return t
		t -> throwLocError loc $ InvalidIndexTarget t

typecheck (AST.Slice list start end, loc) = do -- TODO: DRY
	list_t <- typecheck list
	start_t <- typecheck start
	when (start_t /= PyInt) (throwLocError loc $ InvalidIndexValue start_t)
	end_t <- typecheck end
	when (end_t /= PyInt) (throwLocError loc $ InvalidIndexValue end_t)
	case list_t of
		(PyList t) -> return (PyList t)
		t -> throwLocError loc $ InvalidIndexTarget t

typecheck (AST.Break, _) = return PyVoid

typecheck (AST.Continue, _) = return PyVoid

typecheck (AST.For var collection body, loc) = do
	col_t <- typecheck collection
	element_t <- case col_t of
		(PyList t) -> return t
		PyRange -> return PyInt
		t -> throwLocError loc $ ForNotACollection col_t

	tm <- lift get
	let var_t = fromMaybe element_t (M.lookup var tm)
	if var_t == element_t
		then do
			lift $ modify $ M.insert var var_t
			typecheck body
		else throwLocError loc $ ForInvalidIndexType var_t element_t

typecheck (AST.Sequence asts, _) = mapM_ typecheck asts >> return PyVoid 

typecheck (AST.Void, _) = return $ PyVoid -- TODO: Find out if Void is actually needed

checkTypes :: TypeMap -> AST.ASTLoc -> Either CheckError ()
checkTypes tm ast = case fst $ runState (runErrorT $ typecheck ast) tm of
					(Right _) -> Right ()
					(Left e) -> (Left e)
