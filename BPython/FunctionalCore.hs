module BPython.FunctionalCore where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State.Lazy
import BPython.Lexer
import BPython.ProfileCommon
import BPython.PyType
import Data.List (nub)
import Data.Char (chr, ord)
import qualified BPython.AST as AST
import qualified Data.Map as M

import Debug.Trace

type Substitution = M.Map String PythonType
type TypeContext = M.Map String PythonType

data CheckState = CS {
	typeMap :: TypeContext,
	context :: TypeContext,
	nameGenState :: (Char, Int)
} deriving Show;

type CheckMonad = ErrorT CheckError (State CheckState)

newVar :: CheckMonad String
newVar = do
	(CS tm ctx ngs@(c, n)) <- lift get
	lift $ put (CS tm ctx (nextVar ngs))
	return $ c : show n 

freeVars :: PythonType -> [String]
freeVars = nub . freeVars'
	where
		freeVars' (PyVar x) = [x]
		freeVars' (PyFun args ret) = concatMap freeVars' args ++ freeVars' ret
		freeVars' (PyForAll x t) = filter (/= x) (freeVars' t)
		freeVars' (PyForAnyOf x _ t) = filter (/= x) (freeVars' t)
		freeVars' t = []

substitute :: Substitution -> PythonType -> CheckMonad PythonType
substitute sub v@(PyVar x) = return $ case M.lookup x sub of
	(Just t) -> t
	Nothing -> v
substitute sub (PyFun arg_ts ret_t) = do
	arg_ts' <- mapM (substitute sub) arg_ts
	ret_t' <- substitute sub ret_t
	return (PyFun arg_ts' ret_t') 
substitute sub (PyForAll bound scope)
	| M.member bound sub = do
		bound' <- newVar
		renamed_scope <- substitute (M.singleton bound (PyVar bound')) scope
		scope' <- substitute sub renamed_scope
		return $ PyForAll bound' scope'
	| otherwise = do
		scope' <- substitute sub scope
		return $ PyForAll bound scope
substitute sub (PyForAnyOf bound ts scope) = do
	(PyForAll bound' scope') <- substitute sub (PyForAll bound scope) -- XXX: Hackish
	return $ PyForAnyOf bound' ts scope'
substitute _ t = return t

substituteContext :: Substitution -> TypeContext -> CheckMonad TypeContext
substituteContext s = liftM M.fromList . mapM applyCM . M.toList
	where
		applyCM (v, t) = do
			t' <- substitute s t
			return (v, t')

compose :: Substitution -> Substitution -> CheckMonad Substitution
s1 `compose` s2 = do
	s <- substituteContext s1 s2
	return $ M.union s s1

nextVar :: (Char, Int) -> (Char, Int)
nextVar (c, n)
	| c == 'z' = ('a', n+1)
	| otherwise = (chr (ord (c) + 1), n)

opFuncs :: M.Map String PythonType
opFuncs = M.fromList $ [
	("+", PyForAnyOf "x" [PyInt, PyFloat, PyString] $ PyFun [PyVar "x", PyVar "x"] (PyVar "x")),
	("-", PyForAnyOf "x" [PyInt, PyFloat] $ PyFun [PyVar "x", PyVar "x"] (PyVar "x")),
	("*", PyForAnyOf "x" [PyInt, PyFloat] $ PyFun [PyVar "x", PyVar "x"] (PyVar "x")),
	("/", PyForAnyOf "x" [PyInt, PyFloat] $ PyForAnyOf "y" [PyInt, PyFloat] $ PyFun [PyVar "x", PyVar "y"] PyFloat),
	("//", PyFun [PyInt, PyInt] PyInt),
	("%", PyFun [PyInt, PyInt] PyInt),
	("and", PyFun [PyBool, PyBool] PyBool),
	("or", PyFun [PyBool, PyBool] PyBool),
	("not", PyFun [PyBool] PyBool),
	("==", PyForAll "x" $ PyFun [PyVar "x", PyVar "x"] PyBool),
	("!=", PyForAll "x" $ PyFun [PyVar "x", PyVar "x"] PyBool)] ++
	[(op, PyForAnyOf "x" [PyInt, PyFloat] (PyFun [PyVar "x", PyVar "x"] PyBool)) | op <- ["<", ">", "<=", ">="]]

getOpFunc :: String -> CheckMonad PythonType
getOpFunc op = let f = (M.!) opFuncs op in instantiate f

-- TODO: Maybe just operate on state, without those context returns? We already use CheckMonad for typevar generation
-- TODO: Maybe we need only assignments?
-- IDEA : populate context initially, setting a typevar to each ID
insertIDsIntoContext :: TypeContext -> AST.ASTLoc -> CheckMonad TypeContext
insertIDsIntoContext ctx (AST.ID id, _)
	| id `M.notMember` ctx = do
		varName <- newVar
		return $ M.insert id (PyVar varName) ctx
	| otherwise = return ctx -- TODO: Can we detect all multiple assignments this way?
insertIDsIntoContext ctx (AST.Literal t, _) = return ctx
insertIDsIntoContext ctx (AST.BinaryOp _ l r, _) = do
	lctx <- insertIDsIntoContext ctx l
	insertIDsIntoContext lctx r
insertIDsIntoContext ctx (AST.UnaryOp _ arg, _) = insertIDsIntoContext ctx arg
insertIDsIntoContext ctx (AST.FunctionCall fun args, _) = do
	ctx' <- insertIDsIntoContext ctx fun
	foldM insertIDsIntoContext ctx' args
insertIDsIntoContext ctx (AST.Assignment l r, _) = do 
	lctx <- insertIDsIntoContext ctx l 
	insertIDsIntoContext lctx r
insertIDsIntoContext ctx (AST.IfThenElse pairs elseclause, _) = do
	pairs_ctx <- insertFromPairs ctx pairs
	case elseclause of
		(Just stmt) -> insertIDsIntoContext pairs_ctx stmt
		Nothing -> return pairs_ctx
	where
		insertFromPairs ctx [] = return ctx
		insertFromPairs ctx ((cond, stmt) : pairs) = do
			cond_ctx <- insertIDsIntoContext ctx cond
			stmt_ctx <- insertIDsIntoContext ctx stmt
			insertFromPairs stmt_ctx pairs
insertIDsIntoContext ctx (AST.Sequence stmts, _) = foldM insertIDsIntoContext ctx stmts
insertIDsIntoContext ctx (AST.FunctionDefinition func args body, loc) = do 
	name_ctx <- insertIDsIntoContext (AST.ID func, loc) -- XXX: is it right?
	insertID
insertIDsIntoContext ctx (AST.Return ast, _) = insertIDsIntoContext ctx ast

instantiate :: PythonType -> CheckMonad PythonType
instantiate (PyForAll var t) = do
	var' <- newVar
	t' <- substitute (M.singleton var (PyVar var')) t
	tinst <- instantiate t'
	return tinst
instantiate (PyForAnyOf var _ t) = instantiate (PyForAll var t)
instantiate t = return t

mergeSubstitutions :: Substitution -> Substitution -> CheckMonad Substitution
mergeSubstitutions s1 s2 =	let
								s1ands2 = M.toList $ M.intersectionWith (\x y -> (x,y)) s1 s2
								unify_int_pairs :: [(String, (PythonType, PythonType))] -> CheckMonad [(String, PythonType)]
								unify_int_pairs [] = return []
								unify_int_pairs ((k, (x,y)) : ps) = do
									mgu <- unify x y
									ps_map <- unify_int_pairs ps
									t <- substitute mgu x
									return $ (k, t) :  ps_map
							in do
								unified_int <- (unify_int_pairs s1ands2)
								return $ M.union (M.fromList unified_int) (M.union s1 s2)


unify :: PythonType -> PythonType -> CheckMonad Substitution
unify (PyVar x) (PyVar y)
	| x == y = return M.empty
	| otherwise = return $ M.singleton y (PyVar x)
unify (PyVar x) f@(PyFun _ _)
	| x `elem` freeVars f = error "Occurs check failed"
	| otherwise = return $ M.singleton x f
unify (PyVar x) t = return $ M.singleton x t
unify t v@(PyVar _) = unify v t
unify f1@(PyFun xs y) f2@(PyFun xs' y') = do
	args <- unifyLists xs xs'
	ret <- unify y y'
	mergeSubstitutions args ret
unify x y
	| x == y	= return M.empty
	| otherwise	= error ("Failed to unify : " ++ show x ++ " and " ++ show y) -- XXX: Throw exception

unifyLists xs ys = foldM nonUnionUnify M.empty (zip xs ys)
	where
		nonUnionUnify sub (x, x') = do
			mgu <- unify x x'
			mergeSubstitutions mgu sub

inferTypes :: TypeContext -> AST.ASTLoc -> CheckMonad (Substitution, PythonType)
inferTypes ctx (AST.ID id, loc) = 
	case M.lookup id ctx of
		(Just t) -> do
						t' <- instantiate t
						return (M.empty, t')
		Nothing -> throwLocError loc $ IDNotFound id
inferTypes ctx (AST.Literal t, _) = return (M.empty, t)
inferTypes ctx (AST.UnaryOp op arg, loc) = do
	func <- getOpFunc op
	inferTypes ctx (AST.FunctionCall (AST.Literal func, loc) [arg], loc)
inferTypes ctx (AST.BinaryOp op larg rarg, loc) = do 
	func <- getOpFunc op
	inferTypes ctx (AST.FunctionCall (AST.Literal func, loc) [larg, rarg], loc)
inferTypes ctx (AST.FunctionCall fun args, _) = do
	(sf, tf) <- inferTypes ctx fun
	sfctx <- trace ("Applying function " ++ show tf) $ substituteContext sf ctx
	(sub, arg_ts) <- inferArgumentTypes (sfctx, []) args

	ret_var <- newVar
	subtf <- substitute sub tf
	v <- trace ("Argument types : " ++ show arg_ts ++ "; subtf = " ++ show subtf) $ unify subtf (PyFun arg_ts (PyVar ret_var))

	sub_sf <- trace ("v = " ++ show v) $ sub `compose `sf
	final_sub <- v `compose` sub_sf

	ret_var' <- substitute v (PyVar ret_var)

	return (final_sub, ret_var')
	where
		inferArgumentTypes (sub, ts) [] = return (sub, reverse ts)
		inferArgumentTypes (sub, ts) (arg : args) = do
			(arg_sub, arg_t) <- inferTypes sub arg
			arg_sub_sub <- arg_sub `compose` sub
			inferArgumentTypes (arg_sub_sub, (arg_t : ts)) args
inferTypes ctx (AST.Assignment (AST.ID id, idloc) rvalue, loc) = do		-- XXX: What to do with assignment????
	case M.lookup id ctx of
		(Just t) -> do
			(rvalue_sub, rvalue_t) <- inferTypes ctx rvalue
			t' <- substitute rvalue_sub t
			mgu <- unify t' rvalue_t  -- XXX: Detect double assignments!!!!
			t'' <- substitute mgu t'
			mgu_rvalue_sub <- mgu `compose` rvalue_sub
			return (mgu_rvalue_sub, t'')
		Nothing -> error $ "inferTypes : assignment to var " ++ id ++ " not in context " ++ show ctx ++ " - initial context population failed" 
inferTypes ctx (AST.IfThenElse pairs elseclause, loc) = do
	sub <- checkPairTypes ctx pairs
	subctx <- substituteContext sub ctx
	case elseclause of
		(Just ast) -> do
			(s, _) <- inferTypes subctx ast
			s_sub <- s `compose` sub
			return (s, PyVoid)
		Nothing -> return (sub, PyVoid)
	where
		checkPairTypes _ [] = return M.empty
		checkPairTypes ctx ((cond, stmt) : pairs) = do
			(cond_sub, cond_t) <- inferTypes ctx cond 
			mgu <- unify cond_t PyBool -- TODO: catch exeption and throw the one about invalid condition type
			mgu_cond_sub <- mgu `compose` cond_sub
			subctx <- substituteContext mgu_cond_sub ctx
			rec_sub <- checkPairTypes subctx pairs
			rec_sub `compose` mgu_cond_sub
inferTypes ctx (AST.FunctionDefinition name args body, loc) = do -- TODO: When to generalize?
	arg_vars <- replicateM (length args) newVar
	let ctx_with_args = foldl (\ctx (arg, t) -> M.insert arg (PyVar t) ctx) ctx (zip args arg_vars)
	(s, t) <- inferTypes ctx_with_args body
	-- TODO: Calculate return type
	let ret_type = PyAny
	arg_types <- mapM (substitute s . PyVar) arg_vars
	sctx <- substituteContext  s ctx
	case M.lookup name sctx of
		(Just t) -> do
			mgu <- unify t (PyFun arg_types ret_type)
			mgu_s <- mgu `compose` s
			return (mgu_s, PyVoid)
		Nothing -> error "inferTypes : func definition type var not in context"
inferTypes ctx (AST.Return arg, loc) = inferTypes ctx arg -- XXX: Temporary
--inferTypes ctx (AST.Sequence stmts, loc) = foldM (\ctx ast -> liftM fst (inferTypes ctx ast)) ctx stmts >>= (\s -> return (s, PyVoid))
inferTypes ctx (AST.Sequence stmts, loc) = inferSequenceTypes ctx stmts >>= (\s -> return (s, PyVoid))
	where 
		inferSequenceTypes ctx [] = return M.empty
		inferSequenceTypes ctx (s:ss) = do
			(ssub, st) <- inferTypes ctx s
			ssub_ctx <- substituteContext ssub ctx
			sssub <- inferSequenceTypes ssub_ctx ss
			final <- sssub `compose` ssub
			return final
	

runCheckMonad x tm = runState (runErrorT x) (CS tm M.empty ('a', 1))

infer :: AST.ASTLoc -> CheckMonad (TypeContext, PythonType)
infer ast = do
	ctx <- insertIDsIntoContext M.empty ast
	(sub, t) <- trace ("Initial context : " ++ show ctx) $ inferTypes ctx ast
	final <- substituteContext sub ctx
	return (final, t)

checkTypes :: TypeMap -> AST.ASTLoc -> Either CheckError ()
checkTypes tm ast = case runCheckMonad (infer ast) tm of
	(Left err, _) -> Left err
	(Right _, _) -> Right ()

debugCheck ast = case runCheckMonad (infer ast) of
	(Left err, s) -> do
		putStrLn ("Error : " ++ show err)
	(Right (tm, t), s) -> do
		putStrLn ("Type : " ++ show t)
		putStrLn ("TM : " ++ show tm)
