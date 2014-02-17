module BPython.PyType where

data PythonType = PyInt | PyFloat | PyString | PyBool | PyVoid | PyFun [PythonType] PythonType |
				 	PyAny | PyAnyOf [PythonType] | PyAnyFunction | PyList PythonType | PyVar String |
					PyForAll String PythonType | PyForAnyOf String [PythonType] PythonType | PyRange

instance Eq PythonType where
	PyInt == PyInt = True
	PyFloat == PyFloat = True
	PyString == PyString = True
	PyBool == PyBool = True
	PyVoid == PyVoid = True
	(PyFun args ret) == (PyFun args' ret') = args == args' && ret == ret'
	(PyList t1) == (PyList t2) = t1 == t2
	PyAny == _ = True
	_ == PyAny = True
	t == PyAnyOf ts = t `elem` ts
	PyAnyOf ts == t = t `elem` ts 
	_ == _ = False

instance Show PythonType where
	show PyInt = "Int"
	show PyFloat = "Float"
	show PyString = "String"
	show PyBool = "Bool"
	show PyVoid = "Void"
	show (PyFun arg_ts ret_t) = show arg_ts ++ "->" ++ show ret_t
	show PyAny = "Any"
	show (PyAnyOf args) = "AnyOf " ++ show args
	show (PyList t) = "ListOf " ++ show t
	show (PyVar x) = x
	show (PyForAll x t) = "forall " ++ x ++ ": " ++ show t
	show (PyForAnyOf x variants t) = "forany " ++ x ++ " of " ++ show variants ++ ": " ++ show t
	show PyRange = "Range"

primitiveTypes :: [PythonType]
primitiveTypes = [PyInt, PyFloat, PyString, PyBool]
