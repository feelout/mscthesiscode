module BPython.Profile where

import BPython.ProfileCommon
import BPython.AST

data Profile = Profile {
	forbiddenExtractor :: AST -> Bool,
	typechecker :: TypeMap -> ASTLoc -> Either CheckError (),
	typemap :: TypeMap
}
