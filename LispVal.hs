module LispVal (LispVal(Atom, List, DottedList, Number, String, Bool, Error, Struct, Lambda, Native), isLambda, isAtom) where

import Data.Map as Map
-----------------------------------------------------------
--                    BASIC DATATYPES                    --
-----------------------------------------------------------
{- 
In Lisp, the data types representing code structures are the same as the
data types representing values. This somewhat simplifies the
construction of an interpreter. For other languages, one would use
different datatypes to represent structures that appear in the code
(statements, expressions, declarations, etc.) and the data that their
evaluation produces.
-}
data LispVal = Atom String
  | List [ LispVal ]
  | DottedList [ LispVal ] LispVal
  | Number Integer
  | String String 
  | Bool Bool
  | Struct (Map String LispVal)
  | Lambda [LispVal] [LispVal]
  | Native ([LispVal] -> LispVal)
  | Error String

isLambda :: LispVal -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False

isAtom :: LispVal -> Bool
isAtom (Atom _) = True
isAtom _ = False

instance Eq LispVal where
	(Atom a) == (Atom b) = a == b
	(Number a) == (Number b) = a == b
	(String a) == (String b) = a == b
	(Bool a) == (Bool b) = a == b
	(List a) == (List b) = a == b
	(DottedList a b) == (DottedList c d) = (a == c) && (b == d)
	(Struct a) == (Struct b) = a == b
	(Lambda a b) == (Lambda c d) = (a == c) && (b == d)
	a == b = False

