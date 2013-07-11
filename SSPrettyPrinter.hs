module SSPrettyPrinter(show) where
import LispVal
import Data.Map as Map
-----------------------------------------------------------
--                    PRETTY PRINTER                     --
-----------------------------------------------------------

-- Pretty-printing for LispVal values. 
instance Show LispVal where 
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Number num) = show num
  show (String str) = "\"" ++ str ++ "\""
  show (Atom name) = name
  show (List l) = "(" ++ showListContents l ++ ")"
  show (DottedList h t) = "(" ++ showListContents h ++ " . " ++ show t ++ ")"
  show (Struct pMap) = "{" ++ showParametersList (toList pMap) ++ "}"
  show (Lambda bindings body) = "lambda " ++ show (List bindings) ++ " " ++ show (List body) 
  show (Native p) = "<native procedure>"
  show (Error s) = "ERROR: " ++ s
  
-- This function could be replaced by (unwords.map show). The unwords
-- function takes a list of String values and uses them to build a 
-- single String where the words are separated by spaces.
showListContents :: [LispVal] -> String
showListContents [] = ""
showListContents [a] = show a
showListContents (a:as) = show a ++ " " ++ (showListContents as)

showParametersList ::[(String, LispVal)] -> String
showParametersList [] = ""
showParametersList ((name, val):[]) = name ++ ": " ++ show val
showParametersList ((name, val):as) = name ++ ": " ++ show val ++ ", " ++ showParametersList as