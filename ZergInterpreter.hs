module Main where
import System.Environment
import Control.Monad
import Data.Map as Map
import LispVal
import SSParser
import SSPrettyPrinter

-----------------------------------------------------------
--                     SCHEME STATE                      --
-----------------------------------------------------------

newtype State stored comp = State {runState :: stored -> (comp, stored)}

instance Monad (State s) where
  return x = State (\state -> (x, state))

  (State stateF) >>= f = State $ 
    \s0 -> let
          (c1, s1) = stateF s0
          (State newStateF) = f c1
      in newStateF s1

type VariableState = Map String LispVal

-- SchemeState = (GlobalState, StackLocalState)
type SchemeState = (VariableState, [VariableState])

type StateTransformer payload = State SchemeState payload

unionList :: [VariableState] -> VariableState
unionList [] = empty
unionList (a:[]) = a
unionList (a:b:bs) = unionList ((union a b):bs)

getContextState :: SchemeState -> VariableState
getContextState (global, stack) = union (union (unionList stack) global) staticGlobalEnvironment 

initialState :: SchemeState
initialState = (empty, [])

-----------------------------------------------------------
--              STATE CHANGING FUNCTIONS                 --
-----------------------------------------------------------
stateLookup :: String -> StateTransformer LispVal
stateLookup var = State $
  (\s -> 
    (maybe (Error noSuchVariable) 
           id (Map.lookup var (getContextState s)
    ), s))

pushStack :: VariableState -> StateTransformer ()
pushStack ctx = State $
  (\(global, stack) -> ((), (global, (ctx:stack))))

popStack :: t -> StateTransformer t
popStack comp = State $
  (\(global, stack) -> (comp, (global, tail stack)))

instantiateLocalVar :: String -> LispVal -> VariableState -> StateTransformer VariableState
instantiateLocalVar id valor ctx = eval valor >>=
	(
		\val -> State $
		(
			\(global, stack) -> (insert id val ctx, (global, stack))
		)
	)

instantiateGlobalVar :: String -> LispVal -> StateTransformer LispVal
instantiateGlobalVar id valor = eval valor >>=
	(
		\val -> State $
		(
			\(global, stack) -> (val, ((insert id val global), stack))
		)
	)
-----------------------------------------------------------
--                      INTERPRETER                      --
-----------------------------------------------------------

onErrorApplyStandard :: [LispVal] -> ([LispVal] -> StateTransformer LispVal) -> LispVal -> (StateTransformer LispVal)
onErrorApplyStandard args standardTransformer (Error _) = standardTransformer args
onErrorApplyStandard args _ other = eval (List (other:args))

applyUserOrStandard :: String -> [LispVal] -> ([LispVal] -> StateTransformer LispVal) -> (StateTransformer LispVal)
applyUserOrStandard name args standardTransformer = stateLookup name >>= onErrorApplyStandard args standardTransformer

evalList :: [LispVal] -> StateTransformer LispVal
evalList [] = return (List [])
evalList [a] = eval a
evalList (a:as) = eval a >> evalList as

eval :: LispVal -> StateTransformer LispVal

eval val@(String _) = return val
eval val@(Atom var) = stateLookup var 
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Lambda _ _) = return val
eval val@(Struct _) = return val

eval (List [Atom "quote", val]) = return val
eval (List [Atom "begin", v]) = eval v
eval (List (Atom "begin": l: ls)) = eval l >> eval (List (Atom "begin": ls))
eval (List [Atom "begin"]) = return (List [])

eval lam@(List (Atom "lambda":(List formals):body:bs))
  | all isAtom formals = return (Lambda formals (body:bs))
  | otherwise = return (Error invalidArgumentList)

-- STANDARD FUNCTIONS
eval (List (List list : args)) = eval (List list) >>= (\val -> eval (List (val:args)))

eval (List (Atom "define": args)) = applyUserOrStandard "define" args schemeDefine
eval (List (Atom "let": args)) = applyUserOrStandard "let" args schemeLet
eval (List (Atom "set!": args)) = applyUserOrStandard "set" args schemeSet
eval (List (Atom "if": args)) = applyUserOrStandard "if" args schemeIf

-- USER DEFINED FUNCTION
eval (List ((Lambda formals body):args)) = mapM (eval) args >>= schemeLambda formals body
eval (List (Atom func : args)) = mapM (eval) args >>= schemeApply func 

eval val@(List _) = return val 

eval (Error s)  = return (Error s)
eval form = return (Error (evalError form))

-----------------------------------------------------------
--             STATE CHANGING FUNCTIONS                  --
-----------------------------------------------------------

-- BEGIN 'define'
schemeDefine :: [LispVal] -> StateTransformer LispVal
schemeDefine [(Atom id), val] = instantiateGlobalVar id val
--schemeDefine [(List [Atom id]), val] = instantiateGlobalVar id val                                     
schemeDefine _ = return (Error wrongArgumentNumber)
-- END 'define'

-- BEGIN 'let'
schemeInstantiateBindings :: [LispVal] -> VariableState -> StateTransformer VariableState
schemeInstantiateBindings [] ctx = return ctx
schemeInstantiateBindings ((List [(Atom id), val]):rs) ctx = instantiateLocalVar id val ctx >>= schemeInstantiateBindings rs
--schemeInstantiateBindings [(List [(List [Atom id]), val])] = instantiateLocalVar id val
--schemeInstantiateBindings ((List [(List [Atom id]), val]):rs) = instantiateLocalVar id val >> schemeInstantiateBindings rs

schemeLet :: [LispVal] -> StateTransformer LispVal
schemeLet ((List bindings):body) = schemeInstantiateBindings bindings empty
								   >>= pushStack
								   >> evalList body
								   >>= popStack

schemeLet _ = return (Error invalidArgumentList)
-- END 'let'

-- BEGIN 'set!'
schemeSet' :: String -> LispVal -> SchemeState -> (LispVal, SchemeState)
schemeSet' id val (global, [])
	| member id global = (val, (insert id val global, []))
	| member id staticGlobalEnvironment = (val, (insert id val global, []))
	| otherwise = (Error noSuchVariable, (global, []))

schemeSet' id val (global, ctx:stack)
	| member id ctx = (val, (global, ((insert id val ctx):stack)))
	| otherwise = (nVal, (nGlobal, ctx:nStack))
		where
			(nVal, (nGlobal, nStack)) = schemeSet' id val (global, stack)

schemeSet :: [LispVal] -> StateTransformer LispVal
schemeSet [(Atom id), val] = eval val >>= (\nVal -> State $ (schemeSet' id nVal))
schemeSet _ = return (Error wrongArgumentNumber)
-- END 'set!'

-- BEGIN 'if'
schemeIfCondition :: LispVal -> LispVal -> LispVal -> StateTransformer LispVal
schemeIfCondition consequent alternate (Bool True) = eval consequent
schemeIfCondition consequent alternate (Bool False) = eval alternate 
schemeIfCondition _ _ _ = return (Error notACondition)

schemeIf :: [LispVal] -> StateTransformer LispVal
schemeIf [test, consequent, alternate] = eval test >>= (schemeIfCondition consequent alternate)
schemeIf [test, consequent] = eval test >>= (schemeIfCondition consequent (Error alternateNotSpecified))
schemeIf _ = return (Error wrongArgumentNumber)
-- END 'if'

-- BEGIN FUNCTION APPLICATION

schemeApply' :: [LispVal] -> LispVal -> StateTransformer LispVal
schemeApply' args (Native f) = return (f args)
schemeApply' args (Lambda formals body) = schemeLambda formals body args
schemeApply' _ _ = return (Error notAFunction)


schemeApply :: String -> [LispVal] -> StateTransformer LispVal
schemeApply name args = stateLookup name >>= schemeApply' args

-- END FUNCTION APPLICATION

-- BEGIN LAMBDA APPLICATION

schemeBindLambda :: [LispVal] -> [LispVal] -> [LispVal]
schemeBindLambda (a:as) (b:bs) = (List [a, b] : schemeBindLambda as bs)
schemeBindLambda (a:as) [] = (List [a, (Error unboundedVariable)] : schemeBindLambda as [])
schemeBindLambda _ _ = [] 

schemeLambda :: [LispVal] -> [LispVal] -> [LispVal] -> StateTransformer LispVal
schemeLambda formals body args = return (schemeBindLambda formals args)
								 >>= (\flist -> schemeInstantiateBindings flist empty)
								 >>= pushStack
								 >> evalList body
								 >>= popStack

-- END LAMBDA APPLICATION


-----------------------------------------------------------
--               GLOBAL ENVIRONMENT                      --
-----------------------------------------------------------

staticGlobalEnvironment :: VariableState
staticGlobalEnvironment =   
            insert "number?"        (Native schemePredNumber)
          $ insert "boolean?"       (Native schemePredBoolean)
          $ insert "list?"          (Native schemePredList)
          $ insert "+"              (Native schemeNumericSum) 
          $ insert "*"              (Native schemeNumericMult) 
          $ insert "-"              (Native schemeNumericSub) 
          $ insert "/"              (Native schemeNumericDiv) 
          $ insert "mod"            (Native schemeNumericMod) 
          $ insert "car"            (Native schemeCar)           
          $ insert "cdr"            (Native schemeCdr)
          $ insert "cons"			      (Native schemeCons)
          $ insert "lt?"			      (Native schemeLessThan)
          $ insert "eqv?"			      (Native schemeEquiv)
          $ insert "create-struct"  (Native schemeStructCreate) 
          $ insert "set-attr!"      (Native schemeStructSetAttr)  
          $ insert "get-attr"       (Native schemeStructGetAttr)	
            empty

   --- Uma função "/" para realizar a divisão inteira entre
   --números e uma função "mod" que forneça o resto da divisão inteira.
   --Você pode definir essas funções tanto em Scheme quanto em Haskell.

-----------------------------------------------------------
--                      ERROR STRING                     --
-----------------------------------------------------------

type ErrorString = String

notImplemented :: ErrorString
notImplemented = "NOT IMPLEMENTED YET."

dividedByZero :: ErrorString
dividedByZero = "dividing by zero."

notANumber :: ErrorString
notANumber = "not a number."

notACondition :: ErrorString
notACondition = "not a condition."

notAFunction :: ErrorString
notAFunction = "not a function."

unboundedVariable :: ErrorString
unboundedVariable = "unbounded variable"

alternateNotSpecified :: ErrorString
alternateNotSpecified = "alternate condition was not specified." 

noSuchVariable :: ErrorString
noSuchVariable = "variable does not exist."

noSuchAttribute :: ErrorString
noSuchAttribute = "attribute does not exist."

invalidArgumentList :: ErrorString
invalidArgumentList = "invalid list."

wrongArgumentNumber :: ErrorString
wrongArgumentNumber = "wrong number of arguments."

evalError :: LispVal -> ErrorString
evalError form = "Could not eval the special form: " ++ (show form)

-----------------------------------------------------------
--          HARDWIRED PREDEFINED LISP FUNCTIONS          --
-----------------------------------------------------------

-- Includes some auxiliary functions. Does not include functions that modify
-- state. These functions, such as define and set!, must run within the
-- StateTransformer monad. 

schemeCar :: [LispVal] -> LispVal
schemeCar [List (a:as)] = a
schemeCar [DottedList (a:as) _] = a
schemeCar ls = Error invalidArgumentList

schemeCdr :: [LispVal] -> LispVal
schemeCdr (List (a:as) : ls) = List as
schemeCdr (DottedList (a:[]) c : ls) = c
schemeCdr (DottedList (a:as) c : ls) = DottedList as c
schemeCdr ls = Error invalidArgumentList

schemePredNumber :: [LispVal] -> LispVal
schemePredNumber (Number _ : []) = Bool True
schemePredNumber (a:[]) = Bool False
schemePredNumber ls = Error wrongArgumentNumber

schemePredBoolean :: [LispVal] -> LispVal
schemePredBoolean (Bool _ : []) = Bool True
schemePredBoolean (a:[]) = Bool False
schemePredBoolean ls = Error wrongArgumentNumber

schemePredList :: [LispVal] -> LispVal
schemePredList (List _ : []) = Bool True
schemePredList (a:[]) = Bool False
schemePredList ls = Error wrongArgumentNumber

schemeNumericSum :: [LispVal] -> LispVal
schemeNumericSum [] = Number 0
schemeNumericSum l = schemeNumericBinOp (+) l

schemeNumericMult :: [LispVal] -> LispVal
schemeNumericMult [] = Number 1
schemeNumericMult l = schemeNumericBinOp (*) l

schemeNumericSub :: [LispVal] -> LispVal
schemeNumericSub [] = Error wrongArgumentNumber
-- The following case handles negative number literals.
schemeNumericSub [x] = if schemeOnlyNumbers [x]
                 then (\num -> (Number (- num))) (schemeUnpackNum x)
                 else Error notANumber
schemeNumericSub l = schemeNumericBinOp (-) l

-- We have not implemented division. Also, notice that we have not 
-- addressed floating-point numbers.

schemeNumericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
schemeNumericBinOp op args = if schemeOnlyNumbers args 
                       then Number $ foldl1 op $ Prelude.map schemeUnpackNum args 
                       else Error notANumber
                       
schemeOnlyNumbers :: [LispVal] -> Bool
schemeOnlyNumbers [] = True
schemeOnlyNumbers (Number n:ns) = schemeOnlyNumbers ns
schemeOnlyNumbers ns = False             
                       
schemeUnpackNum :: LispVal -> Integer
schemeUnpackNum (Number n) = n
--- schemeUnpackNum a = ... -- Should never happen!!!!

---- NEW FUNCTIONS

schemeCons :: [LispVal] -> LispVal
schemeCons [a, List as] = List (a:as)
schemeCons [a, DottedList as e] = DottedList (a:as) e
schemeCons ls = Error invalidArgumentList

schemeLessThan :: [LispVal] -> LispVal
schemeLessThan [(Number a), (Number b)] = Bool (a < b)
schemeLessThan [a, b] = Error notANumber
schemeLessThan _ = Error wrongArgumentNumber

schemeNumericDiv :: [LispVal] -> LispVal
schemeNumericDiv [(Number a), (Number 0)] = Error dividedByZero
schemeNumericDiv [(Number a), (Number b)] = Number (a `div` b)
schemeNumericDiv [a, b] = Error notANumber
schemeNumericDiv _ = Error wrongArgumentNumber

schemeNumericMod :: [LispVal] -> LispVal
schemeNumericMod [(Number a), (Number 0)] = Error dividedByZero
schemeNumericMod [(Number a), (Number b)] = Number (a `mod` b)
schemeNumericMod [a, b] = Error notANumber
schemeNumericMod _ = Error wrongArgumentNumber

schemeEquiv :: [LispVal] -> LispVal
schemeEquiv [a, b] = Bool (a == b)
schemeEquiv _ = Error wrongArgumentNumber

-----------------------------------------------------------
--                   STRUCT FUNCTIONS                    --
-----------------------------------------------------------

schemeStructCreate :: [LispVal] -> LispVal
schemeStructCreate [List l]
  | all isAtom l = Struct (fromList (Prelude.map (\(Atom id) -> (id, Number 0)) l))
  | otherwise = Error invalidArgumentList
schemeStructCreate _ = Error wrongArgumentNumber

schemeStructSetAttr :: [LispVal] -> LispVal
schemeStructSetAttr [(Struct args), (Atom id), val]
  | member id args = (Struct (insert id val args))
  | otherwise = Error noSuchAttribute
schemeStructSetAttr [_, _, _] = Error invalidArgumentList
schemeStructSetAttr _ = Error wrongArgumentNumber

schemeStructGetAttr :: [LispVal] -> LispVal
schemeStructGetAttr [(Struct args), (Atom idN)] = maybe (Error noSuchAttribute) id (Map.lookup idN args)
schemeStructGetAttr [_, _] = Error invalidArgumentList
schemeStructGetAttr _ = Error wrongArgumentNumber


-----------------------------------------------------------
--                     main FUNCTION                     --
-----------------------------------------------------------

showState :: SchemeState -> String
showState (global, []) = "\n" ++ show (toList global)
showState (global, (ctx:stack)) = show (toList ctx) ++ "\n" ++ showState (global, stack)

showResult :: (LispVal, SchemeState) -> String
showResult (val, defs) = show val ++ "\n" ++ showState defs ++ "\n"

getResult :: StateTransformer LispVal -> (LispVal, SchemeState)
getResult (State f) = f initialState -- we start with an empty state. 

main :: IO ()
main = do args <- getArgs
          putStr $ showResult $ getResult $ eval $ readExpr $ concat $ args 