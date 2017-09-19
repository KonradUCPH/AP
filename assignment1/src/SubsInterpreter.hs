module SubsInterpreter
       (
         Value(..)
       , runExpr
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", pCompare)
                       , ("<", pLT)
                       , ("+", pAdd)
                       , ("*", pMul)
                       , ("-", pMinus)
                       , ("%", pMod)
                       , ("Array", pMkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap = liftM
  -- implementation stolen from the testing lecture

instance Applicative SubsM where
  pure = return
  af <*> ax = do
                f <- af
                x <- ax
                return (f x)
  -- implementation stolen from the testing lecture

instance Monad SubsM where
  -- return :: a -> m a
  return x = SubsM ( \(env, _) -> Right (x, env) )
  -- (>>=) :: m a -> (a -> m b) -> m b
  m >>= f = SubsM ( \c -> case runSubsM m c of
                            Left e          -> Left e
                            Right (a, env') -> let (_, penv) = c
                                                   c' = (env', penv)
                                                   -- building a new context
                                               in runSubsM (f a) c'
                          )
  -- fail :: String -> m a
  fail s = SubsM ( \_ -> Left s )


pMkArray :: Primitive
pMkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
pMkArray _ = Left "Array() called with wrong number or type of arguments"

pAdd :: Primitive
pAdd [IntVal x, IntVal y] = return $ IntVal $ x + y
pAdd [StringVal x, StringVal y] = return $ StringVal $ x ++ y
pAdd [StringVal x, IntVal y] = return $ StringVal $ x ++ show y
pAdd [IntVal x, StringVal y] = return $ StringVal $ show x ++ y
pAdd _ = Left "Add() called with wrong number or type of arguments"

pMul :: Primitive
pMul [IntVal x, IntVal y] = return $ IntVal $ x * y
pMul _ = Left "Mul() called with wrong number or type of arguments"

pMinus :: Primitive
pMinus [IntVal x, IntVal y] = return $ IntVal $ x - y
pMinus _ = Left "Minus() called with wrong number or type of arguments"

pLT :: Primitive
pLT [IntVal x, IntVal y] | x < y = return TrueVal
pLT [IntVal _, IntVal _] = return FalseVal
pLT [StringVal x, StringVal y] | x < y = return TrueVal
pLT [StringVal _, StringVal _] = return FalseVal
pLT _ = Left "LessThan() called with wrong number or type of arguments"

pMod :: Primitive
pMod [IntVal x, IntVal y] = return $ IntVal $ mod x y
pMod _ = Left "Mod() called with wrong number or type of arguments"

pCompare :: Primitive
pCompare [x, y] | x == y = return TrueVal
pCompare [_, _] = return FalseVal
pCompare _ = Left "Compare() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\(env, _) -> Right ( (), f env))

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv $ Map.insert name val

deleteVar :: Ident -> SubsM ()
deleteVar name= modifyEnv $ Map.delete name

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(env, _) ->  -- QUESTION: how to use return and fail here?
                case Map.lookup name env of
                  (Just v) -> Right (v, env)
                  Nothing -> Left ("Variable used but not defined: \""
                                    ++ name ++ "\" ")
                )

-- gets a Maybe value instead of throwing an error in case of failure
getMaybeVar :: Ident -> SubsM (Maybe Value)
getMaybeVar name = SubsM (\(env, _) ->
                    case Map.lookup name env of
                      (Just v) -> Right (Just v, env)
                      Nothing -> Right (Nothing, env)
                    )


getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\(env, penv) ->
                    case Map.lookup name penv of
                      (Just p) -> Right (p, env)
                      Nothing -> Left ("Function used but not defined: \""
                                        ++ name ++ "\" ")
                    )

evalExpr :: Expr -> SubsM Value
evalExpr (Number n) = return (IntVal n)
evalExpr (String s) = return (StringVal s)
evalExpr (Array xs) = do
                        vs <- mapM evalExpr xs
                        return (ArrayVal vs)
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Var ident) = getVar ident
evalExpr (Compr ar) = evalArrayComp ar
evalExpr (Call fname params) = do
                                  (ArrayVal values) <- evalExpr (Array params)
                                  function <- getFunction fname
                                  case function values of
                                    (Left e) -> fail e
                                    (Right val) -> return val
evalExpr (Assign ident expr) = do
                                value <- evalExpr expr
                                putVar ident value
                                return value
evalExpr (Comma exp1 exp2) = do
                               _ <- evalExpr exp1
                               evalExpr exp2

-- Helper function to deal with List Comprehensions
evalArrayComp :: ArrayCompr -> SubsM Value
evalArrayComp (ACFor ident expr ac) = do
                                        iteratorValue <- evalExpr expr
                                        -- backup old variable
                                        maybeOldVar <- getMaybeVar ident
                                        eval <- evalArrayCompFor
                                                  ident iteratorValue ac
                                        -- restore old variable or cleanup
                                        case maybeOldVar of
                                          (Just oldVar) -> putVar ident oldVar
                                          Nothing -> deleteVar ident
                                        return eval
evalArrayComp (ACBody expr) =  evalExpr expr
evalArrayComp (ACIf expr ac) = do
                                  condition <- evalExpr expr
                                  case condition of
                                    TrueVal -> evalArrayComp ac
                                    FalseVal -> return (ArrayVal [])
                                    _ -> fail ("condition in if clause is " ++
                                               "not a boolean")

-- Helper function to deal with List Comprehensions.
-- This function does the iteration
evalArrayCompFor :: Ident -> Value -> ArrayCompr -> SubsM Value
evalArrayCompFor _ (ArrayVal []) _ = return (ArrayVal [])
evalArrayCompFor ident (ArrayVal (x:xs)) ac =
    do
      putVar ident x
      mv <- evalArrayComp ac
      (ArrayVal mvs) <- evalArrayCompFor ident (ArrayVal xs) ac
      -- accounting for nested for:
      case mv of
        (ArrayVal mv') -> return (ArrayVal (mv' ++ mvs))
        _ -> return (ArrayVal (mv:mvs))
evalArrayCompFor ident (StringVal s) ac =
    evalArrayCompFor ident (ArrayVal [StringVal [x] | x <- s]) ac
evalArrayCompFor _ _ _ =
    fail "Array Comprehension iterator has to be an Array or String"


runExpr :: Expr -> Either Error Value
runExpr expr = case runSubsM (evalExpr expr) initialContext of
                 Left e -> Left $ "Interpreter failed: " ++ e
                 Right (val, _) -> Right val