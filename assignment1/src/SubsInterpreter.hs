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
  af <*> ax = do f <- af
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
pLT _ = Left "Less than() called with wrong number or type of arguments"

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

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(env, _) ->  -- QUESTION: how to use return and fail here?
                case Map.lookup name env of
                  (Just v) -> Right (v, env)
                  Nothing -> Left ("Variable used but not defined: \""
                                    ++ name ++ "\" ")
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
evalExpr (Array []) = return (ArrayVal []) --TODO: do better with mapM
evalExpr (Array (x:xs)) = do v <- evalExpr x
                             (ArrayVal vs) <- evalExpr (Array xs)
                             return (ArrayVal (v:vs))
evalExpr Undefined = fail "expression undefined."
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Var ident) = getVar ident
evalExpr (Compr ar) = undefined --TODO
evalExpr (Call fname params) = do (ArrayVal values) <- evalExpr (Array params)
                                  function <- getFunction fname
                                  case function values of
                                    (Left e) -> fail e
                                    (Right val) -> return val
evalExpr (Assign ident expr) = do value <- evalExpr expr
                                  putVar ident value
                                  return value
evalExpr (Comma exp1 exp2) = do evalExpr exp1
                                evalExpr exp2



runExpr :: Expr -> Either Error Value
runExpr expr = case runSubsM (evalExpr expr) initialContext of
                 Left e -> Left $ "Interpreter failed: " ++ e
                 Right (val, _) -> Right val
  
  
