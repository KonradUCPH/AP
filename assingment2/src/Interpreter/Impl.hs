module SubsInterpreter
       (
         Value(..)
       , runExpr
       -- exporting additional components for testing monadic laws
       , evalExpr
       , initialContext
       , runSubsM
       , SubsM
       )
       where

import SubsAst
import Value
import qualified Primitives as P

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)

type Env = Map Ident Value
type PEnv = Map FunName P.Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", P.compareP)
                       , ("<", P.lt)
                       , ("+", P.add)
                       , ("*", P.mul)
                       , ("-", P.minus)
                       , ("%", P.modP)
                       , ("Array", P.mkArray)
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

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\(env, _) -> Right ( (), f env))

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv $ Map.insert name val

deleteVar :: Ident -> SubsM ()
deleteVar name= modifyEnv $ Map.delete name

getVar :: Ident -> SubsM Value
getVar name = SubsM (\(env, _) ->
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


getFunction :: FunName -> SubsM P.Primitive
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
          v <- evalArrayComp ac
          vs <- evalArrayCompFor ident (ArrayVal xs) ac
          combineACForResult v vs ac
evalArrayCompFor ident (StringVal s) ac =
        evalArrayCompFor ident (ArrayVal [StringVal [x] | x <- s]) ac
evalArrayCompFor _ _ _ =
        fail "Array Comprehension iterator has to be an Array or String"

-- combines the result of the current and recursive array comp
combineACForResult :: Value -> Value -> ArrayCompr -> SubsM Value
combineACForResult v (ArrayVal vs) (ACBody _) =
  return (ArrayVal (v:vs)) -- preserve nested array
combineACForResult v (ArrayVal vs) _ =
  case v of
    (ArrayVal v') -> return (ArrayVal (v' ++ vs)) -- destroy nested array
    _ -> return (ArrayVal (v:vs)) -- preserve nested array
combineACForResult _ _ _ = fail ("unexpected error: evaluation of Array "
                                ++ "Composition did not yield an array")


runExpr :: Expr -> Either Error Value
runExpr expr = case runSubsM (evalExpr expr) initialContext of
                 Left e -> Left $ "Interpreter failed: " ++ e
                 Right (val, _) -> Right val