{-
    A module containing the primitives for the subset interpreter
-}
module Primitives (
    Primitive,
    mkArray,
    add,
    mul,
    minus,
    lt,
    modP,
    compareP
    ) where

import Value

type Primitive = [Value] -> Either Error Value


mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

add :: Primitive
add [IntVal x, IntVal y] = return $ IntVal $ x + y
add [StringVal x, StringVal y] = return $ StringVal $ x ++ y
add [StringVal x, IntVal y] = return $ StringVal $ x ++ show y
add [IntVal x, StringVal y] = return $ StringVal $ show x ++ y
add _ = Left "Add() called with wrong number or type of arguments"

mul :: Primitive
mul [IntVal x, IntVal y] = return $ IntVal $ x * y
mul _ = Left "Mul() called with wrong number or type of arguments"

minus :: Primitive
minus [IntVal x, IntVal y] = return $ IntVal $ x - y
minus _ = Left "Minus() called with wrong number or type of arguments"

lt :: Primitive
lt [IntVal x, IntVal y] | x < y = return TrueVal
lt [IntVal _, IntVal _] = return FalseVal
lt [StringVal x, StringVal y] | x < y = return TrueVal
lt [StringVal _, StringVal _] = return FalseVal
lt _ = Left "LessThan() called with wrong number or type of arguments"

modP :: Primitive
modP [IntVal x, IntVal y] = return $ IntVal $ mod x y
modP _ = Left "Mod() called with wrong number or type of arguments"

compareP :: Primitive
compareP [x, y] | x == y = return TrueVal
compareP [_, _] = return FalseVal
compareP _ = Left "Compare() called with wrong number or type of arguments"
