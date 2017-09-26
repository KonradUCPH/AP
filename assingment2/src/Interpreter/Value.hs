module Value  (
    Value(..),
    Error
    ) where

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