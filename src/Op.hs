module Op (Op (..)) where

data Op
  = Number Integer
  | Add
  | Ap
  | B
  | C
  | Car
  | Cdr
  | Cons
  | Div
  | Eq
  | I
  | IsNil
  | Lt
  | Mul
  | Neg
  | Nil
  | S
  | T
  deriving (Show)
