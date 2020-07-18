-- These are tokens
module Op where

newtype Name = Name String
  deriving (Show, Eq, Ord)

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
  | Inc
  | IsNil
  | Lt
  | Mul
  | Neg
  | Nil
  | Ref Name
  | S
  | T
  deriving (Show)
