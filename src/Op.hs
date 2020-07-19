-- These are tokens
module Op where

newtype Name = Name String
  deriving (Show, Eq, Ord)

data Expr
  = Number !Integer
  | Add
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
  | Ref !Name
  | S
  | T
  | F
  | Ap !Expr !Expr
  deriving (Show)
