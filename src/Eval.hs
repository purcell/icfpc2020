{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import Control.Monad.Reader
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Op
import qualified Parse

newtype EvalError = EvalError String
  deriving (Show)

asNum :: Expr -> Integer
asNum (Number i) = i
asNum e = error $ "Not a number: " <> show e

numOp :: (MonadReader (Map Name Expr) m) => (Integer -> Integer -> a) -> Expr -> Expr -> m a
numOp op a b = op <$> (asNum <$> eval a) <*> (asNum <$> eval b)

toBoolean :: Bool -> Expr
toBoolean True = T
toBoolean False = F

eval :: (MonadReader (Map Name Expr) m) => Expr -> m Expr
eval (Ap (Ap (Ap S x0) x1) x2) = pure (Ap (Ap x0 x2) (Ap x1 x2))
eval (Ap (Ap (Ap C x0) x1) x2) = pure (Ap (Ap x0 x2) x1)
eval (Ap (Ap (Ap B x0) x1) x2) = pure (Ap x0 (Ap x1 x2))
eval (Ap (Ap (Ap Cons x0) x1) x2) = pure (Ap (Ap x2 x0) x1)
eval (Ap (Ap (Ap f x0) x1) x2) = eval f >>= \f' -> pure (Ap (Ap (Ap f' x0) x1) x2)
eval (Ap (Ap Cons x) y) = do
  x' <- eval x
  y' <- eval y
  pure (Ap (Ap Cons x') y')
eval (Ap (Ap Mul a) b) = Number <$> numOp (*) a b
eval (Ap (Ap Div a) b) = Number <$> numOp div a b
eval (Ap (Ap Add a) b) = Number <$> numOp (+) a b
eval (Ap (Ap Lt a) b) = toBoolean <$> numOp (<) a b
eval (Ap (Ap F _) y) = y
eval (Ap (Ap T x) _) = x
eval (Ap (Ap Eq a) b) = toBoolean <$> numOp (==) a b
eval (Ap (Ap f x0) x1) = eval f >>= \f' -> (Ap (Ap f' x0) x1)
eval (Ref name) = reader (Map.! name)
eval (Ap IsNil Nil) = T
eval (Ap Neg x) = eval Number (- x)
eval (Ap Inc a) = Number (asNum (ev a) + 1)
eval (Ap I x) = x
eval (Ap Car x2) = (Ap x2 T)
eval (Ap Cdr x2) = (Ap x2 F)
eval (Ap f x) = Ap f <$> eval x
eval v@(Number _) = v
eval expr = error ("no eval for " <> show expr)

evalIn :: Map Name Expr -> Expr -> Expr
evalIn bindings = fix (eval bindings)

galaxy :: IO Expr
galaxy = do
  defs <- Parse.unsafeParseFile "input/galaxy.txt"
  let bindings = Map.fromList [(dName, dExpr) | Parse.Def {..} <- defs]
  pure $ evalIn bindings (Ref (Name "galaxy"))
