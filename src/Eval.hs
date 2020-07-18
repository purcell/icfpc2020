{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import Control.Monad.Reader
import Control.Monad.State.Lazy as St
import Data.Map (Map)
import qualified Data.Map as Map
import Op
import qualified Parse

newtype EvalError = EvalError String
  deriving (Show)

data Value
  = Lit Integer
  | Vec [Integer]
  | Logic Bool

eval :: (MonadState (Map Name Expr) m, MonadFail m) => Expr -> m Value
eval (Number i) = pure $ Lit i
eval (Ref name) = force name
eval (Ap (Ap Cons e) rest) = do
  Lit x <- eval e
  Vec xs <- eval rest
  pure $ Vec (x : xs)
eval (Ap Car rest) = do
  Vec (x : xs) <- eval rest
  pure $ Lit x
eval (Ap Cdr e) = do
  Lit x <- eval e
  pure $ Lit x
eval (Ap Car e) = do
  Vec (x : xs) <- eval e
  pure $ Lit x
eval (Ap (Ap Mul a) b) = do
  Lit x <- eval a
  Lit y <- eval b
  pure $ Lit (x * y)
eval (Ap (Ap Div a) b) = do
  Lit x <- eval a
  Lit y <- eval b
  pure $ Lit (x `div` y)
eval (Ap (Ap Add a) b) = do
  Lit x <- eval a
  Lit y <- eval b
  pure $ Lit (x + y)
eval (Ap Neg a) = do
  Lit x <- eval a
  pure $ Lit ((-1) * x)
eval (Ap Inc a) = do
  Lit x <- eval a
  pure $ Lit (x + 1)
eval (Ap Inc a) = do
  Lit x <- eval a
  pure $ Lit (x + 1)
eval (Ap (Ap Lt a) b) = do
  Lit x <- eval a
  Lit y <- eval b
  pure $ Logic (x < y)
eval (Ap (Ap Eq a) b) = do
  Lit x <- eval a
  Lit y <- eval b
  pure $ Logic (x == y)
eval (Ap (Ap Eq a) b) = do
  Lit x <- eval a
  Lit y <- eval b
  pure $ Logic (x == y)
eval (Ap (Ref name) arg) = do
  fv <- force name
  eval (Ap fv arg)

replaceRefs :: MonadReader (Map Name Expr) m => Expr -> m Expr
replaceRefs (Ref name) = replaceRefs =<< reader (Map.! name)
replaceRefs (Ap (Ref name) arg) = Ap <$> replaceRefs (Ref name) <*> replaceRefs arg
replaceRefs e = pure e

force :: (MonadReader (Map Name Expr) m, MonadState (Map Name Value) m, MonadFail m) => Name -> m Value
force name = do
  val <- gets (Map.lookup name)
  case val of
    Just v -> pure v
    Nothing -> do
      expr <- reader (Map.! name)
      val <- eval expr
      modify (Map.insert name val)
      pure val

-- exprToHS :: Expr -> String
-- exprToHS (Number i)
--   | i < 0 = parens (show i)
--   | otherwise = show i
-- exprToHS (Ref n) = nameToHS n
-- exprToHS (Ap a@(Ap _ _) b) = parens (exprToHS a) <> " " <> exprToHS b
-- exprToHS (Ap a b) = exprToHS a <> " " <> exprToHS b
-- exprToHS Cons = "cons"
-- exprToHS Cdr = "cdr"
-- exprToHS Eq = "eq"
-- exprToHS Mul = "mul"
-- exprToHS Div = "div"
-- exprToHS Add = "add"
-- exprToHS Inc = "inc"
-- exprToHS Lt = "lt"
-- exprToHS Neg = "neg"
-- exprToHS Nil = "nil"
-- exprToHS IsNil = "isnil"
-- exprToHS I = "i"
-- exprToHS T = "t"
-- exprToHS C = "c"
-- exprToHS B = "b"
-- exprToHS S = "s"
-- exprToHS Car = "car"

-- test :: String
-- test = defToHS (Parse.Def (Name ":1030") [Ap, Ap, Cons, Number 2, Ap, Ap, Cons, Number 7, Nil])

-- fullTest :: IO ()
-- fullTest = do
--   defs <- Parse.unsafeParseFile "input/galaxy.txt"
--   -- defs <- Parse.unsafeParseFile "/tmp/sample"
--   writeFile
--     "cli/Galaxy.hs"
--     ( "{-# LANGUAGE NoImplicitPrelude #-}\n"
--         <> "module Galaxy where\n"
--         <> "import Runtime\n"
--         <> compile defs
--         <> "\nmain = print galaxy"
--     )
