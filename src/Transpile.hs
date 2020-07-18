{-# LANGUAGE RecordWildCards #-}

module Transpile where

-- import Control.Monad.Reader
-- import Control.Monad.State.Lazy as St
-- import Data.Map (Map)
-- import qualified Data.Map as Map

import qualified Op
import qualified Parse

newtype EvalError = EvalError String
  deriving (Show)

data Value

compile :: [Parse.Def] -> String
compile defs =
  unlines (defToHS <$> defs)

defToHS :: Parse.Def -> String
defToHS Parse.Def {..} =
  nameToHS dName
    <> " = "
    <> exprToHS dExpr

nameToHS :: Op.Name -> String
nameToHS (Op.Name n) =
  case n of
    (':' : rest) -> "fn" <> rest
    _ -> n

parens :: String -> String
parens s = "(" <> s <> ")"

exprToHS :: Op.Expr -> String
exprToHS (Op.Number i)
  | i < 0 = parens (show i)
  | otherwise = show i
exprToHS (Op.Ref n) = nameToHS n
exprToHS (Op.Ap a@(Op.Ap _ _) b) = parens (exprToHS a) <> " " <> exprToHS b
exprToHS (Op.Ap a b) = exprToHS a <> " " <> exprToHS b
exprToHS Op.Cons = "cons"
exprToHS Op.Cdr = "cdr"
exprToHS Op.Eq = "eq"
exprToHS Op.Mul = "mul"
exprToHS Op.Div = "div"
exprToHS Op.Add = "add"
exprToHS Op.Inc = "inc"
exprToHS Op.Lt = "lt"
exprToHS Op.Neg = "neg"
exprToHS Op.Nil = "nil"
exprToHS Op.IsNil = "isnil"
exprToHS Op.I = "i"
exprToHS Op.T = "t"
exprToHS Op.C = "c"
exprToHS Op.B = "b"
exprToHS Op.S = "s"
exprToHS Op.Car = "car"

-- test :: String
-- test = defToHS (Parse.Def (Op.Name ":1030") [Op.Ap, Op.Ap, Op.Cons, Op.Number 2, Op.Ap, Op.Ap, Op.Cons, Op.Number 7, Op.Nil])

fullTest :: IO ()
fullTest = do
  defs <- Parse.unsafeParseFile "input/galaxy.txt"
  -- defs <- Parse.unsafeParseFile "/tmp/sample"
  writeFile
    "cli/Galaxy.hs"
    ( "{-# LANGUAGE NoImplicitPrelude #-}\n"
        <> "module Galaxy where\n"
        <> "import Runtime\n"
        <> compile defs
        <> "\nmain = print galaxy"
    )
