{-# LANGUAGE RecordWildCards #-}

module Eval where

-- import Control.Monad.Reader
-- import Control.Monad.State.Lazy as St
-- import Data.Map (Map)
-- import qualified Data.Map as Map

import Data.Tree (Tree)
import qualified Data.Tree as Tree
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
    <> unwords
      ( opsToHS
          dOps
      )

nameToHS :: Op.Name -> String
nameToHS (Op.Name n) =
  case n of
    (':' : rest) -> "fn" <> rest
    _ -> n

opsToHS :: [Op.Op] -> [String]
opsToHS ((Op.Number i) : rest) =
  ( if i < 0
      then "(" <> show i <> ")"
      else show i
  ) :
  opsToHS rest
opsToHS ((Op.Ref n) : rest) = nameToHS n : opsToHS rest
opsToHS (Op.Ap : func : rest) =
  opsToHS [func] <> ["$"] <> opsToHS rest
opsToHS (Op.Cons : rest) = "cons" : opsToHS rest
opsToHS (Op.Cdr : rest) = "cdr" : opsToHS rest
opsToHS (Op.Eq : rest) = "eq" : opsToHS rest
opsToHS (Op.Mul : rest) = "mul" : opsToHS rest
opsToHS (Op.Div : rest) = "div" : opsToHS rest
opsToHS (Op.Add : rest) = "add" : opsToHS rest
opsToHS (Op.Inc : rest) = "inc" : opsToHS rest
opsToHS (Op.Lt : rest) = "lt" : opsToHS rest
opsToHS (Op.Neg : rest) = "neg" : opsToHS rest
opsToHS (Op.Nil : rest) = "nil" : opsToHS rest
opsToHS (Op.IsNil : rest) = "isNil" : opsToHS rest
opsToHS (Op.I : rest) = "i" : opsToHS rest
opsToHS (Op.T : rest) = "t" : opsToHS rest
opsToHS (Op.C : rest) = "c" : opsToHS rest
opsToHS (Op.B : rest) = "b" : opsToHS rest
opsToHS (Op.S : rest) = "s" : opsToHS rest
opsToHS (Op.Car : rest) = "car" : opsToHS rest
opsToHS [] = mempty

test :: String
test = defToHS (Parse.Def (Op.Name ":1030") [Op.Ap, Op.Ap, Op.Cons, Op.Number 2, Op.Ap, Op.Ap, Op.Cons, Op.Number 7, Op.Nil])

fullTest :: IO ()
fullTest = do
  defs <- Parse.unsafeParseFile "input/galaxy.txt"
  -- defs <- Parse.unsafeParseFile "/tmp/sample"
  writeFile
    "/tmp/sample.noths"
    ( "{-# LANGUAGE NoImplicitPrelude #-}\n"
        <> "import Runtime\n"
        <> compile defs
        <> "\nmain = print galaxy"
    )
