module Parse (defs, test, Def (..), OpOrRef (..)) where

import Op
import Text.Parsec
import Text.Parsec.String

data OpOrRef = AnOp Op | Ref UserFnName
  deriving (Show)

newtype UserFnName = UserFnName String
  deriving (Show)

data Def = Def {dName :: UserFnName, dOps :: [OpOrRef]}
  deriving (Show)

defname :: Parser UserFnName
defname = UserFnName <$> (string "galaxy" <|> ((:) <$> char ':' <*> many1 digit))

number :: Parser Integer
number =
  posNumber
    <|> (char '-' >> (* (-1)) <$> posNumber)

posNumber :: Parser Integer
posNumber = read <$> many1 digit

op :: Parser Op
op =
  (Number <$> number)
    <|> try (Add <$ string "add")
    <|> try (Ap <$ string "ap")
    <|> try (Car <$ string "car")
    <|> try (Cdr <$ string "cdr")
    <|> try (Cons <$ string "cons")
    <|> try (Div <$ string "div")
    <|> try (Eq <$ string "eq")
    <|> try (IsNil <$ string "isnil")
    <|> try (Lt <$ string "lt")
    <|> try (Mul <$ string "mul")
    <|> try (Neg <$ string "neg")
    <|> try (Nil <$ string "nil")
    <|> (B <$ string "b")
    <|> (C <$ string "c")
    <|> (I <$ string "i")
    <|> (S <$ string "s")
    <|> (T <$ string "t")

opOrRef :: Parser OpOrRef
opOrRef = (AnOp <$> op) <|> (Ref <$> defname)

def :: Parser Def
def =
  Def <$> (defname <* char ' ' <* char '=')
    <*> many1 (char ' ' *> opOrRef)

defs :: Parser [Def]
defs = sepBy1 def newline <* (optional newline >> eof)

test :: IO ()
test = do
  res <- parseFromFile defs "input/galaxy.txt"
  case res of
    Right ds -> print (length ds)
    Left e -> print e
