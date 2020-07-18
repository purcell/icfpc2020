-- This is really parsing tokens
module Parse (defs, test, Def (..), unsafeParseFile) where

import Op
import Text.Parsec
import Text.Parsec.String

data Def = Def {dName :: Name, dOps :: [Op]}
  deriving (Show)

name :: Parser Name
name = Name <$> (string "galaxy" <|> ((:) <$> char ':' <*> many1 digit))

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
    <|> try (Inc <$ string "inc")
    <|> try (IsNil <$ string "isnil")
    <|> try (Lt <$ string "lt")
    <|> try (Mul <$ string "mul")
    <|> try (Neg <$ string "neg")
    <|> try (Nil <$ string "nil")
    <|> try (Ref <$> name)
    <|> (B <$ string "b")
    <|> (C <$ string "c")
    <|> (I <$ string "i")
    <|> (S <$ string "s")
    <|> (T <$ string "t")

def :: Parser Def
def =
  Def <$> (name <* char ' ' <* char '=')
    <*> many1 (char ' ' *> op)

defs :: Parser [Def]
defs = sepBy1 def newline <* (optional newline >> eof)

unsafeParseFile :: FilePath -> IO [Def]
unsafeParseFile f = do
  res <- parseFromFile defs f
  either (error . show) pure res

test :: IO ()
test = do
  res <- parseFromFile defs "input/galaxy.txt"
  case res of
    Right ds -> print (length ds)
    Left e -> print e
