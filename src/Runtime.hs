module Runtime where

import qualified Data.List as L
import Prelude

nil :: [a]
nil = mempty

eq :: Eq a => a -> a -> Bool
eq = (==)

mul :: Integer -> Integer -> Integer
mul = (*)

inc :: Integer -> Integer
inc = (+ 1)

add :: Integer -> Integer -> Integer
add = (+)

lt :: Ord a => a -> a -> Bool
lt = (<)

div :: Integer -> Integer -> Integer
div = Prelude.div

isNil :: [a] -> Bool
isNil = L.null

i :: a -> a
i = Prelude.id

t = k

cons :: a -> [a] -> [a]
cons = (:)

car (x : _) = x
car [] = error "car of empty list"

cdr [] = error "cdr of empty list"
cdr l = tail l

neg :: Integer -> Integer
neg = (* (-1))

-- https://en.wikipedia.org/wiki/B,_C,_K,_W_system
b x y z = x (y z)

c x y z = x z y

k x _y = x

w x y = x y y

s = b (b w) (b b c)

print :: Show a => a -> IO ()
print = Prelude.print
