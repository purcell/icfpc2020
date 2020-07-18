{-# LANGUAGE BangPatterns #-}

module Runtime where

import qualified Data.List as L
import Prelude

type Cons = [Integer]

nil :: Cons
nil = mempty

eq !x !y = if x == y then t else f

mul :: Integer -> Integer -> Integer
mul = (*)

inc :: Integer -> Integer
inc = (+ 1)

add :: Integer -> Integer -> Integer
add = (+)

lt x y = if x < y then t else f

div :: Integer -> Integer -> Integer
div = Prelude.div

i :: a -> a
i = Prelude.id

t = k

f = s . t

isnil a = if L.null a then t else f

cons :: Integer -> Cons -> Cons
cons = (:)

car :: Cons -> Integer
car (!x : _) = x
car [] = error "car of empty list"

cdr :: Cons -> Cons
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

{-# INLINE i #-}

{-# INLINE t #-}

{-# INLINE mul #-}

{-# INLINE eq #-}

{-# INLINE nil #-}

{-# INLINE inc #-}

{-# INLINE add #-}

{-# INLINE lt #-}

{-# INLINE b #-}

{-# INLINE c #-}

{-# INLINE k #-}

{-# INLINE w #-}

{-# INLINE s #-}
