{-|
Module      : Primeri
Description : Primeri tipov
Copyright   : (c) Domen Močnik, 2015
                  Matej Aleksandorv 2015
License     : GPL-3
Maintainer  : domen.monik@gmail.com, matej.aleksandrov@gmail.com
Stability   : experimental
Portability : POSIX

Nekaj primerov tipov, ki se jih lahko uporabi v programu za generiranje zastonjskih izrekov.

Leva komponenta para je predstavitev tipa v nizu, desna komponenta pa je ime tipa.
-}

module Primeri where

-- ^ Primere se testira z: odgovor tPrimer.

-- | Aplikacija.
tApplication :: (String, String)
tApplication = ("(a -> b) -> a -> b", "app")

-- | Kompozitum.
tComposition :: (String, String)
tComposition = ("(a -> b) -> (b -> c) -> (a -> c)", "comp")

-- | Fold.
tFold :: (String, String)
tFold = ("(a -> b -> b) -> b -> [a] -> b", "fold")

-- | Filter.
tFilter :: (String, String)
tFilter = ("(a -> Bool) -> [a] -> [a] ", "filter")

-- | Map.
tMap :: (String, String)
tMap = ("(a -> b) -> [a] -> [b]", "map")

-- | Konkatenacija.
tConcat :: (String, String)
tConcat = ("[[a]] -> [a]", "concat")

-- | Izmišljena 1.
tIzm :: (String, String)
tIzm = ("x -> (y -> [x -> [y]]) -> y", "f")

-- | Reverse.
tReverse :: (String, String)
tReverse = ("[x] -> [x]", "reverse")
