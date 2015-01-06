{-|
Module      : FreeTheorem
Description : Zastonjski izreki za tipe
Copyright   : (c) Domen Močnik, 2015
                  Matej Aleksandrov, 2015
License     : GPL-3
Maintainer  : domen.monik@gmail.com, matej.aleksandrov@gmail.com
Stability   : experimental
Portability : POSIX


-}

import Data.Maybe
import Parser
import TheoremGenerator
import Primeri

-- * Glavni program

-- | Funkcija prebere ime in predpis iz standardnega vhoda ter vrne zastonjski izrek.

main = do
    putStrLn "Vpiši ime tipa:"
    ime <- getLine    -- ^ Prebere ime predpisa.
    putStrLn "Vpiši predpis tipa:"
    predpis <- getLine       -- ^ Prebere predpis, ki ga podamo v standardnem vhodu.
    putStrLn (odgovor (parse predpis) ime) -- ^ Izpiše izrek oziroma sporoči napako.

-- | Funkcija, ki vrne odgovor.

odgovor :: Maybe Type -> String -> String
odgovor tip ime
   | isNothing tip = "Vnos je napačen."
   | otherwise   = "\nIzrek za " ++ ime ++ " :: " ++ (show (fromJust tip)) ++ ":\n\n" ++ (show izrek)
   where izrek = theorem ( fromJust tip) ime
