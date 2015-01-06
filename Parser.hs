{-|
Module      : Parser
Description : Program za pretvarjanje vhodnih podatkov v tipe
Copyright   : (c) Domen Močnik, 2015
                  Matej Aleksandrov, 2015
License     : GPL-3
Maintainer  : matej.aleksandrov@gmail.com
Stability   : experimental
Portability : POSIX

Modul vsebuje funkcijo parse, ki pretvori predpis tipa iz oblike niza znakov v 
Maybe Type. V primeru, da vhodni niz ne predstavlja tipa, vrne Nothing. Pri so 
spremenljivke v vhodnem nizu lahko sestavljene le iz črk in številk ter se začnejo
s črko. 
-}

module Parser where

import Data.Maybe
import Data.Char
import TheoremGenerator

-- * Glavna funkcija parse

-- | Za dani niz vrne pripadajoči Just Type oziroma Nothing, če je v nizu napaka.

parse :: String -> Maybe Type
parse [] = Nothing
parse (x:xs)  -- Funkcija deluje rekurzivno.
	| x==' '                   = parse xs  -- Presledke preskoči.
	| x=='(' && a1==(-1)       = Nothing  -- Nato preveri, ali se niz začne z oklepajom ter obravnava primere.
	| x=='(' && a3/=(-1) && (isNothing t1 || isNothing t2 || a3==(-2))   = Nothing
	| x=='(' && a3/=(-1)       = Just (TypeFun (fromJust t1) (fromJust t2))
	| x=='(' && isNothing t1   = Nothing
	| x=='('                   = t1
	| x=='[' && a2==(-1)       = Nothing  -- Preveri še, ali se niz začne s seznamom ter obravnva primere.
	| x=='[' && a4/=(-1) && (isNothing t3 || isNothing t4 || a4==(-2))   = Nothing
	| x=='[' && a4/=(-1)       = Just (TypeFun (TypeList(fromJust t3)) (fromJust t4))
	| x=='[' && (isNothing t3) = Nothing
	| x=='['                   = Just (TypeList(fromJust t3))
	| b1==(-1) || not(isAlpha x) = Nothing 
	| b2/=(-1) && (isNothing t5 || isNothing t6 || b2==(-2))   = Nothing -- Preveri, ali v nizu obstaja kakšen podniz "->".
	| b2/=(-1)                 = Just (TypeFun ( fromJust t5)  (fromJust t6))
	| elem s konstTipi 	       = Just (TypeConst s)  -- Imena konstant loči od imen spremenljivk.
	| otherwise                = Just (TypeVar s)
	where niz = (x:xs)
	      a1=zaklepaj 0 0 '(' ')' niz
	      a2=zaklepaj 0 0 '[' ']' niz
	      a3=naslednji (a1+1) (drop a1 niz)
	      a4=naslednji (a2+1) (drop a2 niz)
	      b1=konecNiza 0 niz
	      b2=naslednji (b1+1) (drop b1 niz)
	      s=take b1 niz
	      konstTipi = ["Bool", "Int", "Integer", "Float", "Double", "Char", "String"] -- Seznam konstantnih tipov.
	      t1=parse(take(a1-2) xs)
	      t2=parse(drop a3 niz)
	      t3=parse(take(a2-2) xs)
	      t4=parse(drop a4 niz)
	      t5=parse(take(b2-2) niz)
	      t6=parse(drop b2 niz)
	      
-- * Pomožne funckije	      

-- | Funkcija, ki za dano obliki predklepaja in zaklepaja poišče prvo mesto v nizu, kjer se nahaja najbolj zunanji zaklepaj. V primeru, da ga ne najde, vrne -1.

zaklepaj :: Int -> Int -> Char -> Char -> String-> Int
zaklepaj n m c1 c2 [] =1
zaklepaj n m c1 c2 [y]
	|y==c2        =m+1
	|otherwise    = -1
zaklepaj n m c1 c2 (y:ys)
	|y/=c1 && m==0 =1
	|n==0 && m==0   = zaklepaj (n+1) (m+1) c1 c2 ys
	|n==0           = m
     |y==c1         = zaklepaj (n+1) (m+1) c1 c2 ys
	|y==c2         = zaklepaj (n-1) (m+1) c1 c2 ys
	|otherwise      =zaklepaj n (m+1) c1 c2 ys

-- | Funkcija v nizu poišče prvo pojavitev podniza "->". Če ga ne najde, vrne -1, če najde kaj drugega, vrne -2.
 
naslednji :: Int -> String -> Int
naslednji n niz
	|(length niz)==0     = -1
	|(length niz)==1 && y/=' '= -2
	|(length niz)==1     = -1
	| x1=='-' && x2=='>' = n+1
	| x1/=' '             = -2
	| otherwise          = naslednji (n+1) (x2:xs)
	where (x1:x2:xs)=niz
	      (y:ys)=niz

-- | Funkcija v nizu poišče mesto, kjer se konča prvi podniz sestavljen samo iz črk in številk.

konecNiza :: Int -> String -> Int
konecNiza n [] = n 
konecNiza n (x:xs)
	| x==' ' || x=='-' =n
	| not (isAlphaNum x) = -1
	| otherwise  = konecNiza (n+1) xs
	
