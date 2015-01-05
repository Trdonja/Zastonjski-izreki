module Parser
	where

import Data.Maybe
import Data.Char
import Zastonjski

parse :: String -> Maybe Type  --Glavna funkcija parse
parse [] = Nothing
parse (x:xs)
	| x==' '                   = parse xs
	| x=='(' && a1==(-1)       = Nothing
	| x=='(' && a3/=(-1) && (isNothing t1 || isNothing t2 || a3==(-2))   = Nothing
	| x=='(' && a3/=(-1)       = Just (TypeFun (fromJust t1) (fromJust t2))
	| x=='(' && isNothing t1   = Nothing
	| x=='('                   = t1
	| x=='[' && a2==(-1)       = Nothing
	| x=='[' && a4/=(-1) && (isNothing t3 || isNothing t4 || a4==(-2))   = Nothing
	| x=='[' && a4/=(-1)       = Just (TypeFun (TypeList(fromJust t3)) (fromJust t4))
	| x=='[' && (isNothing t3) = Nothing
	| x=='['                   = Just (TypeList(fromJust t3))
	| b1==(-1) || not(isAlpha x) = Nothing 
	| b2/=(-1) && (isNothing t5 || isNothing t6 || b2==(-2))   = Nothing
	| b2/=(-1)                 = Just (TypeFun ( fromJust t5)  (fromJust t6))
	| elem s konstTipi 	       = Just (TypeConst s)
	| otherwise                = Just (TypeVar s)
	where niz = (x:xs)
	      a1=zaklepaj 0 0 '(' ')' niz
	      a2=zaklepaj 0 0 '[' ']' niz
	      a3=naslednji (a1+1) (drop a1 niz)
	      a4=naslednji (a2+1) (drop a2 niz)
	      b1=konecNiza 0 niz
	      b2=naslednji (b1+1) (drop b1 niz)
	      s=take b1 niz
	      konstTipi = ["Bool", "Int", "Integer", "Float", "Double", "Char"] --Seznam konstantnih tipov
	      t1=parse(take(a1-2) xs)
	      t2=parse(drop a3 niz)
	      t3=parse(take(a2-2) xs)
	      t4=parse(drop a4 niz)
	      t5=parse(take(b2-2) niz)
	      t6=parse(drop b2 niz)
	      
zaklepaj :: Int -> Int -> Char -> Char -> String-> Int  -- Poišče mesto zaklepaja, ki pripada predklepaju na začetku niza
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

naslednji :: Int -> String -> Int   -- V obliki "... -> ..." poišče mesto znaka '>' 
naslednji n niz
	|(length niz)==0     = -1
	|(length niz)==1 && y/=' '= -2
	|(length niz)==1     = -1
	| x1=='-' && x2=='>' = n+1
	| x1/=' '             = -2
	| otherwise          = naslednji (n+1) (x2:xs)
	where (x1:x2:xs)=niz
	      (y:ys)=niz

konecNiza :: Int -> String -> Int  -- Poišče mesto, kjer se konča niz črk
konecNiza n [] = n 
konecNiza n (x:xs)
	| x==' ' || x=='-' =n
	| not (isAlphaNum x) = -1
	| otherwise  = konecNiza (n+1) xs
