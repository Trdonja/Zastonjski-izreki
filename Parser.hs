module Parser
	where

import Zastonjski

parse :: String -> Type  --Glavna funkcija parse
parse [] = TypeVar ""
parse [x] = TypeVar [x]
parse (x:xs)
	| x==' '                 = parse xs
	| x=='(' && a3/=(-1)     = TypeFun (parse(take(a1-2) xs)) (parse(drop a3 niz))
	| x=='('                 = parse(take (a1-2) xs)
	| x=='[' && a4/=(-1)     = TypeFun (TypeList(parse(take(a2-2) xs))) (parse(drop a4 niz))
	| x=='['                 = TypeList(parse(take (a2-2) xs))
	| a3 /=(-1)              = TypeFun ( parse(take(a3-2) niz) )  (parse (drop a3 niz))
	| elem s konstTipi 	     = TypeConst s
	| otherwise              = TypeVar s
	where niz = (x:xs)
	      a1=zaklepaj 0 0 '(' ')' niz
	      a2=zaklepaj 0 0 '[' ']' niz
	      a3=naslednji a1 (drop (a1-1) niz)
	      a4=naslednji a2 (drop (a2-1) niz)
	      s=take(konecNiza 0 niz) niz
	      konstTipi = ["Bool", "Int", "Char", "Double"] --Seznam konstantnih tipov
	      
zaklepaj :: Int -> Int -> Char -> Char -> String-> Int  -- Poišče mesto zaklepaja, ki pripada predklepaju na začetku niza
zaklepaj n m c1 c2 [] =1
zaklepaj n m c1 c2 [y] = m+1
zaklepaj n m c1 c2 (y:ys)
	|y/=c1 && m==0 =1
	|n==0 && m==0   = zaklepaj (n+1) (m+1) c1 c2 ys
	|n==0           = m
     |y==c1         = zaklepaj (n+1) (m+1) c1 c2 ys
	|y==c2         = zaklepaj (n-1) (m+1) c1 c2 ys
	|otherwise      =zaklepaj n (m+1) c1 c2 ys

naslednji :: Int -> String -> Int   -- V obliki '( ...) -> ...' poišče mesto, kjer se začne 2. del
naslednji n niz
	|(length niz)<=1     = -1
	| x1=='-' && x2=='>' = n+1
	| otherwise          = naslednji (n+1) (x2:xs)
	where (x1:x2:xs)=niz

konecNiza :: Int -> String -> Int  --
konecNiza n [] = n 
konecNiza n [' '] =n
konecNiza n [x] = n+1
konecNiza n (x:xs)
	|x==' '    =n
	|otherwise =konecNiza (n+1) xs
	
