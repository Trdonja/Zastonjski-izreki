{-|
Module      : TheoremGenerator
Description : Generator zastonjskih izrekov za tipe
Copyright   : (c) Domen Močnik, 2015
                  Matej Aleksandrov, 2015
License     : GPL-3
Maintainer  : domen.monik@gmail.com
Stability   : experimental
Portability : POSIX

Modul vsebuje potrebne podatkovne strukture za predstavitev tipov ter generiranje zastonjskih
izrekov za tipe. Vključuje funkcijo, ki za dani tip vrne pripadajoči zastonjski izrek.

Ta projekt je omejen na tipe, ki so sestavljeni iz spremenljivk, konstantnih tipov,
funkcij med tipi ter seznami tipov.
-}

module TheoremGenerator where

import Data.List

-- * Osnovne podatkovne strukture

-- ** Tip

-- | Tipovna spremenljivka.

type TypeVariable = String


-- | Tip.

data Type
    = TypeVar TypeVariable      -- ^ Spremenljivka
    | TypeConst TypeVariable    -- ^ Konstantni tip
    | TypeFun Type Type         -- ^ Funkcijski konstruktor tipov ->
    | TypeList Type             -- ^ Konstruktor za sezname []

instance Show Type where
    show (TypeVar v) = v
    show (TypeConst c) = c
    show (TypeList t) = "[" ++ (show t) ++ "]"
    show (TypeFun t1 t2) = case t1 of
        TypeFun _ _ -> "(" ++ (show t1) ++ ") -> " ++ (show t2)
        _ -> (show t1) ++ " -> " ++ (show t2)


-- ** Izraz

-- | Spremenljivka, ki predstavlja izraz.

type TermVariable = String


-- | Izraz.

data Term
	= TermVar TermVariable  -- ^ Spremenljivka, ki predstavlja izraz
	| TermApp Term Term     -- ^ Aplikacija izraza na izraz

instance Show Term where
    show (TermVar v) = v
    show (TermApp t1 t2) = case t2 of
        TermApp _ _ -> (show t1) ++ " (" ++ (show t2) ++ ")"
        _ -> (show t1) ++ " " ++ (show t2)


-- ** Relacija

-- | Relacijska spremenljivka.

type RelationVariable = String


-- | Osnovna relacija med dvema osnovnima tipoma. Vsaki tipovni spremenljivki v nekem tipu pripada natanko ena osnovna relacija.

data Relation = Relation { associatedType :: TypeVariable   -- ^ tipovna spremenljivka, na katero se nanaša relacija
                         , relvar :: RelationVariable       -- ^ spremenljivka, ki predstavlja relacijo
                         , leftType :: TypeVariable         -- ^ spremenljivka levega tipa
                         , rightType :: TypeVariable        -- ^ spremenljivka desnega tipa
                         }


-- | Relacija, ki pripada seznamu

data ListRelation = ListRelation { listrelvar :: RelationVariable   -- ^ Spremenljivka, ki predstavlja relacijo
                                 , headformula :: Formula           -- ^ Formula za glavi iz para, ki pripada relaciji za seznam
                                 , tailformula :: Formula           -- ^ Formula za repa iz para, ki pripada relaciji za seznam
                                 }

instance Show ListRelation where
    show lr = (listrelvar lr) ++ " = {([],[])} union\n{(x : xs, y : ys) | (" ++ (show $ headformula lr) ++ ") and (" ++ (show $ tailformula lr) ++ ")}"


-- | Izpiše elemente seznama, ločene s praznimi vrsticami.

listToString :: Show a => [a] -> String
listToString list = case list of [] -> ""
                                 l:ls -> (show l) ++ "\n\n" ++ (listToString ls)


-- ** Formula in izrek

-- | Logična formula, ki predstavlja zastonjski izrek.

data Formula
    = ForallRelations RelationVariable (TypeVariable, TypeVariable) Formula -- ^ Za vsako relacijo med podanima tipoma velja formula
    | ForallVariables TermVariable Type Formula                             -- ^ Za vsako spremenljivko določenega tipa velja formula
    | ForallPairs (TermVariable, TermVariable) RelationVariable Formula     -- ^ Za vsak par izrazovnih spremelnjivk, ki pripada dani relaciji, velja formula
    | IsMember (Term, Term) RelationVariable                                -- ^ Par izrazov je v dani relaciji
    | Implication Formula Formula                                           -- ^ Iz prve formule sledi druga formula
    | Equivalence Term Term                                                 -- ^ Izraza sta ekvivalentna

instance Show Formula where
    show f = toString f 0 where
        toString formul n = case formul of
            ForallRelations rv (tv1, tv2) fml -> "Forall " ++ rv ++ " between " ++ tv1 ++ " x " ++ tv2 ++ ".\n" ++ (replicate (n+2) ' ') ++ (toString fml $ n+2)
            ForallVariables tv ty fml -> "Forall " ++ tv ++ " :: " ++ (show ty) ++ ".\n" ++ (replicate (n+2) ' ') ++ (toString fml $ n+2)
            ForallPairs (tv1, tv2) rv fml -> "Forall (" ++ tv1 ++ ", " ++ tv2 ++ ") in " ++ rv ++ "." ++
                                             case fml of
                                                ForallRelations _ _ _ -> "\n" ++ (replicate (n+2) ' ') ++ (toString fml (n+2))
                                                ForallVariables _ _ _ -> "\n" ++ (replicate (n+2) ' ') ++ (toString fml (n+2))
                                                _ -> " " ++ (toString fml n)
            IsMember (term1, term2) rv -> "(" ++ (show term1) ++ ", " ++ (show term2) ++ ") in " ++ rv
            Implication fml1 fml2 -> "(" ++ (toString fml1 n) ++ ")\n" ++ (replicate n ' ') ++ "=> (" ++ (toString fml2 $ n+3) ++ ")"
            Equivalence term1 term2 -> (show term1) ++ " = " ++ (show term2)


-- | Izrek.

data Theorem = Theorem { theoremFormula :: Formula      -- ^ Formula, ki pripada izreku
                       , theoremLRs :: [ListRelation]   -- ^ Relacije, ki pripadajo vsem seznamom, ki nastopajo v izreku
                       }

instance Show Theorem where
    show theo = case (theoremLRs theo) of
        [] -> show $ theoremFormula theo
        _ -> (show $ theoremFormula theo) ++ "\n\nwhere\n\n" ++ (listToString $ theoremLRs theo)


-- * Generator spremenljivk

-- | Shramba spremenljivk za relacije.

relVarStore = ["R","S","T","U","V","Z"]


-- | Generator spremenljivk za funkcije in izrazre.

data VarGenerator = VarGenerator {funVarStore :: [String], termVarStore :: [String]}


-- | Generira par izrazovnih spremenljivk in vrne nov generator.

nextTermVar :: VarGenerator -> (String, String, VarGenerator)
nextTermVar vg = let tv = termVarStore vg in (head tv, head $ tail tv, VarGenerator {funVarStore = funVarStore vg, termVarStore = tail $ tail tv})


-- | Generira par funkcijskih spremenljivk in vrne nov generator.

nextFunVar :: VarGenerator -> (String, String, VarGenerator)
nextFunVar vg = let fv = funVarStore vg in (head fv, head $ tail fv, VarGenerator {funVarStore = tail $ tail fv, termVarStore = termVarStore vg})


-- * Funkcija za generiranje zastonjskega izreka

-- | Za dani tip z danim imenom vrne pripadajoči zastonjski izrek.

theorem :: Type -> String -> Theorem
theorem ty tyName = Theorem { theoremFormula = fold relations (fst $ formula (TermVar tyName) (TermVar tyName) ty varGenerator)
                            , theoremLRs = listrelations ty
                            } where
    
    -- Pregleda dani izraz in vrne seznam vseh spremenljivk, ki nastopajo v tipu.
    allTypeVariables' :: Type -> [TypeVariable]
    allTypeVariables' typ =
        case typ of
            TypeVar x -> [x]
            TypeConst _ -> []
            TypeList t -> allTypeVariables' t
            TypeFun a b -> nub $ concat [allTypeVariables' a, allTypeVariables' b]
    
    -- Vrne seznam vseh spremenljivk, ki nastopajo v tipu ty (podanem v funkciji theorem).
    allTypeVariables :: [TypeVariable]
    allTypeVariables = allTypeVariables' ty
    
    -- Število vseh različnih spremenljivk, ki nastopajo v tipu ty.
    n :: Int
    n = length allTypeVariables
    
    -- Za dani seznam tipovnih spremenljivk ustvari osnovne relacije, ki pripadajo tem spremenljivkam.
    relations' :: [TypeVariable] -> [Relation]
    relations' tvars =
        case tvars of
            [] -> []
            v:vs -> Relation { associatedType = v   -- relacija pripada tej tipovni spremenljivki
                             , relvar = relVarStore !! (n-j-1)    -- v shrambi poišče novo relacijsko spremenljivko za to relacijo
                             , leftType = "t" ++ (show $ 2*(n-j)-1)    -- generira spremenljivko levega in desnega osnovnega tipa,
                             , rightType = "t" ++ (show $ 2*(n-j))     -- med katerima bo ta relacija definirana
                             } : (relations' vs)
                             where j = length vs
    
    -- Seznam vseh osnovnih relacij za vhodni tip ty.
    relations :: [Relation]
    relations = relations' allTypeVariables
    
    -- Sestavi prvi del izreka - postavi kvantifikatorje za osnovne relacije, nato pa sledi podana formula.
    fold :: [Relation] -> Formula -> Formula
    fold rels frm =
        case rels of
            [] -> frm
            r:rs -> ForallRelations (relvar r) (leftType r, rightType r) $ fold rs frm
    
    -- Podani tipovni spremenljivki poišče njej pripadajočo osnovno relacijo v seznamu osnovnih relacij.
    associatedRelation' :: TypeVariable -> [Relation] -> Relation
    associatedRelation' tvar rels =
        case rels of
            r:rs -> if (associatedType r) == tvar then r else associatedRelation' tvar rs
    
    -- Podani tipovni spremenljivki poišče njej pripadajočo osnovno relacijo iz (zgoraj definiranega) seznama relations.
    associatedRelation :: TypeVariable -> Relation
    associatedRelation tvar = associatedRelation' tvar relations
    
    -- Poišče relacijsko spremenljivko, ki pripada relaciji, katera pripada podani tipovni spremenljivki.
    assocRelVar :: TypeVariable -> RelationVariable
    assocRelVar tvar = relvar $ associatedRelation tvar
    
    -- Naredi smiselno relacijsko spremenljivko za relacijo, ki pripada seznamu, t.j. tipu, generiranemu iz TypeList.
    -- To ime še nima zunanjih oklepajev [ in ]. Za ta dodatek je poskrbljeno v funkcijah formula in listrelations (spodaj).
    assocListRelVar :: Type -> RelationVariable
    assocListRelVar typ =
        case typ of
            TypeVar v -> assocRelVar v
            TypeConst c -> "id_" ++ c
            TypeList t -> "[" ++ (assocListRelVar t) ++ "]"
            TypeFun a b -> (assocListRelVar a) ++ " -> " ++ (assocListRelVar b)
    
    -- Danemu tipu pripada neka relacija, ki je sestavljena iz osnovnih relacij. Ta sestavljena relacija je definirana med
    -- dvema tipoma. Ta funkcija vrne levi tip.
    associatedFunction1 :: Type -> Type
    associatedFunction1 typ =
        case typ of
            TypeVar v -> TypeVar $ leftType $ associatedRelation v
            TypeConst c -> TypeConst c
            TypeList t -> TypeList $ associatedFunction1 t
            TypeFun a b -> TypeFun (associatedFunction1 a) (associatedFunction1 b)
    
    -- Podobno, kot prejšnja funkcija, le da ta vrne desni tip.
    associatedFunction2 :: Type -> Type
    associatedFunction2 typ =
        case typ of
            TypeVar v -> TypeVar $ rightType $ associatedRelation v
            TypeConst c -> TypeConst c
            TypeList t -> TypeList $ associatedFunction2 t
            TypeFun a b -> TypeFun (associatedFunction2 a) (associatedFunction2 b)
    
    -- Generator spremenljivk za funkcije in izraze.
    varGenerator :: VarGenerator
    varGenerator = VarGenerator { funVarStore = ["p","q","r","s","g","h","k","l","i","j","t","o"] -- Shramba spremenljivk za funkcije
                                , termVarStore = ["x","y","u","v","w","z","a","b","c","d","m","n"] -- Shramba spremenljivk za izraze
                                }
    
    -- Za podani par izrazov, ki pripada relaciji podanega tipa, vrne formulo.
    -- Pri tem se za generiranje novih spremenljivk uporabi podani generator, funkcija pa vrne še ustrezno popravljen
    -- nov generator, ki se lahko uporablja nadalje v rekurziji.
    formula :: Term -> Term -> Type -> VarGenerator -> (Formula, VarGenerator)
    formula term1 term2 typ gen =
        case typ of
            TypeVar var -> (IsMember (term1, term2) $ assocRelVar var, gen)
            TypeConst c -> (Equivalence term1 term2, gen)
            TypeList t -> (IsMember (term1, term2) $ "[" ++ (assocListRelVar t) ++ "]", gen)
            TypeFun (TypeVar avar) b ->
                let (x, y, newgen) = nextTermVar gen    -- generator poda novi izrazovni spremenljivki in se ustrezno popravi
                    (endFormula, newgen1) = formula (TermApp term1 $ TermVar x) (TermApp term2 $ TermVar y) b newgen
                in  (ForallPairs (x, y) (assocRelVar avar) endFormula, newgen1)
            TypeFun (TypeConst c) b ->
                let (x, y, newgen) = nextTermVar gen
                    (endFormula, newgen1) = formula (TermApp term1 $ TermVar x) (TermApp term2 $ TermVar x) b newgen
                in  (ForallVariables x (TypeConst c) endFormula, newgen1)
            TypeFun (TypeList t) b ->
                let (x, y, newgen) = nextTermVar gen
                    (endFormula, newgen1) = formula (TermApp term1 $ TermVar x) (TermApp term2 $ TermVar y) b newgen
                in  (ForallPairs (x, y) ("[" ++ (assocListRelVar t) ++ "]") endFormula, newgen1)
            TypeFun a b ->
                let (p, q, newgen1) = nextFunVar gen
                    (fstFormula, newgen2) = formula (TermVar p) (TermVar q) a newgen1
                    (sndFormula, newgen3) = formula (TermApp term1 $ TermVar p) (TermApp term2 $ TermVar q) b newgen2
                in  (ForallVariables p (associatedFunction1 a) $ ForallVariables q (associatedFunction2 a) $ Implication fstFormula sndFormula, newgen3)
    
    -- Generira ustrezno relacijo, ki pripada seznamu.
    listrelation :: Type -> ListRelation
    listrelation (TypeList t) =
        let (x, y, gen) = nextTermVar varGenerator
            alrv = "[" ++ assocListRelVar t ++ "]"
        in ListRelation { listrelvar = alrv
                        , headformula = fst $ formula (TermVar x) (TermVar y) t gen
                        , tailformula = IsMember ((TermVar "xs"), (TermVar "ys")) alrv
                        }
    -- Generira seznam vseh relacij, ki pripadajo seznamom v danemu tipu.
    listrelations :: Type -> [ListRelation]
    listrelations typ =
        case typ of
            TypeVar _ -> []
            TypeConst _ -> []
            TypeList t -> (listrelation typ) : (listrelations t)
            TypeFun a b -> (listrelations a) ++ (listrelations b)
