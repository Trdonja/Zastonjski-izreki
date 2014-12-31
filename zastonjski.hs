import Data.List
import Data.Maybe (fromJust)

-- | Program za generiranje zastonjskih izrekov za tipe
-- module FreeTheorems where

-- | Spremenljivka, ki predstavlja tip
type TypeVariable = String

-- | Tip
data Type
	= TypeVar TypeVariable  -- ^ Spremenljivka, ki predstavlja tip
	| TypeFun Type Type     -- ^ Funkcijski konstruktor tipov ->

instance Show Type where
    show (TypeVar v) = v
    show (TypeFun t1 t2) = (show t1) ++ " -> " ++ (show t2)

-- | Spremenljivka, ki predstavlja izraz
type TermVariable = String

-- | Izraz
data Term
	= TermVar TermVariable  -- ^ Spremenljivka, ki predstavlja izraz
	| TermApp Term Term     -- ^ Aplikacija izraza na izraz

instance Show Term where
    show (TermVar v) = v
    show (TermApp t1 t2) = (show t1) ++ " " ++ (show t2)

-- | Spremenljivka, ki predstavlja relacijo
type RelationVariable = String

-- | Osnovna relacija med dvema tipoma
data Relation = Relation { associatedType :: TypeVariable   -- ^ spremenljivka tipa, na katero se nanaša relacija
                         , relvar :: RelationVariable       -- ^ spremenljivka, ki predstavlja relacijo
                         , leftType :: TypeVariable         -- ^ spremenljivka levega tipa
                         , rightType :: TypeVariable        -- ^ spremenljivka desnega tipa
                         }

-- | Logična formula, ki predstavlja zastonjski izrek
data Formula
	= ForallRelations RelationVariable (TypeVariable, TypeVariable) Formula -- ^ Kvantificira relacijsko spremenljivko in dva tipa
	| ForallFunctions TermVariable Type Formula                             -- ^ Kvantificira funkcijsko spremenljivko in funkcijski tip (konstruiran iz TypeFun)
	| ForallPairs (TermVariable, TermVariable) RelationVariable Formula     -- ^ Kvantificira dve izrazovni spremenljivki
	| IsMember (Term, Term) RelationVariable                                -- ^ Par izrazov je v relaciji
	| Implication Formula Formula                                           -- ^ Iz prve formule sledi druga formula

instance Show Formula where
    show (ForallRelations rv (tv1, tv2) fml) = "Forall " ++ rv ++ " between " ++ tv1 ++ " x " ++ tv2 ++ "\n" ++ (show fml)
    show (ForallFunctions tv ty fml) = "Forall " ++ tv ++ " :: " ++ (show ty) ++ "\n" ++ (show fml)
    show (ForallPairs (tv1, tv2) rv fml) = "Forall (" ++ tv1 ++ ", " ++ tv2 ++ ") in " ++ rv ++ " . " ++ (show fml)
    show (IsMember (term1, term2) rv) = "(" ++ (show term1) ++ ", " ++ (show term2) ++ ") in " ++ rv
    show (Implication fml1 fml2) = "(" ++ (show fml1) ++ ") => (" ++ (show fml2) ++ ")"

-- | Shramba spremenljivk za relacije	
relVarStore = ["R","S","T","U","V","Z"]

-- | Generator spremenljivk za funkcije in izrazre
data VarGenerator = VarGenerator {funVarStore :: [String], termVarStore :: [String]}

-- | Generira par izrazovnih spremenljivk in vrne nov generator
nextTermVar :: VarGenerator -> (String, String, VarGenerator)
nextTermVar vg = let tv = termVarStore vg in (head tv, head $ tail tv, VarGenerator {funVarStore = funVarStore vg, termVarStore = tail $ tail tv})

-- | Generira par funkcijskih spremenljivk in vrne nov generator
nextFunVar :: VarGenerator -> (String, String, VarGenerator)
nextFunVar vg = let fv = funVarStore vg in (head fv, head $ tail fv, VarGenerator {funVarStore = tail $ tail fv, termVarStore = termVarStore vg})

-- | Za dani tip vrne pripadajoči zastonjski izrek
getFormula :: Type -> Formula
getFormula ty = fold relations (fst $ formula (TermVar "f") (TermVar "f") ty varGenerator) where
    allTypeVariables' typ =
        case typ of
            TypeVar x -> [x]
            TypeFun a b -> nub $ concat [allTypeVariables' a, allTypeVariables' b]
    allTypeVariables = allTypeVariables' ty
    n = length allTypeVariables
    relations' tvars =
        case tvars of
            [] -> []
            v:vs -> Relation { associatedType = v
                             , relvar = relVarStore !! (n-j-1)
                             , leftType = "t" ++ (show $ 2*(n-j)-1)
                             , rightType = "t" ++ (show $ 2*(n-j))
                             } : (relations' vs)
                             where j = length vs
    relations = relations' allTypeVariables
    fold rels frm =
        case rels of
            [] -> frm
            r:rs -> ForallRelations (relvar r) (leftType r, rightType r) $ fold rs frm
    associatedRelation' tvar rels =
        case rels of
            r:rs -> if (associatedType r) == tvar then r else associatedRelation' tvar rs
    associatedRelation tvar = associatedRelation' tvar relations
    assocRelVar tvar = relvar $ associatedRelation tvar
    associatedFunction1 typ =
        case typ of
            TypeVar v -> TypeVar $ leftType $ associatedRelation v
            TypeFun a b -> TypeFun (associatedFunction1 a) (associatedFunction1 b)
    associatedFunction2 typ =
        case typ of
            TypeVar v -> TypeVar $ rightType $ associatedRelation v
            TypeFun a b -> TypeFun (associatedFunction2 a) (associatedFunction2 b)
    varGenerator = VarGenerator { funVarStore = ["p","q","r","s","g","h","k","l","i","j","t","o"] -- | Shramba spremenljivk za funkcije
                                , termVarStore = ["x","y","u","v","w","z","a","b","c","d","m","n"] -- | Shramba spremenljivk za izraze
                                }
    formula term1 term2 typ gen =
        case typ of
            TypeVar var -> (IsMember (term1, term2) $ assocRelVar var, gen)
            TypeFun (TypeVar avar) b ->
                let (x, y, newgen) = nextTermVar gen
                    (endFormula, newgen1) = formula (TermApp term1 $ TermVar x) (TermApp term2 $ TermVar y) b newgen
                in (ForallPairs (x, y) (assocRelVar avar) endFormula, newgen1)
            TypeFun a b ->
                let (p, q, newgen1) = nextFunVar gen
                    (fstFormula, newgen2) = formula (TermVar p) (TermVar q) a newgen1
                    (sndFormula, newgen3) = formula (TermApp term1 $ TermVar p) (TermApp term2 $ TermVar q) b newgen2
                in (ForallFunctions p (associatedFunction1 a) $ ForallFunctions q (associatedFunction2 a) $ Implication fstFormula sndFormula, newgen3)


tapp :: Type -- primer izraza (aplikacija) ; za poiskus zaženi getFormula tapp
tapp = TypeFun ( TypeFun (TypeVar "x") (TypeVar "y") ) ( TypeFun (TypeVar "x") (TypeVar "y") )

tcompright :: Type -- primer izraza (desna kompozicija)
tcompright = TypeFun ( TypeFun (TypeVar "x") (TypeVar "y") ) ( TypeFun ( TypeFun (TypeVar "y") (TypeVar "z") )  ( TypeFun (TypeVar "x") (TypeVar "z") ) )

tcompleft :: Type -- primer izraza (leva kompozicija)
tcompleft = TypeFun ( TypeFun ( TypeFun (TypeVar "x") (TypeVar "y") ) ( TypeFun (TypeVar "y") (TypeVar "z") ) ) ( TypeFun (TypeVar "x") (TypeVar "z") )