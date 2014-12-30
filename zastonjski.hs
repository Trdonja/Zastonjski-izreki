-- | Program za generiranje zastonjskih izrekov za tipe

import Data.List
import Data.Maybe (fromJust)

-- | Spremenljivka, ki predstavlja tip
type TypeVariable = String

-- | Tip
data Type
	= TypeVar TypeVariable  -- ^ Spremenljivka, ki predstavlja tip
	| TypeFun Type Type     -- ^ Funkcijski konstruktor tipov ->
	deriving (Show, Eq)

-- | Spremenljivka, ki predstavlja izraz
type TermVariable = String

-- | Izraz
data Term
	= TermVar TermVariable  -- ^ Spremenljivka, ki predstavlja izraz
	| TermApp Term Term     -- ^ Aplikacija izraza na izraz
	deriving Show

-- | Spremenljivka, ki predstavlja relacijo
type RelationVariable = String

-- | Osnovna relacija med dvema tipoma
data Relation = Relation { associatedType :: TypeVariable   -- ^ spremenljivka tipa, na katero se nanaša relacija
                         , relvar :: RelationVariable       -- ^ spremenljivka, ki predstavlja relacijo
                         , leftType :: TypeVariable         -- ^ spremenljivka levega tipa
                         , rightType :: TypeVariable        -- ^ spremenljivka desnega tipa
                         } deriving Show

-- | Logična formula, ki predstavlja zastonjski izrek
data Formula
	= ForallRelations RelationVariable (TypeVariable, TypeVariable) Formula -- ^ Kvantificira relacijsko spremenljivko in dva tipa
	| ForallFunctions TermVariable Type Formula                             -- ^ Kvantificira funkcijsko spremenljivko in funkcijski tip (konstruiran iz TypeFun)
	| ForallPairs (TermVariable, TermVariable) RelationVariable Formula     -- ^ Kvantificira dve izrazovni spremenljivki
	| IsMember (Term, Term) RelationVariable                                -- ^ Par izrazov je v relaciji
	| Implication Formula Formula                                           -- ^ Iz prve formule sledi druga formula
	deriving Show

-- | Shramba spremenljivk za relacije	
relVarStore = ["R","S","T","U","V","Z"]

-- | Shramba spremenljivk za funkcije
funVarStore = ["p","q","g","h","r","s","k","l","i","j","t","o"]

-- | Shramba spremenljivk za izraze
termVarStore = ["x","y","u","v","w","z","a","b","c","d","m","n"]

-- | Za dani tip vrne pripadajoči zastonjski izrek
getFormula :: Type -> Formula
getFormula ty = fold relations (formula (TermVar "f") (TermVar "f") ty) where
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
    formula term1 term2 typ =
        case typ of
            TypeVar var -> IsMember (term1, term2) $ assocRelVar var
            TypeFun (TypeVar avar) b -> ForallPairs ("x", "y") (assocRelVar avar) $ formula (TermApp term1 $ TermVar "x") (TermApp term2 $ TermVar "y") b -- TODO: namesto x in y naj bosta generirani spremenljivki iz shrambe termVarStore
            TypeFun a b -> ForallFunctions "p" -- TODO: namesto spremenljivk p in q naj bosta generirani spremenljivki iz shrambe funVarStore
                                           (associatedFunction1 a)
                                           (ForallFunctions "q"
                                                            (associatedFunction2 a)
                                                            (Implication (formula (TermVar "p") (TermVar "q") a)
                                                                         (formula (TermApp term1 $ TermVar "p") (TermApp term2 $ TermVar "q") b)
                                                            )
                                           )

t :: Type -- primer izraza (aplikacija) ; za poiskus zaženi getFormula t
t = TypeFun ( TypeFun (TypeVar "x") (TypeVar "y") ) ( TypeFun (TypeVar "x") (TypeVar "y") )
