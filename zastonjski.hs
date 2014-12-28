import Data.List
import Data.Maybe (fromJust)

type TypeVariable = String

data Type
	= TypeVar TypeVariable
	| TypeFun Type Type
	deriving (Show, Eq)

type TermVariable = String
	
data Term
	= TermVar TermVariable
	| TermApp Term Term
	deriving Show


type RelationVariable = String

data Relation
	= RelVar RelationVariable Type Type
	| RelFun Relation Relation
	deriving Show

	
data Formula
	= ForallRelations Relation Formula
	| ForallPairs (TermVariable, TermVariable) Relation Formula
	| IsMember (Term, Term) Relation
	| Equivalence Formula Formula
	| Implication Formula Formula
	deriving Show


createBasicRelations :: Int -> [Relation]
createBasicRelations n = zipWith3 RelVar (take n ["R","S","T","U","V","Z"]) (take n (map TypeVar $ map ("t" ++) $ map (\x -> show x) [1,3..])) (take n (map TypeVar $ map ("t" ++) $ map (\x -> show x) [2,4..]))

getTypeVariables :: Type -> [TypeVariable]
getTypeVariables (TypeVar x) = [x]
getTypeVariables (TypeFun a b) = nub $ concat [getTypeVariables a, getTypeVariables b]

createRelation :: Type -> Relation
createRelation ty =
    let vars = getTypeVariables ty
        rels = createBasicRelations $ length vars
    in case ty of
        TypeVar tyvar -> rels !! (fromJust $ elemIndex tyvar vars)
        TypeFun ty1 ty2 -> RelFun (createRelation ty1) (createRelation ty2)

getFormula :: Type -> Formula
getFormula ty = foldr ForallRelations (IsMember (TermVar "f", TermVar "f") (createRelation ty)) (createBasicRelations (length $ getTypeVariables ty))

t :: Type
t = TypeFun ( TypeFun (TypeVar "x") (TypeVar "y") ) ( TypeFun (TypeVar "x") (TypeVar "y") )
