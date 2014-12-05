newtype Par = Par String deriving Show

data Type =
	Par Parameter |
	Arrow Type Type
	deriving Show

data PolyType = Forall [Parameter] Type