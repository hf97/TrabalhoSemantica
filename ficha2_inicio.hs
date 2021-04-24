type Var = String

type State = [(Var, Int)]

data Aexp = C Int
			| V Var 
			| Soma Aexp Aexp
			| Mult Aexp Aexp
			| Sub Aexp Aexp
		deriving (Show,Eq,Ord)

data Bexp = T Bool
			| F Bool
			| Eq Aexp Aexp
			| Leq Aexp Aexp
			| Not Bexp
			| And Bexp Bexp
			| Or Bexp Bexp
		deriving (Show,Eq,Ord)

funcA :: Aexp -> State -> Int
funcA (C a1) s = a1
funcA (V x) s = get x s
funcA (Soma a1 a2) s = (funcA a1 s) + (funcA a2 s)
funcA (Mult a1 a2) s = (funcA a1 s) * (funcA a2 s)
funcA (Sub a1 a2) s = (funcA a1 s) - (funcA a2 s)

funcB :: Bexp -> State -> Bool
funcB (T a1) s = True
funcB (F a1) s = False
funcB (Eq a1 a2) s = (funcA a1 s) == (funcA a2 s)
funcB (Leq a1 a2) s = (funcA a1 s) <= (funcA a2 s)
funcB (Not a1) s = not(funcB a1 s)
funcB (And a1 a2) s = funcB a1 s && funcB a2 s
funcB (Or a1 a2) s = funcB a1 s || funcB a2 s


data Stm = Ass Var Aexp
			| Skip
			| Comp [Stm]
			| If Bexp Stm Stm
			| While Bexp Stm
		deriving (Show,Eq,Ord)

funcS :: Stm -> State -> Either Stm State
funcS (Ass x a) s = Right ((x,funcA a s):s)
funcS Skip s = Right s
funcS (Comp []) s = Right s
-- funcS (Comp (s1:s2)) s = Left (Comp (funcS s1 s):s2)

funcS (Comp (s1:s2)) s = (Comp (funcS s1 s) s2) (funcS s1 s)
funcS (Comp (s1:s2)) s = Left s2 (funcS s1 s)

-- funcS (Comp (s1:s2)) s = funcS (Comp s2) (funcS s1 s)
funcS (If b s1 s2) s = if funcB b s == True
						then Just(s1,s)
						else Just(s2,s)
funcS (While b s1) s = if funcB b s == True
					   	then funcS (Comp [s1, While b s1]) s
					   	else s


update :: Var -> Aexp -> State -> State
update v a s = [(v, z)] ++ filter (\(key, value) -> key /= v) s
						where z = funcA a s

get :: Var -> State -> Int
get v ((key,value):t) = if v == key
					  then value
					  else get v t

