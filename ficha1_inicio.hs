-- EXERCICIO 8

-- A

type Var = String

data State = State [(Var, Int)]

data Aexp a = Int a 
			| Var x
			| Soma (Aexp a) (Aexp a)
			| Mult (Aexp a) (Aexp a)
			| Sub (Aexp a) (Aexp a)

data Bexp b = BTrue b
			| BFalse b
			| Eq (Aexp b) (Aexp b)
			| Leq (Aexp b) (Aexp b)
			| Not (Bexp a)
			| And (Bexp b) (Bexp b)
			| Or (Bexp b) (Bexp b)

funcA :: Aexp -> Int
funcA (Aexp a1) = funcA a1
-- //TODO meter a ir ver ao estado o valor da variavel
funcA (Soma a1 a2) = funcA a1 + funcA a2
funcA (Mult a1 a2) = funcA a1 * funcA a2
funcA (Sub a1 a2) = funcA a1 - funcA a2

funcB :: Bexp -> Bool
funcB (BTrue a1) = True
funcB (BFalse a1) = False
funcB (Eq a1 a2) = funcB a1 == funcB a2
funcB (Leq a1 a2) = funcB a1 <= funcB a2
funcB (Not a1) = not(funcB a1)
funcB (And a1 a2) = funcB a1 && funcB a2
funcB (Or a1 a2) = funcB a1 || funcB a2

--B

data Stm s = Ass Var Aexp
			| Skip s
			| Comp (Stm s) (Stm s)
			| If (Bexp b) (Stm s) (Stm s)
			| While (Bexp b) (Stm s)

funcS :: Stm -> Estado
-- //TODO esta a dar erro depois do assignment, provavelmente por causa do assignment estar mal
funcS (Ass x a) = let x = funcA(a) in x
funcS Skip _ = Nothing
funcS (Comp Stm s1 Stm s2) = funcS s1 funcS s2
funcS (If Bexp b Stm s1 Stm s2) = if funcB b == True
						  		then funcS s1
						  		else funcS s2
funcS (While Bexp b Stm s) = if funcB b == True
					   	   then funcS s (While b s)
					   	   else Skip


update :: Var -> Aexp -> State -> state
update v a (State s) = State $ [(v, z)] ++ filter (\(key, value) -> key /= v) s
						where z = funcA a (State s)


update :: Stm -> (State -> State)
-- Meh
update (x a) state = state[x->a]
update skip state = state
-- Tipo isto
update Comp Stm Stm state = state1 : update Stm state1
	where state1 = state[Stm]
update If Bexp Stm1 Stm2 state = if Bexp == true
							     then state[Stm1]
							     else state[Stm2]
update While Bexp Stm state = if Bexp == true:
							  then state1 : update While Bexp Stm
							  else skip