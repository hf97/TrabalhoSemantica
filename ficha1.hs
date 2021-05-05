-- EXERCICIO 8

-- A

type Var = String

type State = [(Var, Int)]

-- //TODO ver se e preciso meter deriving (Show, Eq, Ord)

data Aexp = C Int
          | V Var 
          | Soma Aexp Aexp
          | Mult Aexp Aexp
          | Sub Aexp Aexp

data Bexp = T Bool
          | F Bool
          | Eq Aexp Aexp
          | Leq Aexp Aexp
          | Not Bexp
          | And Bexp Bexp
          | Or Bexp Bexp

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
funcB (And b1 b2) s = funcB b1 s && funcB b2 s
funcB (Or b1 b2) s = funcB b1 s || funcB b2 s

--B

data Stm = Ass Var Aexp
         | Skip
         | Comp [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm

funcS :: Stm -> State -> State
-- //TODO ver se este ass esta bem
funcS (Ass x a) s = (x,funcA a s):s
-- //TODO ver se e preciso ter alguma cena a frente do skip
funcS Skip s = s
funcS (Comp []) s = s
funcS (Comp (s1:s2)) s = funcS (Comp s2) (funcS s1 s) 
-- //TODO mudar if then else para otherwise
funcS (If b s1 s2) s = if funcB b s == True
                       then funcS s1 s
                       else funcS s2 s
funcS (While b s1) s = if funcB b s == True
                       then funcS (Comp [s1, While b s1]) s
                       else s


update :: Var -> Aexp -> State -> State
update v a s = [(v, z)] ++ filter (\(key, value) -> key /= v) s
               where z = funcA a s

get :: Var -> State -> Int
-- //TODO mudar if then else para otherwise
get v ((key,value):t) = if v == key
                        then value
                        else get v t

-- //TODO nao sei se esta fixe ao usar funcA nos statements

main