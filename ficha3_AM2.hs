-- AM21

data Aexp = C Int
          | V Int
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

data Stm = Ass Int Aexp
         | Skip
         | Comp [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm

type Memory = [(Int, Int)]

-- //TODO ser se isto esta fixe
data Val = N Int | B Bool
type Stack = [Val]

data AM2 = PUSH Int
        | ADD
        | MULT
        | SUB
        | TRUE
        | FALSE
        | EQQ
        | LE
        | AND
        | NEG
        | GET Int
        | PUT Int
        | NOOP
        | LABEL
        | JUMP
        | JUMPFALSE
        -- // TODO meter com code em vez de [AM2]
        -- | BRANCH [AM2] [AM2]
        -- | LOOP [AM2] [AM2]
        -- | BRANCH Code Code
        -- | LOOP Code Code

-- type Code = [AM2]
-- //TODO meter o vazio ???

update :: Int -> Int -> Memory -> Memory
update n a s = [(n, a)] ++ filter (\(key, value) -> key /= n) s

get :: Int -> Memory -> Int
-- //TODO mudar if then else para otherwise
get n ((key,value):t) = if n == key
                        then value
                        else get n t


----------------------------------E-----------------------------------


-- so :: (Code -> Stack -> State) -> (Code -> Stack -> State)
-- //TODO meter com code em vez de [AM2]
so :: ( -> [AM2] -> Stack -> Memory) -> ( -> [AM2] -> Stack -> Memory)
-- //TODO o que e suposto dar no fim ???
-- //TODO meter o vazio ???
-- //TODO meter tudo em pares com o code stack e state
so (n, (PUSH a):t, st, m) = (t, a : st, m)
so (n, (ADD):t, h1:h2:st, m) = (t, (h1+h2) : st, m)
so (n, (MULT):t, h1:h2:st, m) = (t, (h1*h2) : st, m)
so (n, (SUB):t, h1:h2:st, m) = (t, (h1-h2) : st, m)
so (n, (TRUE):t, st, m) = (t, TRUE : st, m)
so (n, (FALSE):t, st, m) = (t, FALSE : st, m)
so (n, (EQQ):t, h1:h2:st, m) = (t, (h1==h2) : st, m)
so (n, (LE):t, h1:h2:st, m) = (t, (h1<=h2) : st, m)
so (n, (AND):t, h1:h2:st, m) = if h1 == True && h2 == True
                        then (t, True : st, m)
                        else (t, False : st, m)
so (n, (NEG):t, h:st, m) = if h == True
                    then (t, False : st, m)
                    else (t, True : st, m)
so (n, (GET x):t, st, m) = (t, (get x) : st, m)
so (n, (PUT x):t, h:st, m) = (t, st, update x h m)
so (n, (NOOP):t, st, m) = (t, st, m)
so (n, (LABEL l):t, st, m) = (n+1, t, st, m)
so JUMP
so JUMPFALSE
-- so ((BRANCH c1 c2):t, h:st, m) = if h == True
--                              then (c1 : t, st, m)
--                              else (c2 : t, st, m)
-- so ((LOOP c1 c2):t, st, m) = (c1 : (BRANCH (c2 : LOOP c1 c2) NOOP), st, m)


-----------------------------------------------------F-------------------------------


ca :: Aexp -> Code
ca (C a) = [PUSH a]
ca (V x) = [GET x]
ca (Soma a1 a2) = [(ca a2) : (ca a1) : ADD]
ca (Mult a1 a2) = [(ca a2) : (ca a1) : MULT]
ca (Sub a1 a2) = [(ca a2) : (ca a1) : SUB]


cb :: Bexp -> Code
-- //TODO ver se e preciso cenas alguma cena a frente do T e do F
cb (T _) = [True]
cb (F _) = [False]
cb (Eq a1 a2) = [(cb a2) : (cb a1) : EQQ]
cb (LE a1 a2) = [(cb a2) : (cb a1) : LE]
cb (Not b) = [not(cb b) : NEG]
cb (And b1 b2) = [(cb b2) : (cb b1) : AND]


cs :: Stm -> Code
cs (Ass x a) = [(ca a) : (PUT x)]
cs Skip = [NOOP]
cs (Comp (s1:s2)) = [(cs s1) : (cs s2)]
CS (If b s1 s2) = [(cb b) : BRANCH((cs s1), (cs s2))]
CS (While b s1) = [LOOP((cb b), (cs s1))]


-------------------------------------G-----------------------



so apos ca,cb,cs


-------------------------------------D----------------------------