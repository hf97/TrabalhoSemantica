-- AM11

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

data Stm = Ass Var Aexp
         | Skip
         | Comp [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm

type Memory = [(Var, Int)]

-- //TODO ser se isto esta fixe
data Val = N Int | B Bool
type Stack = [Val]
type Var = String

data AM1 = PUSH Val
        | ADD
        | MULT
        | SUB
        | TRUE
        | FALSE
        | EQQ
        | LE
        | AND
        | NEG
        | FETCH Var
        | STORE Var
        | NOOP
        -- // TODO meter com code em vez de [AM1]
        | BRANCH [AM1] [AM1]
        -- | BRANCH AM1 AM1
        | LOOP AM1 AM1
        -- | BRANCH Code Code
        -- | LOOP Code Code

type Code = [AM1]
-- //TODO meter o vazio ???

update :: Var -> Val -> Memory -> Memory
update n (N a) s = [(n, a)] ++ filter (\(key, value) -> key /= n) s


get :: Var -> Memory -> [Val]
-- //TODO mudar if then else para otherwise
get n ((key,value):t) = if n == key
                        then [N value]
                        else get n t

sumValInt :: Val -> Val -> [Val]
sumValInt (N i1) (N i2) = [N i]
                        where i = i1+i2

multValInt :: Val -> Val -> [Val]
multValInt (N i1) (N i2) = [N i]
                        where i = i1*i2

subValInt :: Val -> Val -> [Val]
subValInt (N i1) (N i2) = [N i]
                        where i = i1-i2

eqValBool :: Val -> Val -> [Val]
eqValBool (N i1) (N i2) | i1 == i2 = [B True]
                       | otherwise = [B False]

leValBool :: Val -> Val -> [Val]
leValBool (N i1) (N i2) | i1<=i2 = [B True]
                       | otherwise = [B False]

andValBool :: Val -> Val -> [Val]
andValBool (B b1) (B b2) | b1==True && b2==True = [B True]
                         | otherwise = [B False]

negValBool :: Val -> [Val]
negValBool (B b1) | b1==True = [B False]
                  | otherwise = [B True]



----------------------------------A-----------------------------------


-- so :: (Code -> Stack -> State) -> (Code -> Stack -> State)
-- //TODO meter com code em vez de [AM1]
so :: ([AM1], [Val], Memory) -> ([AM1], [Val], Memory)
-- //TODO o que e suposto dar no fim ???
-- //TODO meter o vazio ???
-- //TODO meter tudo em pares com o code stack e state
so ((PUSH a):t, st, m) = (t, a : st, m)
so ((ADD):t, (h1):(h2):st, m) = (t, (sumValInt h1 h2) ++ st, m)
so ((MULT):t, h1:h2:st, m) = (t, (multValInt h1 h2) ++ st, m)
so ((SUB):t, h1:h2:st, m) = (t, (subValInt h1 h2) ++ st, m)
-- //TODO pode dar problemas com estes true false
so ((TRUE):t, st, m) = (t, [B True] ++ st, m)
so ((FALSE):t, st, m) = (t, [B False] ++ st, m)
so ((EQQ):t, h1:h2:st, m) = (t, (eqValBool h1 h2) ++ st, m)
so ((LE):t, h1:h2:st, m) = (t, (leValBool h1 h2) ++ st, m)
so ((AND):t, h1:h2:st, m) = (t, (andValBool h1 h2) ++ st, m)
so ((NEG):t, h:st, m) = (t, (negValBool h) ++ st, m)
so ((FETCH x):t, st, m) = (t, (get x m) ++ st, m)
so ((STORE x):t, h:st, m) = (t, st, update x h m)
so ((NOOP):t, st, m) = (t, st, m)
so ((BRANCH c1 c2):t, (B h):st, m) | h == True = (c1 ++ t, st, m)
                                   | otherwise = (c2 ++ t, st, m)
so ((LOOP c1 c2):t, st, m) = ([c1] ++ [BRANCH ([c2] ++ [LOOP c1 c2]) [NOOP]], st, m)


-----------------------------------------------------B-------------------------------


-- ca :: Aexp -> Code
ca :: Aexp -> [AM1]
ca (C a) = [PUSH (N a)]
ca (V x) = [FETCH x]
ca (Soma a1 a2) = (ca a2) ++ (ca a1) ++ [ADD]
ca (Mult a1 a2) = (ca a2) ++ (ca a1) ++ [MULT]
ca (Sub a1 a2) = (ca a2) ++ (ca a1) ++ [SUB]


-- -- cb :: Bexp -> Code
cb :: Bexp -> [AM1]
-- -- //TODO ver se e preciso cenas alguma cena a frente do T e do F
cb (T _) = [TRUE]
cb (F _) = [FALSE]
cb (Eq a1 a2) = (ca a2) ++ (ca a1) ++ [EQQ]
cb (Leq a1 a2) = (ca a2) ++ (ca a1) ++ [LE]
cb (Not b) = (cb b) ++ [NEG]
cb (And b1 b2) = (cb b2) ++ (cb b1) ++ [AND]


-- -- cs :: Stm -> Code
cs :: [Stm] -> [AM1]
cs [] = []
cs (h:t) = case h of Skip -> [NOOP]
                     Ass x a -> (ca a) ++ [STORE x]
                     Comp (s1:s2) -> (cs [s1]) ++ (cs s2)
                     (If b s1 s2) -> (cb b) ++ [BRANCH (cs [s1]) (cs [s2])]
                --      (While b s1) -> [LOOP (cb b) (cs [s1])]
-- cs (Ass x a) = (ca a) : (STORE x)
-- cs Skip = [NOOP]
-- cs (Comp (s1:s2)) = (cs s1) ++ (cs s2)
-- cs (If b s1 s2) = (cb b) ++ (BRANCH (cs s1) (cs s2))
-- cs (While b s1) = LOOP (cb b) (cs s1)



-------------------------------------C-----------------------



-- so apos ca,cb,cs


-------------------------------------D----------------------------