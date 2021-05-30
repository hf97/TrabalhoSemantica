-- FICHA3 AM2

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

data Val = N Int | B Bool
type Stack = [Val]
type Pc = Int
type Var = String

data AM2 = PUSH Val
        | LABEL Int
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
        | JUMP
        | JUMPFALSE


update :: Var -> Val -> Memory -> Memory
update n (N a) s = [(n, a)] ++ filter (\(key, value) -> key /= n) s


get :: Var -> Memory -> [Val]
get n ((key,value):t) | n == key = [N value]
                      | get n t


----------------------------------E-----------------------------------


-- so :: (Code -> Stack -> State) -> (Code -> Stack -> State)
-- //TODO meter com code em vez de [AM2]
so :: (Pc, [AM2], [Val], Memory) -> (Pc, [AM2], [Val], Memory)
so (i, (LABEL l):t, st, m) = (i+l, t, st, m)


-----------------------------------------------------B-------------------------------


-- ca :: Aexp -> Code
ca :: Aexp -> [AM2]
ca (C a) = [PUSH (N a)]
ca (V x) = [FETCH x]
ca (Soma a1 a2) = (ca a2) ++ (ca a1) ++ [ADD]
ca (Mult a1 a2) = (ca a2) ++ (ca a1) ++ [MULT]
ca (Sub a1 a2) = (ca a2) ++ (ca a1) ++ [SUB]


-- -- cb :: Bexp -> Code
cb :: Bexp -> [AM2]
cb (T _) = [TRUE]
cb (F _) = [FALSE]
cb (Eq a1 a2) = (ca a2) ++ (ca a1) ++ [EQQ]
cb (Leq a1 a2) = (ca a2) ++ (ca a1) ++ [LE]
cb (Not b) = (cb b) ++ [NEG]
cb (And b1 b2) = (cb b2) ++ (cb b1) ++ [AND]


-- -- cs :: Stm -> Code
cs :: [Stm] -> [AM2]
cs [] = []
cs (h:t) = case h of Skip -> [NOOP]
                     Ass x a -> (ca a) ++ [STORE x]
                     Comp (s1:s2) -> (cs [s1]) ++ (cs s2)
                     (If b s1 s2) -> case (cb b) of [TRUE] -> [LABEL 0]
                                                    [FALSE] -> [LABEL 1]
                     (While b s1) -> case (cb b) of [TRUE] -> [LABEL 0]
                                                    [FALSE] -> [LABEL 1]


-------------------------------------G-----------------------





-------------------------------------H----------------------------
