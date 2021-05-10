-- FICHA 2

type Var = String

type State = [(Var, Int)]

type Prog = [Stm]

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

data Stm = Ass Var Aexp
         | Skip
         | Comp [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm
        deriving (Show,Eq,Ord)


update :: Var -> Aexp -> State -> State
update v a s = [(v, z)] ++ filter (\(key, value) -> key /= v) s
               where z = funcA a s

get :: Var -> State -> Int
get v ((key,value):t) | v == key = value
                      | otherwise = get v t

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

evalNS :: Stm -> State -> State
-- evalNS (Ass x a) s = (x,funcA a s):s
evalNS (Ass x a) s = update x a s
evalNS Skip s = s
evalNS (Comp []) s = s
evalNS (Comp (s1:s2)) s = evalNS (Comp s2) (evalNS s1 s) 
evalNS (If b s1 s2) s | funcB b s == True = evalNS s1 s
                      | otherwise = evalNS s2 s
evalNS (While b s1) s | funcB b s == True = evalNS (Comp [s1, While b s1]) s
                      | otherwise = s


-- //TODO Penso que tenho de passa o primeiro tipo para [Stm]
-- stepSOS :: Stm -> State -> (Prog, State)
-- stepSOS (Ass x a) s = ([], update x a s)
-- stepSOS Skip s = ([], s)
-- -- stepSOS Comp1
-- -- stepSOS Comp2
-- stepSOS (Comp ((Ass x a):a2)) s = stepSOS (Ass x a) s
-- -- stepSOS (Comp (a1:a2)) s = stepSOS (Comp (Step))
-- stepSOS (If b a1 a2) s | (funcB b s) == True = ([a1],s)
--                        | otherwise = ([a2],s)
-- stepSOS (While b a) s = ([If b a Skip], s)


stepSOS :: [Stm] -> State -> (Prog, State)
stepSOS (h:t) s = case h of (Ass x a) -> ([]++t, update x a s)
                            Skip -> ([]++t,s)
                            -- comp
                            (If b a1 a2) | (funcB b s) == True -> ([a1]++t,s)
                                         | otherwise -> ([a2]++t,s) 
                            (While b a) -> ([If b a Skip]++t, s)


-- //TODO testar
nstepSOS :: (Prog, State) -> Int -> (Prog, State)
nstepSOS ((h:t), s) n | n>1 = nstepSOS (stepSOS [h] s) (n-1)
                      | otherwise = ((h:t),s)


-- //TODO testar
evalSOS :: (Prog, State) -> State
evalSOS ([], s) = s
evalSOS ((h:t), s) = evalSOS (stepSOS [h] s)


main = do
        putStr "\nExemplos para 1 passo:\n"
        print (stepSOS ([Ass "x" (C 1)]) [])
        print (stepSOS ([Skip, Ass "x" (C 1)]) [])
        print (stepSOS ([If (F False) (Ass "x" (C 1)) (Ass "y" (C 2))]) [])
        print (stepSOS ([While (T True) (Ass "x" (Soma (V "x") (C 1)))]) [("x",1)])