
type Var = String

type State = [(Var, Int)]

-- type Prog = [Stm State]
type Prog = [Stm]

-- //TODO ver se e preciso meter deriving (Show, Eq, Ord)

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
-- //TODO mudar if then else para otherwise
get v ((key,value):t) = if v == key
                        then value
                        else get v t

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
                            Skip -> ([],s)
                            -- comp
                            (If b a1 a2) | (funcB b s) == True -> ([a1],s)
                                         | otherwise -> ([a2],s) 
                            (While b a) -> ([If b a Skip], s)


-- ntepsos usar stepSOS recursivamente e assignment e caso base

-- nstepSOS :: (Prog, State) -> State
-- nstepSOS (C,s) = stepSOS

-- stepSOS ((While (T True) (Ass "x" (C 1))):[]) []