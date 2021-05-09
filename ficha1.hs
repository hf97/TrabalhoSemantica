-- FICHA 1
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
funcB (T True) s = True
funcB (F False) s = False
funcB (Eq a1 a2) s = (funcA a1 s) == (funcA a2 s)
funcB (Leq a1 a2) s = (funcA a1 s) <= (funcA a2 s)
funcB (Not a1) s = not(funcB a1 s)
funcB (And b1 b2) s = funcB b1 s && funcB b2 s
funcB (Or b1 b2) s = funcB b1 s || funcB b2 s


data Stm = Ass Var Aexp
         | Skip
--  //TODO a lista tem de ser de Stm, Aexp ou Bexp
         | Comp [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm
        deriving (Show,Eq,Ord)

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


update :: Var -> Aexp -> State -> State
update v a s = [(v, z)] ++ filter (\(key, value) -> key /= v) s
               where z = funcA a s

get :: Var -> State -> Int
get v ((key,value):t) | v == key = value
                      | otherwise = get v t


main = do
        putStr "\nVerificar valor de variavel no estado\n"
        print (funcA (V "x") [("x",1)])
        putStr "\nAtribuição x=1: \n"
        print (evalNS (Ass "x" (C 1)) [])
        putStr "\nValidar x == y and True, no estado em que x=1 e y=1:\n"
        print (funcB (And ((Eq (V "x") (V "y"))) (T True)) [("x",1),("y",1)])
        putStr "\nComp x=1;y=3\n"
        print (evalNS (Comp [(Ass "a" (C 1)), (Ass "y" (C 3))]) [])
        putStr "\nx=1; y=3; if x<=y then x=y else skip\n"
        print (evalNS (Comp [(Ass "x" (C 1)), (Ass "y" (C 3)) , (If (Leq (V "x") (V "y")) (Ass "x" (V "y")) (Skip))]) [])
        putStr "\nx=1; y=3; while y>x do x=x+1:\n"
        print (evalNS (Comp [(While (Not(Leq (V "y") (V "x"))) (Ass "x" (Soma (V "x") (C 1))))]) [("x",1),("y",3)])
