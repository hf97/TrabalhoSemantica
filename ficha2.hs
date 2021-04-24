
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

data Stm = Ass Var Aexp
         | Skip
         | Comp [Stm]
         | If Bexp Stm Stm
         | While Bexp Stm

