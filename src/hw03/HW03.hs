module HW03 where

import Foreign.Marshal.Utils ( fromBool )

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st key val x = if x == key then val else st x

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var key) = st key
evalE st (Val val) = val
evalE st (Op lhs bop rhs) = transBOP bop (evalE st lhs) (evalE st rhs)
  where boolCast fn = curry $ fromBool . uncurry fn
        transBOP Plus   = (+)
        transBOP Minus  = (-)
        transBOP Times  = (*)
        transBOP Divide = div
        transBOP Gt     = boolCast (>)
        transBOP Ge     = boolCast (>=)
        transBOP Lt     = boolCast (<)
        transBOP Le     = boolCast (<=)
        transBOP Eql    = boolCast (==)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign key expr) = DAssign key expr
desugar (Incr key) = DAssign key (Op (Var key) Plus (Val 1))
desugar (If cond tstmt fstmt) = DIf cond (desugar tstmt) (desugar fstmt)
desugar (While cond stmt) = DWhile cond (desugar stmt)
desugar (Sequence x xs) = DSequence (desugar x) (desugar xs)
desugar (For init cond update stmt) =
  DSequence (desugar init) (DWhile cond (DSequence (desugar stmt) (desugar update)))
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign key expr) = extend st key $ evalE st expr
evalSimple st (DIf cond ts fs) =
  if evalE st cond /= 0
    then evalSimple st ts
    else evalSimple st fs
evalSimple st w@(DWhile cond stmt) =
  if evalE st cond /= 0
    then let st' = evalSimple st stmt in seq st' (evalSimple st' w)
    else st
evalSimple st (DSequence x xs) =
  let st' = evalSimple st x in seq st' (evalSimple st' xs)
evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st $ desugar stmt

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
