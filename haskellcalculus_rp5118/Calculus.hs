module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]



lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key ((k, v) : table)
  | key == k  = v
  | otherwise = lookUp key table

eval :: Exp -> Env -> Double
eval (Val x) _ 
  = x
eval (Id x) values
  = lookUp x values
eval (UnApp func exp) values
  | func == Sin = sin (eval exp values)
  | func == Cos = cos (eval exp values)
  | func == Neg = (-1) * (eval exp values)
  | func == Log = log (eval exp values)
eval (BinApp func exp1 exp2) values
  | func == Add = (+) (eval exp1 values) (eval exp2 values)
  | func == Mul = (*) (eval exp1 values) (eval exp2 values)
  | func == Div = (/) (eval exp1 values) (eval exp2 values)

diff :: Exp -> String -> Exp
diff (Val x) var
  = Val 0.0
diff (Id y) var
  | y == var  = Val 1.0
  | otherwise = Val 0.0
diff (UnApp func exp) var
  | func == Neg = UnApp Neg (diff exp var)
  | func == Sin = BinApp Mul (UnApp Cos exp) (diff exp var)
  | func == Cos = UnApp Neg (BinApp Mul (UnApp Sin exp) (diff exp var))
  | func == Log = BinApp Div (diff exp var) exp
diff (BinApp func exp1 exp2) var
  | func == Add = BinApp Add (diff exp1 var) (diff exp2 var)
  | func == Mul = BinApp Add (BinApp Mul exp1 (diff exp2 var)) (BinApp Mul (diff exp1 var) exp2)
  | func == Div = BinApp Div (BinApp Add (BinApp Mul (diff exp1 var) exp2) (UnApp Neg (BinApp Mul exp1 (diff exp2 var)))) (BinApp Mul exp2 exp2)

maclaurin :: Exp -> Double -> Int -> Double
maclaurin func x term
  = sum (zipWith (/) (zipWith (*) diff' pow) fact)
  where
    diff' = take term (map (flip eval [("x", 0)]) (iterate (flip diff "x") func))
    pow   = scanl (*) 1 (take term (repeat x)) 
    fact  = map fromIntegral (scanl (*) 1 [1..term])

showExp :: Exp -> String
showExp (Val n)
  = show n
showExp (Id n)
  = n
showExp (UnApp Neg exp)
  = "-(" ++ showExp exp ++ ")"
showExp (UnApp Sin exp)
  = "sin(" ++ showExp exp ++ ")"
showExp (UnApp Cos exp)
  = "cos(" ++ showExp exp ++ ")"
showExp (UnApp Log exp)
  = "log(" ++ showExp exp ++ ")"
showExp (BinApp Add exp1 exp2)
  = "(" ++ showExp exp1 ++ "+" ++ showExp exp2 ++ ")"
showExp (BinApp Mul exp1 exp2)
  = "(" ++ showExp exp1 ++ "*" ++ showExp exp2 ++ ")"
showExp (BinApp Div exp1 exp2)
  = "(" ++ showExp exp1 ++ "/" ++ showExp exp2 ++ ")"
 

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where


-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
