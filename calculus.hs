module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key list
  = fromJust (lookup key list)

showExp :: Exp -> String
showExp (Val e)
  = show e
showExp (Id e)
  = show e
showExp (UnApp op e)
  = lookUp op unTable ++ "(" ++ showExp e ++ ")"
  where
    unTable = [(Neg, "-"), (Sin, "sin"), (Cos, "cos"), (Log, "log")]
showExp (BinApp op e e')
  = "(" ++ showExp e ++ lookUp op binTable ++ showExp e' ++ ")"
  where
    binTable = [(Add, "+"), (Mul, "*"), (Div, "/")]

eval :: Exp -> Env -> Double
-- Number
eval (Val e) env
  = e
-- Identifier
eval (Id e) env
  = lookUp e env
-- Unary operation
eval (UnApp op e) env
  = lookUp op unTable (eval e env)
  where
    unTable = [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log)]
-- Binary operation
eval (BinApp op e e') env
  = lookUp op binTable (eval e env) (eval e' env)
  where
    binTable = [(Add, (+)), (Mul, (*)), (Div, (/))]

diff :: Exp -> String -> Exp
-- Number
diff (Val a) _
  = Val 0.0
-- Identifier
diff (Id e) x
  | e == x    = Val 1.0
  | otherwise = Val 0.0
-- Negative expression
diff (UnApp Neg e) x
  = UnApp Neg (diff e x)
-- Binary operations (Add, Mul, Div)
diff (BinApp Add e e') x
  = BinApp Add (chain e x) (chain e' x)
diff (BinApp Mul e e') x
  = BinApp Add (BinApp Mul e (chain e' x)) (BinApp Mul (chain e x) e')
diff (BinApp Div e e') x
  = BinApp Div num denom
  where
    num   = BinApp Add (BinApp Mul (chain e x) e') (UnApp Neg (BinApp Mul e (chain e' x)))
    denom = BinApp Mul e' e'
-- Unary operations (Sin, Cos, Log)
diff (UnApp Sin e) x
  | x == show e  = UnApp Cos e
  | otherwise    = chain (UnApp Sin e) x
diff (UnApp Cos e) x
  | x == show e  = UnApp Sin e
  | otherwise    = chain (UnApp Cos e) x
diff (UnApp Log e) x
  | x == show e  = BinApp Div (Val 1.0) e
  | otherwise    = chain (UnApp Log e) x

-- CHAIN RULE FUNCTION
chain :: Exp -> String -> Exp
chain (UnApp Neg e) x
  = diff (UnApp Neg e) x
chain (UnApp Sin e) x
  = BinApp Mul (diff (UnApp Sin e) (show e)) (diff e x)
chain (UnApp Cos e) x
  = UnApp Neg (BinApp Mul (diff (UnApp Cos e) (show e)) (diff e x))
chain (UnApp Log e) x
  = BinApp Div (diff e x) e
chain e x
  = diff e x

maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp val n
  -- ds -> derivatives = [exp, exp', exp'' ..]
  -- ps -> powers      = [1, x, (x * x) ..]
  -- fs -> factorials  = [1, 1, 2 ..]
  = sum (take n (map (`eval` [("x", val)]) (zipWith3 createTerm ds ps fs)))
  where
    ds = iterate (`diff` "x") exp 
    ps = Val 1 : iterate (BinApp Mul (Id "x")) (Id "x")
    fs = scanl (*) 1 [1, 2 ..]
    createTerm d p f
      = BinApp Div (BinApp Mul (Val (eval d [("x", 0.0)])) p) (Val f)

--------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))