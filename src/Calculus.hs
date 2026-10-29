module Calculus (vars, inTermsOf, eval, pretty, diff, maclaurin, pi, sumTo) where

import Prelude hiding (pi)

import Expr

import Data.Map (Map, (!))
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

type Env = Map String Double

---------------------------------------------------------------------------
-- Type classes and class instances

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger n = Val (fromInteger n)
  negate      :: Expr -> Expr
  negate (Val 0) = Val 0
  negate exp = Pre Neg exp
  (+), (*)    :: Expr -> Expr -> Expr
  (+) e (Val 0) = e
  (+) (Val 0) e = e
  (+) exp1 exp2        = Bin Add exp1 exp2
  (*) e (Val 1) = e
  (*) (Val 1) e = e
  (*) e (Val 0) = Val 0
  (*) (Val 0) e = Val 0
  (*) exp1 exp2        = Bin Mul exp1 exp2

instance Fractional Expr where
  fromRational :: Rational -> Expr
  fromRational n = Val (fromRational n) 
  (/)          :: Expr -> Expr -> Expr

  (/) (Val 0) e = Val 0
  (/) e (Val 1) = e
  (/) exp1 exp2 = Bin Div exp1 exp2

instance Floating Expr where
  sin, cos, log, exp :: Expr -> Expr
  sin = Pre Sin
  cos = Pre Cos
  log = Pre Log
  exp = Pre Exp

---------------------------------------------------------------------------

{-|
Computes the sum of first `n` terms of the given series.
-}
sumTo :: Num a => Int -> [a] -> a
sumTo 0 _ = 0
sumTo _ [] = 0
sumTo n (x:xs) = x + sumTo (n-1) xs

{-|
Computes the set of variable names that appear within the given expression.
-}
vars :: Expr -> Set String
vars (Id vr) = Set.singleton vr
vars (Bin _ left right) = Set.union (vars left) (vars right)
vars (Pre _ ex) = vars ex
vars _ = Set.empty

{-|
Does the given expression only use the given variable and no others?
-}
inTermsOf :: String -> Expr -> Bool
inTermsOf str exp = (Set.singleton str) == vars exp || Set.null (vars exp)

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}

binMap :: Map BinOp (Double -> Double -> Double)
binMap = Map.fromList [(Add, (+)), (Mul, (*)), (Div, (/))]

preMap :: Map PreOp (Double -> Double)
preMap = Map.fromList [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log), (Exp, exp)]

-- type Env = Map String Double
eval :: Env -> Expr -> Double
eval _ (Val x) = x
eval dict (Id var) = dict ! var
eval dict (Bin op exp1 exp2) = (binMap ! op) (eval dict exp1) (eval dict exp2)  
eval dict (Pre op ex) = (preMap ! op) (eval dict ex)

{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}

binMapSymbol :: Map BinOp String
binMapSymbol = Map.fromList [(Add, "+"), (Mul, "*"), (Div, "/")]

preMapSymbol :: Map PreOp String
preMapSymbol = Map.fromList [(Neg, "-"), (Sin, "sin"), (Cos, "cos"), (Log, "log"), (Exp, "exp")]

pretty :: Expr -> String
pretty (Val x) = show x
pretty (Id var) = var
pretty (Bin op exp1 exp2) = "(" ++ pretty (exp1) ++ (binMapSymbol ! op) ++ pretty (exp2) ++ ")"
pretty (Pre op ex) = (preMapSymbol ! op) ++ "(" ++ pretty (ex) ++ ")"

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: String -> Expr -> Expr
diff x (Val y) = 0
diff x (Id y)
 | x == y = 1
 | otherwise = 0
diff x (Bin Add e1 e2) = (diff x e1) + (diff x e2)
diff x (Bin Mul e1 e2) = (e1 * (diff x e2)) + ((diff x e1) * e2)
diff x (Bin Div e1 e2) =  a/b
 where
  a = (diff x e1)*e2 - (e1*(diff x e2))
  b = e2 * e2
diff x (Pre Neg e) = negate (diff x e)
diff x (Pre Sin e) = (cos e) * (diff x e)
diff x (Pre Cos e) = -(sin e) * (diff x e)
diff x (Pre Log e) = (1/e) * (diff x e)
diff x (Pre Exp e) = (exp e) * (diff x e)

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Maybe Double -- ^ the approximate result
maclaurin expr val n
 | not (inTermsOf "x" expr) = Nothing
 | otherwise = Just (foldr (+) 0 final)
  where 
    dif = take n (iterate (diff "x") expr)
    env = Map.fromList [("x", 0.0)]
    deriv = map (eval env) dif
    coeffs = [val^^(k)/fromIntegral (fact k) | k <- [0..(n-1)]]
    final = zipWith (*) deriv coeffs
    fact :: Int -> Integer
    fact 0 = 1
    fact k = product [1..fromIntegral k]

-- Extension
instance Num a => Num [a] where
  fromInteger  :: Integer -> [a]
  fromInteger  = undefined
  negate       :: [a] -> [a]
  negate       = undefined
  (+), (*)     :: [a] -> [a] -> [a]
  (+)          = undefined
  (*)          = undefined

instance Fractional a => Fractional [a] where
  fromRational :: Rational -> [a]
  fromRational = undefined
  (/)          :: [a] -> [a] -> [a]
  (/)          = undefined

instance Floating a => Floating [a] where
  (**)         :: [a] -> [a] -> [a]
  (**)         = undefined

pi :: (Enum a, Fractional a) => Int -> a
pi = undefined
