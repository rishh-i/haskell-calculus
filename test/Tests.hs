{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate", ignore "Redundant negate" #-}
import Test.Tasty ( defaultMain, TestTree, testGroup
                  , after, DependencyType(AllSucceed)
                  , adjustOption, mkTimeout, Timeout(..))
import Test.Tasty.HUnit (testCase, Assertion)
import IC.Approx ((~~>), (~~>?))
import IC.Exact ((-->))

import Prelude hiding (pi)

import Expr
import Calculus
import Data.Map qualified as Map
import Data.Set qualified as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "calculus"
  [ testGroup "sumTo" (numberedTests sumToTests)
  , testGroup "vars" (numberedTests varsTests)
  , after AllSucceed "/vars/" $
      testGroup "inTermsOf" (numberedTests inTermsOfTests)
  , testGroup "eval" (numberedTests evalTests)
  , testGroup "class" (numberedTests classTests)
  , after AllSucceed "/class/" $
      testGroup "diff" (numberedTests diffTests)
  , after AllSucceed "/diff/ || /eval/" $
      testGroup "maclaurin" (numberedTests maclaurinTests)
  , after AllSucceed "/maclaurin/" $
      testGroup "simplify expr" simplifyTests
  , after AllSucceed "/class/ || /eval/" $
      testGroup "pi" (numberedTests piTests)
  ]

sumToTests :: [Assertion]
sumToTests = [ sumTo 4 [1..] --> 10
             , sumTo 10 [1,3..] --> 100
             ]

varsTests :: [Assertion]
varsTests = [ vars e1 --> Set.fromList ["x"]
            , vars e2 --> Set.fromList ["x", "y"]
            , vars e3 --> Set.fromList ["x", "y"]
            , vars e4 --> Set.fromList ["x"]
            , vars e5 --> Set.fromList ["x"]
            , vars e6 --> Set.fromList ["x"]
            , vars (Val 5.0) --> Set.empty
            ]

inTermsOfTests :: [Assertion]
inTermsOfTests = [ inTermsOf "x" e1 --> True
                 , inTermsOf "x" e2 --> False
                 , inTermsOf "y" e3 --> False
                 , inTermsOf "x" e4 --> True
                 , inTermsOf "x" e5 --> True
                 , inTermsOf "y" e6 --> False
                 , inTermsOf "x" (Val 5.0) --> True
                 , inTermsOf "y" (Val 5.0) --> True
                 ]

evalTests :: [Assertion]
evalTests = [ eval (Map.fromList [("x",380)]) (Val 7) ~~> 7.0
            , eval (Map.fromList [("x",380), ("a",42), ("t",10)]) (Id "a")  ~~> 42.0
            , eval (Map.fromList [("t",10), ("t'",18)])
                   (Bin Add (Val (-5)) (Id "t'")) ~~> 13.0
            , eval (Map.fromList [("t",10), ("t'",19)])
                   (Pre Neg (Bin Add (Val (-5)) (Id "t'"))) ~~> -14.0
            , eval (Map.fromList [("t",10), ("t'",18.6), ("x",-55)])
                   (Bin Mul (Id "x") (Id "x")) ~~> 3025.0
            , eval (Map.fromList [("z",7)])
                   (Bin Div (Val 3) (Id "z")) ~~> 0.42857142857142855
            , eval (Map.fromList [("x",0.37)])
                   (Pre Neg (Id "x")) ~~> -0.37
            , eval Map.empty (Pre Sin (Val 2.4)) ~~> 0.675463180551151
            , eval Map.empty (Pre Cos (Val 2.4)) ~~> -0.7373937155412454
            , eval (Map.fromList [("x",0.37)]) e1 ~~> 1.85
            , eval (Map.fromList [("x",0.37), ("y", 8.2)]) e2 ~~> 1.3369
            , eval (Map.fromList [("x",0.37), ("y", 2.0)]) e3 ~~> 4.216153846153846
            , eval (Map.fromList [("x",0.37)]) e4 ~~> (-0.9323273456060345)
            , eval (Map.fromList [("x",0.37)]) e5 ~~> 0.6433720724587564
            , eval (Map.fromList [("x",0.37)]) e6 ~~> 0.8799171617597958
            ]

classTests :: [Assertion]
classTests = [ (5 :: Expr) --> (5.0 :: Expr)
             , Id "x" - 3 --> Id "x" + negate 3
             , Id "x" ^ 3 --> Id "x" * Id "x" * Id "x"
             , recip (Id "x") --> 1 / Id "x"
             , Id "x" ^^ (-3) --> 1 / (Id "x" * Id "x" * Id "x")
             ]

-- All of these tests have been written using the overloadings, to ensure that
-- rewrite steps do not affect the testing
diffTests :: [Assertion]
diffTests = [ diff "x" e1 --> 5 * 1 + 0 * Id "x"
            , diff "x" e2 --> Id "x" * 1 + 1 * Id "x" + 0 - 0
            , diff "y" e2 --> Id "x" * 0 + 0 * Id "x" + 1 - 0
            , diff "x" e4 --> negate (negate (sin (Id "x") * 1))
            , diff "x" e5 --> cos (1 + log (2 * Id "x"))
                            * (0 + (2 * 1 + 0 * Id "x") / (2 * Id "x"))
            , diff "x" e6 --> (3 * (Id "x" * 1 + 1 * Id "x")
                                + 0 * (Id "x" * Id "x") + 0)
                            / (3 * (Id "x" * Id "x") + 2)
            , diff "x" (exp (Id "x")) --> exp (Id "x")
            , diff "x" (exp (Id "x" ^ 2)) -->
                (Id "x" * 1 + 1 * Id "x") * exp (Id "x" ^ 2)
            ]

maclaurinTests :: [Assertion]
maclaurinTests = [ maclaurin (Pre Sin (Id "x")) 2 2 ~~>? Just 2.0
                 , maclaurin (Pre Sin (Id "x")) 2 3 ~~>? Just 2.0
                 , maclaurin (Pre Sin (Id "x")) 2 5 ~~>? Just 0.6666666666666667
                 , maclaurin (Pre Sin (Id "x")) 2 7 ~~>? Just 0.9333333333333333
                 , maclaurin (Pre Sin (Id "x")) 2 9 ~~>? Just 0.9079365079365079
                 , maclaurin (Pre Cos (Id "x")) 4 9 ~~>? Just (-0.39682539682539764)
                 , maclaurin (Pre Exp (Id "x")) 1 7 ~~>? Just (exp 1)
                 , maclaurin (Pre Sin (Id "y")) 1 5 ~~>? Nothing
                 ]

simplifyTests :: [TestTree]
simplifyTests = [ withTimeout 100_000 $ testCase "long series"
                    (maclaurin (Pre Sin (Id "x")) 2 30 ~~>? Just 0.909)
                ]

piTests :: [Assertion]
piTests = [ pi 1 --> 4
          , pi 10000 ~~> 3.142
          , eval Map.empty (cos (pi 25)) ~~> -1.0
          ]

-------------------------------------------------------------------------------
-- HELPERS

-- Why is this here? we _really_ want you to be using pattern matching at this
-- point.
deriving instance Eq Expr

-- allows us to use `pi :: Int -> Expr`, for funsies.
instance Enum Expr where
  toEnum = Val . fromIntegral
  fromEnum ~(Val n) = floor n

{-|
This function just matches up a bunch of assertions to a numerical naming
system, allowing us to distinguish them.

If we wanted, we could provide descriptions to them instead...
-}
numberedTests :: [Assertion] -> [TestTree]
numberedTests = zipWith (\n -> testCase ("#" ++ show n)) ([1..] :: [Integer])

withTimeout :: Integer -> TestTree -> TestTree
withTimeout n =
  adjustOption (\to -> if to == NoTimeout then mkTimeout n else to)
