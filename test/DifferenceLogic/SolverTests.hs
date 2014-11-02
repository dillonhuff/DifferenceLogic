module DifferenceLogic.SolverTests() where

import Data.Set as S

import DifferenceLogic.Solver
import DifferenceLogic.TestUtils
import FirstOrderTheory.Syntax
import FirstOrderTheory.Theory

allSolverTests = do
  testFunction decideSatisfiability tSatCases
  testFunction (fst . decideSat integerDifferenceLogic) theorySatCases

tSatCases =
  [(zF [zLit "a" "b" eq 10], True),
   (zF [zLit "a" "b" eq 10, zLit "a" "b" lt 10], False),
   (zF [zLit "a" "b" leq 20, zLit "a" "b" geq 30], False),
   (zF [zLit "a" "b" lt (-23), zLit "a" "b" gt (-22)], False),
   (zF [zLit "a" "b" lt 54, zLit "a" "b" gt 52], True),
   (zF [zLit "c" "d" eq 0, zLit "a" "c" lt 10, zLit "a" "d" gt 4], True),
   (zF [zLit "c" "d" eq 0, zLit "a" "c" lt 10, zLit "a" "d" geq 11], False)]

theorySatCases =
  [(S.fromList [lit $ atom "=" [func "-" [var "a", var "b"], intConst 2]], True),
   (S.fromList [lit $ atom ">" [func "-" [var "a", var "b"], intConst (-12)],
                lit $ atom "<" [func "-" [var "a", var "b"], intConst (-23)]], False),
   (S.fromList [lit $ atom ">=" [func "-" [var "a", var "b"], intConst (-12)],
                lit $ atom "<=" [func "-" [var "a", var "b"], intConst 70],
                lit $ atom "=" [func "-" [var "a", var "c"], intConst 0]], True)]

