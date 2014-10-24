module DifferenceLogic.SolverTests() where

import DifferenceLogic.Solver
import DifferenceLogic.TestUtils

allSolverTests = do
  testFunction consistentOverZ tSatCases

tSatCases =
  [(zF [zLit "a" "b" eq 10], True),
   (zF [zLit "a" "b" eq 10, zLit "a" "b" lt 10], False),
   (zF [zLit "a" "b" leq 20, zLit "a" "b" geq 30], False),
   (zF [zLit "a" "b" lt (-23), zLit "a" "b" gt (-22)], False),
   (zF [zLit "a" "b" lt 54, zLit "a" "b" gt 52], True),
   (zF [zLit "c" "d" eq 0, zLit "a" "c" lt 10, zLit "a" "d" gt 4], True),
   (zF [zLit "c" "d" eq 0, zLit "a" "c" lt 10, zLit "a" "d" geq 11], False)]
