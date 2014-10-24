module DifferenceLogic.SolverTests() where

import DifferenceLogic.Solver
import DifferenceLogic.TestUtils

allSolverTests = do
  testFunction satisfiableOverZ tSatCases

tSatCases =
  [(zF [zLit "a" "b" eq 10], True),
   (zF [zLit "a" "b" eq 10, zLit "a" "b" lt 10], False)]
