module DifferenceLogic.Solver(
  ZFormula, ZLiteral,
  integerDifferenceLogic,
  zF,
  zLit,
  eq, gt, lt, geq, leq,
  decideSatisfiability) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

import FirstOrderTheory.Syntax
import FirstOrderTheory.Theory as T
import FirstOrderTheory.Utils as U

-- | FirstOrderTheory implementation for integer difference logic
data IntegerDifferenceLogic = IDF

integerDifferenceLogic = IDF

instance FirstOrderTheory IntegerDifferenceLogic where
  theoryName t = "IntegerDifferenceLogic"
  sorts t = S.singleton $ U.sort "Integer"
  predicates t = S.fromList [eqPred, leqPred, geqPred, ltPred, gtPred]
  functions t = S.singleton minusFunc
  decideSat t lits = case decideSatisfiability $ zF $ S.toList $ S.map toZLiteral lits of
    True -> (True, S.empty)
    False -> (False, lits)

minusFunc = functionDecl "-" 2 [U.sort "Integer", U.sort "Integer"] (U.sort "Integer")

eqPred = predicateDecl "=" 2 [U.sort "Integer", U.sort "Integer"]
leqPred = predicateDecl "<=" 2 [U.sort "Integer", U.sort "Integer"]
geqPred = predicateDecl ">=" 2 [U.sort "Integer", U.sort "Integer"]
ltPred = predicateDecl "<" 2 [U.sort "Integer", U.sort "Integer"]
gtPred = predicateDecl ">" 2 [U.sort "Integer", U.sort "Integer"]

-- |A conjunction of literals in integer difference logic
data ZFormula = ZFormula (Set ZLiteral)
                deriving (Eq, Ord, Show)

-- | Make a conjuction of literals in integer difference logic
zF :: [ZLiteral] -> ZFormula
zF litList = ZFormula $ S.fromList litList

-- |Literal of the form 'a - b <predicate> <integer constant>
data ZLiteral = ZLiteral {
  left :: String,
  right :: String,
  predicate :: Predicate,
  const :: Int
  } deriving (Eq, Ord, Show)

-- |'zLit a b <pred> c' returns the literal 'a - b <pred> c'
zLit = ZLiteral

toZLiteral :: Literal -> ZLiteral
toZLiteral l = case isNeg l of
  True -> negateZLit $ toZLit predL
  False -> toZLit predL
  where
    predL = getAtom l

toZLit :: Atom -> ZLiteral
toZLit a = case predicateName a of
  "=" -> ZLiteral leftVar rightVar Eq constVal
  ">" -> ZLiteral leftVar rightVar Gt constVal
  "<" -> ZLiteral leftVar rightVar Lt constVal
  "<=" -> ZLiteral leftVar rightVar Leq constVal
  ">=" -> ZLiteral leftVar rightVar Geq constVal
  other -> error $ "Predicate " ++ other ++ " is not supported"
  where
    args = extractArgs a
    leftVar = varName $ args !! 0
    rightVar = varName $ args !! 1
    constVal = intVal $ args !! 2

extractArgs :: Atom -> [Term]
extractArgs a = case length pArgs /= 2 of
  True -> error $ "Too many arguments to " ++ show a
  False -> vars ++ [const]
  where
    pArgs = atomArgs a
    vars = extractVars pArgs
    const = head $ tail pArgs

extractVars :: [Term] -> [Term]
extractVars (x:y:[]) = case isFunctionWithName "-" x of
  True -> funcArgs x
  False -> error $ show x ++ " is not a valid function in integer difference logic"

negateZLit :: ZLiteral -> ZLiteral
negateZLit (ZLiteral l r Lt c) = ZLiteral l r Geq c
negateZLit (ZLiteral l r Gt c) = ZLiteral l r Leq c
negateZLit (ZLiteral l r Geq c) = ZLiteral l r Lt c
negateZLit (ZLiteral l r Leq c) = ZLiteral l r Gt c
negateZLit eqLit@(ZLiteral l r Eq c) =
  error $ "Error: Cannot negate " ++ show eqLit ++ ", this solver does not currently support disequality"

normalizeLiteral :: ZLiteral -> [ZLiteral]
normalizeLiteral (ZLiteral l r Eq c) = [zLit l r leq c, zLit r l leq (-c)]
normalizeLiteral (ZLiteral l r Lt c) = [zLit l r leq (c - 1)]
normalizeLiteral (ZLiteral l r Gt c) = [zLit r l leq (-(c - 1))]
normalizeLiteral (ZLiteral l r Geq c) = [zLit r l leq (-c)]
normalizeLiteral l = [l]

literalNames :: ZLiteral -> [String]
literalNames (ZLiteral l r _ _) = [l, r]

buildLiteralEdge :: Map String Node -> ZLiteral -> LEdge Int
buildLiteralEdge namesToNodes (ZLiteral l r Leq c) = (lNode, rNode, c)
  where
    lNode = fromJust $ M.lookup l namesToNodes
    rNode = fromJust $ M.lookup r namesToNodes
buildLiteralEdge _ l = error $ "Trying to build literal from " ++ show l

data Predicate
  = Eq
  | Lt
  | Gt
  | Leq
  | Geq
    deriving (Eq, Ord, Show)

-- |Predicate '='
eq = Eq
-- |Predicate '<'
lt = Lt
-- |Predicate '>'
gt = Gt
-- |Predicate '<='
leq = Leq
-- |Predicate '>='
geq = Geq

-- Solver for conjunctions of literals

decideSatisfiability :: ZFormula -> Bool
decideSatisfiability formula = not $ containsNegCycle formulaGraph
  where
    normedForm = normalizeFormula formula
    nodeList = buildNodeList normedForm
    litNameNodeMap = M.fromList $ L.map (\(n, s) -> (s, n)) nodeList
    edgeList = buildEdgeList normedForm litNameNodeMap
    formulaGraph = mkGraph nodeList edgeList :: Gr String Int

buildNodeList :: ZFormula -> [(Node, String)]
buildNodeList (ZFormula lits) = L.zip [1..(length litNames)] litNames
  where
    litNames = L.nub $ L.concat $ L.map literalNames $ S.toList lits

buildEdgeList :: ZFormula -> Map String Node -> [LEdge Int]
buildEdgeList (ZFormula lits) namesToNodes = edgeList
  where
    edgeList = S.toList $ S.map (buildLiteralEdge namesToNodes) lits

normalizeFormula :: ZFormula -> ZFormula
normalizeFormula (ZFormula lits) = ZFormula $ S.fromList normedLits
  where
    normedLits = L.concat $ L.map normalizeLiteral $ S.toList lits

containsNegCycle :: Gr String Int -> Bool
containsNegCycle formulaGraph = case isEmpty formulaGraph of
  True -> False
  False -> negCycleFrom headNode graphNodes graphEdges
  where
    graphNodes = nodes formulaGraph
    graphEdges = labEdges formulaGraph
    headNode = head graphNodes

negCycleFrom :: Node -> [Node] -> [LEdge Int] -> Bool
negCycleFrom source nodes edges = checkNegCycle edges weights
  where
    initialWeights = M.fromList $ L.zip nodes $ L.map (initWeight source) nodes
    weights = computeWeights nodes edges M.empty initialWeights 0

initWeight :: Node -> Node -> Int
initWeight source node = case source == node of
  True -> 0
  False -> maxBound :: Int

computeWeights :: [Node] ->
                  [LEdge Int] ->
                  Map Node Node ->
                  Map Node Int ->
                  Int ->
                  Map Node Int
computeWeights nodes edges predecessors weights i = case i == L.length nodes of
  True -> weights
  False -> computeWeights nodes edges newPredecessors newWeights (i + 1)
  where
    (newWeights, newPredecessors) = updateWeightsAndPreds edges weights predecessors

updateWeightsAndPreds :: [LEdge Int] ->
                         Map Node Int ->
                         Map Node Node ->
                         (Map Node Int, Map Node Node)
updateWeightsAndPreds edges weights preds = L.foldl updateEdge (weights, preds) edges

updateEdge :: (Map Node Int, Map Node Node) -> LEdge Int -> (Map Node Int, Map Node Node)
updateEdge (weights, preds) (u, v, w) = case wu + w < wv of
  True -> (M.insert v (wu + w) weights, M.insert v u preds)
  False -> (weights, preds)
  where
    wu = fromJust $ M.lookup u weights
    wv = fromJust $ M.lookup v weights

checkNegCycle :: [LEdge Int] -> Map Node Int -> Bool
checkNegCycle edges weights = L.or negCycleChecks
  where
    negCycleChecks = L.map (checkEdgeForNegCycle weights) edges

checkEdgeForNegCycle :: Map Node Int -> LEdge Int -> Bool
checkEdgeForNegCycle weights (u, v, w) = wu + w < wv
  where
    wu = fromJust $ M.lookup u weights
    wv = fromJust $ M.lookup v weights
