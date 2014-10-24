module DifferenceLogic.Solver(
  zF,
  zLit,
  eq, gt, lt, geq, leq,
  consistentOverZ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Set as S

-- This solver solves conjuctions of literals
data ZFormula = ZFormula (Set ZLiteral)
                deriving (Eq, Ord, Show)

zF :: [ZLiteral] -> ZFormula
zF litList = ZFormula $ S.fromList litList

data ZLiteral = ZLiteral {
  left :: String,
  right :: String,
  predicate :: Predicate,
  const :: Int
  } deriving (Eq, Ord, Show)

zLit = ZLiteral

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
buildLiteralEdge namesToNodes (ZLiteral l r Gt c) = (rNode, lNode, -c)
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

eq = Eq
lt = Lt
gt = Gt
leq = Leq
geq = Geq

consistentOverZ :: ZFormula -> Bool
consistentOverZ formula = not $ containsNegCycle formulaGraph
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
