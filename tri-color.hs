{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.Graph (Graph, Vertex, edges, vertices)

data Color = Red | Blue | Green
    deriving Eq

both :: (t -> b) -> (t, t) -> (b, b)
both f (a, b) = (f a, f b)

on :: (t1 -> t1 -> t2) -> (t3 -> t1) -> t3 -> t3 -> t2
(g `on` f) x y = f x `g` f y

checkColoring :: Graph -> [(Vertex, Color)] -> Bool
checkColoring graph coloredVertices = all (uncurry $ (/=) `on` getVertexColor) graphEdges
  where
    graphEdges = edges graph
    getVertexColor vert = snd $ head $ filter ((== vert) . fst) coloredVertices

-- generirai vsichki kombinacii s duljina n
genCombinations :: Int -> [a] -> [[a]]
genCombinations n ogxs =
  if n == 0
  then [[]]
  else [(x : xs) | x <- ogxs, xs <- genCombinations (n - 1) ogxs]

generateAllColoring :: Graph -> [Color] -> [[(Vertex, Color)]]
generateAllColoring graph colors = map (zip vertexes) colorPermutations
  where
    vertexes = vertices graph
    colorPermutations = genCombinations (length vertexes) colors

triColor :: Graph -> Maybe [(Vertex, Color)]
triColor graph =
  if null triColoredGraphs
    then Nothing
    else Just (head triColoredGraphs)
  where
    triColoredGraphs = filter (checkColoring graph) $ generateAllColoring graph [Red, Blue, Green]