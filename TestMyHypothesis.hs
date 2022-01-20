import qualified Data.Set (Set)
import Data.Set (Set)

data Graph a = Graph {
    vertices :: Set a
    edgeSet :: Set (a, a)
    }

addEdge :: Graph a -> a -> a -> Graph a
addEdge g u v = Graph {
    edgeSet = (Set.add (u, v) (Set.add (v, u) (edgeSet g)))
    }

{-
    Note that any odd cycle can start at its LEAST vertex. If you give me a sequence of vertices (representing an odd cycle) that does not start at its least vertex, I can give you a new sequence of vertices representing the SAME odd cycle, but which starts at the least vertex.

    But even if all odd cycles are established as starting on their least vertex, every odd cycle can still be represented in two different ways, because the sequence of vertices could represent walking the odd cycle either clockwise or counterclockwise.
-}

canonicalOddCycle :: Ord a => [a] -> [a]
canonicalOddCycle (v:vs) = canonicalOddCycleHelper [] v vs

minimumWithContext :: Ord a => (a, [a]) -> ([a], a, [a])
minimumWithContext (x, xs) = minimumWithContextHelper xs ([], x, [])

rotateToIndex :: [a] -> Int -> [a]
rotateToIndex xs n = rotateToIndexHelper ([], xs) n

rotateToIndexHelper :: ([a], [a]) -> Int -> [a]
rotateToIndexHelper (seen, []) n = rotateToIndex (reverse seen) i
rotateToIndexHelper (seen, (x:xs)) 0 = (x:xs) ++ (reverse seen)
rotateToIndexHelper (seen, (x:xs)) n = rotateToIndexHelper (x:seen) xs (n - 1)

indexOfMinimum :: Ord a => (a, [a]) -> Int
indexOfMinimum (x, xs) = indexOfMinimumHelper (x, 0) xs

indexOfMinimumHelper :: Ord a => (a, Int) -> [a] -> Int
indexOfMinimumHelper (currentMin, index) [] = index
indexOfMinimumHelper (currentMin, index) (x:rest) = indexOfMinimumHelper (newMin, newIndex) rest
    where
        (newMin, newIndex) =
            if (currentMin < x)
            then (currentMin, index)
            else (x, index + 1)


allOddCycles :: (Ord a) => Graph a -> Set [a]
allOddCycles g = allOddCyclesHelper g Set.empty (Set.toList g.vertices)

allOddCyclesHelper :: (Ord a) => Graph a -> Set [a] -> [a] -> Set [a]
allOddCyclesHelper 

main :: IO ()
main = putStrLn "Hello, world!"