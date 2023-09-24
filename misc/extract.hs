import qualified Data.Set as Set
extract :: (Eq a) =>
    [[(a, b)]] -> -- Given a list of list of pairs
    (
        Maybe a, -- Return the a most common among lists, if one exists
        [(Maybe b, [(a, b)])] -- And return each list of pairs with one a maybe removed and its associated b value recorded
    )
extract = undefined

-- any function of the form (a -> (b, c)) can be transformed into (a -> b), (a -> c)

mostCommon :: (Eq a) => [[(a, b)]] -> Maybe a
mostCommon = undefined

-- notice for the (a -> c) case, we need to pass the Maybe a that we found
commonExtraction :: (Eq a) => [[(a, b)]] -> Maybe a -> [(Maybe b, [(a, b)])]
commonExtraction = undefined

-- But the case when Maybe a is Nothing is trivial, let's pretend we have a instead. And the return value should better represent positional extraction, like proof that the element exists

commonExtraction2 :: (Eq a) => [[(a, b)]] -> a -> [Maybe ([(a, b)], Maybe b, [(a, b)])]
commonExtraction2 = undefined

-- But now it just looks like a mapping, since the size of the input list is the size of the output list. Let's focus on the function inside the call to map

commonExtraction3 :: (Eq a) => [(a, b)] -> a -> Maybe ([(a, b)], Maybe b, [(a, b)])
commonExtraction3 = undefined


pluck :: (Eq a) => a -> [(a, b)] -> (Maybe b, [(a, b)])
pluck _ [] = (Nothing, [])
pluck key ((k, v):rest)
  | key == k = (Just v, rest)
  | otherwise = let (result, newRest) = pluck key rest
                in (result, (k, v) : newRest)

main :: IO ()
main = do
  let list = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]
  print $ pluck 2 list  -- Output: (Just "two",[(1,"one"),(3,"three"),(4,"four")])
  print $ pluck 5 list  -- Output: (Nothing,[(1,"one"),(2,"two"),(3,"three"),(4,"four")])


data Result a b
  = Rec a [b] (Result a b) (Result a b)
  | Base [(a, b)]

recursiveSplit :: (Ord a) => [(a, b)] -> Result a b
recursiveSplit l = let
  s = Set.fromList $ map fst l
  
  
  in undefined