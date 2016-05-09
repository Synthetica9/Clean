// Partially copied from our implementation of assignment 3 of FP1 (in Haskell)
implementation module StdDynSet

import StdEnv
import StdDynamic

class Set a | TC, ==, toString a

:: Set = Set [(Dynamic, String, (Dynamic -> Bool))]

instance zero Set
where zero = abort "zero instance voor Set nog niet geimplementeerd.\n"

instance toString Set where
    toString (Set []) = "∅"
    toString (Set [(_, s, _) : xs]) = "{" +++ s +++ trailing xs where
        trailing [] = "}"
        trailing [(_, s, _) : xs] = ", " +++ s +++ trailing xs


instance == Set
where == a b = abort "== instance voor Set nog niet geimplementeerd.\n"

toSet :: a -> Set | Set a
toSet a = listToSet [a]

listToSet :: [a] -> Set | Set a
listToSet [] = Set []
listToSet xs = Set (map setElem (removeDoubles xs))

removeDoubles :: [a] -> [a] | == a
removeDoubles [] = []
removeDoubles [x : xs]
    | elem x xs = (removeDoubles xs)
    | otherwise = [x : removeDoubles xs]

elem _ [] = False
elem x [y : ys]
    | x == y = True
    | otherwise = elem x ys

setElem :: a -> (Dynamic, String, (Dynamic -> Bool)) | Set a
setElem a = (dynamic a, toString a, eq a) where
    eq :: a Dynamic -> Bool | Set a
    eq x (y :: a^) = x == y
    eq _ _ = False

nrOfElts :: Set -> Int
nrOfElts a = abort "nrOfElts nog niet geimplementeerd.\n"

isEmptySet :: Set -> Bool
isEmptySet a = abort "isEmptySet nog niet geimplementeerd.\n"

memberOfSet :: a Set -> Bool | Set a
memberOfSet x a = abort "memberOfSet nog niet geimplementeerd.\n"

isSubset :: Set Set -> Bool
isSubset a b = abort "isSubset nog niet geimplementeerd.\n"

isStrictSubset :: Set Set -> Bool
isStrictSubset a b = abort "isStrictSubset nog niet geimplementeerd.\n"

union :: Set Set -> Set
union a b = abort "union nog niet geimplementeerd.\n"

intersection :: Set Set -> Set
intersection a b = abort "intersection nog niet geimplementeerd.\n"

without :: Set Set -> Set
without a b = abort "without nog niet geimplementeerd.\n"

Start = toString (listToSet [1,2,3, 2, 2, 3])
