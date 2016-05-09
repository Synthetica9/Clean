// Partially copied from our implementation of assignment 3 of FP1 (in Haskell)
implementation module StdDynSet

import StdEnv
import StdDynamic

class Set a | TC, ==, toString a

:: Set = Set [SetElem]
:: SetElem :== (Dynamic, String, (Dynamic -> Bool))

instance zero Set where
    zero = Set []

instance toString Set where
    toString (Set []) = "∅"
    toString (Set [(_, s, _) : xs]) = "{" +++ s +++ trailing xs where
        trailing [] = "}"
        trailing [(_, s, _) : xs] = ", " +++ s +++ trailing xs


instance == Set
where == a b = abort "== instance voor Set nog niet geimplementeerd.\n"

toSet :: a -> Set | Set a
toSet a = listToSet [a]

elemEq :: SetElem SetElem -> Bool
elemEq (a, _, _) (_, _, f) = f a

listToSet :: [a] -> Set | Set a
listToSet [] = Set []
listToSet xs = Set (map setElem (nub xs))

nub = nubBy elem

nubBy _ [] = []
nubBy f [x : xs]
    | f x xs = (nubBy f xs)
    | otherwise = [x : nubBy f xs]

elem = elemBy (==)

elemBy _ _ [] = False
elemBy f x [y : ys]
    | f x y = True
    | otherwise = elemBy f x ys

setElem :: a -> SetElem | Set a
setElem a = (dynamic a, toString a, eq a) where
    eq :: a Dynamic -> Bool | Set a
    eq x (y :: a^) = x == y
    eq _ _ = False

nrOfElts :: Set -> Int
nrOfElts (Set a) = length a

isEmptySet :: Set -> Bool
isEmptySet a = nrOfElts a == 0

memberOfSet :: a Set -> Bool | Set a
memberOfSet _ (Set []) = False
memberOfSet x (Set [(_, _, f) : ys])
    | f (dynamic x) = True // Let's just hope f isn't lying...
    | otherwise = memberOfSet x (Set ys)

checkEmpty :: (Set -> Set -> Set) Set Set -> Bool
checkEmpty f x y = isEmptySet (f x y)

isSubset :: Set Set -> Bool
isSubset a b = abort "isSubset nog niet geimplementeerd.\n"

isStrictSubset :: Set Set -> Bool
isStrictSubset a b = abort "isStrictSubset nog niet geimplementeerd.\n"

union :: Set Set -> Set
union (Set x) (Set y) = abort "Errrrrrr!"

intersection :: Set Set -> Set
intersection a b = abort "intersection nog niet geimplementeerd.\n"

without :: Set Set -> Set
without a b = abort "without nog niet geimplementeerd.\n"

Start = toString (listToSet [1,2,3, 2, 2, 3])
