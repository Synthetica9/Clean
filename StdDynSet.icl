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

instance == Set where
    == a b = isSubset a b && isSubset b a

toSet :: a -> Set | Set a
toSet a = listToSet [a]

elemEq :: SetElem SetElem -> Bool
elemEq (a, _, _) (_, _, f) = f a

listToSet :: [a] -> Set | Set a
listToSet [] = Set []
listToSet xs = Set (map setElem (nub xs))

filterSet f (Set x) = Set (filter f x)

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

notMemberOfSet e s = not (memberOfSet e s)

checkEmpty :: (Set -> Set -> Set) Set Set -> Bool
checkEmpty f x y = isEmptySet (f x y)

isSubset :: Set Set -> Bool
isSubset = checkEmpty without

isStrictSubset :: Set Set -> Bool
isStrictSubset a b = isSubset a b && not a == b

union :: Set Set -> Set
union (Set x) (Set y) = Set ((nubBy (elemBy elemEq)) (x ++ y))

intersection :: Set Set -> Set
intersection a b = union (without a b) (without b a)s

without :: Set Set -> Set
without a b = filterSet (notMemberOfSet b) a

Start = toString (union (listToSet [1,2,3, 2, 2, 3]) (listToSet ["Hello", "world"]))
