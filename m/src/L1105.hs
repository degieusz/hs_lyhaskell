module L1105
    where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving(Show,Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

fetchCode :: Int -> LockerMap -> Either String Code
fetchCode lockerNo map =
    case Map.lookup lockerNo map of
        Nothing -> Left $ "Locker no" ++ show lockerNo ++ "doesnt ex"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "LLocker no" ++ show lockerNo ++  " taken "
infixr 5 :-:
data List a  = Empty | a :-: (List a) deriving(Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs )  .++ ys = x :-: (xs .++ ys)
{-(.++) (List a) (List a) =-}

data Tree a = EmptyTree | Node a  (Tree a) (Tree a ) deriving(Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insert ::(Ord a ) => a -> Tree a -> Tree a
insert e EmptyTree = singleton e
insert e (Node a left right)
    | e == a = Node e left right
    | e >  a = Node a left (insert e right)
    | e <  a = Node a (insert e left) right

elem' ::(Ord a) => a -> Tree a -> Bool
elem' e EmptyTree = False
elem' e (Node a left right)
    | e == a = True
    | e >  a = elem' e right
    | e <  a = elem' e left

