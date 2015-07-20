module Ebitor.AATree.Internal where

import Data.Foldable
import Prelude hiding (foldr, map, null)
import qualified Prelude

data AATree a = Nil | Branch Int a (AATree a) (AATree a)
                deriving (Show, Eq)

instance Foldable AATree where
    foldr f z Nil = z
    foldr f z (Branch _ k l r) = foldr f (f k (foldr f z r)) l

map :: (Ord a, Ord b) => (a -> b) -> AATree a -> AATree b
map f Nil = Nil
map f (Branch lv v l r) = Branch lv (f v) (map f l) (map f r)

concat :: Ord a => AATree a -> AATree a -> AATree a
concat = foldr insert

empty :: AATree a
empty = Nil

null Nil = True
null _ = False

fromList :: Ord a => [a] -> AATree a
fromList = Prelude.foldr insert empty

level :: AATree a -> Int
level Nil = 0
level (Branch l _ _ _) = l

value :: AATree a -> Maybe a
value Nil = Nothing
value (Branch _ v _ _) = Just v

left Nil = Nil
left (Branch _ _ l _) = l

right Nil = Nil
right (Branch _ _ _ r) = r

leaf :: AATree a -> Bool
leaf Nil = True
leaf (Branch _ _ Nil Nil) = True
leaf _ = False

setLevel Nil _ = Nil
setLevel (Branch _ v l r) lv = Branch lv v l r

setValue Nil _ = Nil
setValue (Branch lv _ l r) v = Branch lv v l r

setLeft Nil _ = Nil
setLeft (Branch lv v _ r) l = Branch lv v l r

setRight Nil _ = Nil
setRight (Branch lv v l _) r = Branch lv v l r

skew :: AATree a -> AATree a
skew t@(Branch _ _ Nil _) = t
skew t@(Branch levelT vT l@(Branch levelL vL a b) r)
    | levelT == levelL = Branch levelL vL a $ Branch levelT vT b r
skew t = t

split :: AATree a -> AATree a
split t@(Branch _ _ _ Nil) = t
split t@(Branch _ _ _ (Branch _ _ _ Nil)) = t
split t@(Branch levelT vT a r@(Branch levelR vR b x))
    | levelT == level x = Branch (levelR + 1) vR (Branch levelT vT a b) x
split t = t

insert :: Ord a => a -> AATree a -> AATree a
insert x Nil = Branch 1 x Nil Nil
insert x t@(Branch _ y l r) =
    split $ skew $ case compare x y of
        GT -> setRight t $ insert x r
        _ -> setLeft t $ insert x l

delete :: Ord a => AATree a -> a -> AATree a
delete Nil _ = Nil
delete (Branch levelT y l r) x =
    balance $ case compare x y of
        GT -> Branch levelT y l $ delete r x
        LT -> Branch levelT y (delete l x) r
        EQ -> deleteNode l
  where
    deleteNode t | leaf t = Nil
    deleteNode t@(Branch levelT y Nil r) =
        let (Branch _ vl _ _) = successor t
        in  Branch levelT vl Nil $ delete r vl
    deleteNode t@(Branch levelT y l r) =
        let (Branch _ vl _ _) = predecessor t
        in  Branch levelT vl (delete l vl) r

    lastLeft t@(Branch _ _ Nil _) = t
    lastLeft (Branch _ _ l _) = lastLeft l
    lastLeft Nil = Nil

    lastRight t@(Branch _ _ _ Nil) = t
    lastRight (Branch _ _ _ r) = lastRight r
    lastRight Nil = Nil

    predecessor (Branch _ _ l _) = lastRight l
    predecessor Nil = Nil

    successor (Branch _ _ _ r) = lastLeft r
    successor Nil = Nil

    decreaseLevel t =
        let shouldBe = 1 + min (level $ left t) (level $ right t)
            r = right t
            levelT = level t
            levelR = level r
        in  setRight (setLevel t (if shouldBe < levelT then shouldBe else levelT))
                     (setLevel r (if shouldBe < levelR then shouldBe else levelR))

    skewRight t = setRight t $ skew (right t)
    skewRight2 t = setRight t $ skewRight (right t)
    splitRight t = setRight t $ split (right t)

    balance :: AATree a -> AATree a
    balance = splitRight . split . skewRight2 . skewRight . skew . decreaseLevel
