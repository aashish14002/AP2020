module Warmup where

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)
-- move _ (x,y) = (x, y)


moves :: [Direction] -> Pos -> Pos
moves [] pos = pos
moves (d:ds) pos = moves ds . move d $ pos 


data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero x = x
add x Zero = x
add x (Succ y) = add (Succ x) y 

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ Zero) x = x
mult x (Succ Zero) = x
mult x (Succ y) = add x . mult x $ y


nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

int2nat :: Int -> Nat
int2nat x
  | x==0 = Zero
  | x>0 = Succ( int2nat (x-1))
  | otherwise = Zero


data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert n Leaf = Node n Leaf Leaf
insert n t@(Node k left right)
  | n==k = t
  | n<k = Node k (insert n left) right
  | otherwise = Node k left (insert n right)

