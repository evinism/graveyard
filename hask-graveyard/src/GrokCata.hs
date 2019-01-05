module GrokCata where

data ListF a r = Nil | Cons a r deriving (Show)

instance Functor (ListF a) where
    fmap _ Nil = Nil
    fmap f (Cons e x) = Cons e (f x)

newtype Fix f = Fx (f (Fix f))

type MyList a = Fix (ListF a)

alg :: ListF Int Int -> Int
alg Nil = 1
alg (Cons x y) = x * y

a :: MyList Int
a = Fx (Cons 1 (Fx Nil))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix


