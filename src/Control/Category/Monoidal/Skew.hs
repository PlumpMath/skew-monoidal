{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{- |
Module      :  Control.Category.Monoidal.Skew
Description :  skew-monoidal
Copyright   :  (c) 2015 Thomas Bereknyei
License     :  BSD3
Maintainer  :  Thomas Bereknyei <tomberek@gmail.com>
Stability   :  unstable
Portability :  GADTs,LambdaCase

Implements normalization from: http://arxiv.org/pdf/1406.2064v1.pdf
-}
module Control.Category.Monoidal.Skew where

import Prelude hiding (id,(.))
import Control.Category
import Control.Categorical.Bifunctor

-- A Skew-monoidal category is a category `k` together with a
-- distinguished object `Id`, a functor `bimap`, and three
-- natural transformations, `lambda`, rho, and disassociate.
-- It is half of an Associative, Monoidal, Cartesian


-- | Objects of free symetric monoidal category Var, in this case: Hask
data Var = A | B | C deriving Show
data Tm = I | X' Var  | Tm :-: Tm deriving Show

-- Maps between two objects
data Rule = IdRule | Dot Rule Rule | Cross Rule Rule | La | Rh | Asc deriving Show
evalRule :: Rule -> Tm -> Tm
evalRule IdRule = id
evalRule (Dot a b) = evalRule a . evalRule b
evalRule (Cross a b) = \(c :-: d) -> evalRule a c :-: evalRule b d
evalRule La = \(I :-: a) -> a
evalRule Rh = \a -> a :-: I
evalRule Asc = \case
    ((a :-: b) :-: c) -> (a :-: (b :-: c))
    a -> error $ show a

-- Do we need to encode all the rules on page 4? Or are those just expected laws?

-- We define "normal forms" of object expression as Nf
data Nf = J | Var :.: Nf deriving Show

emb :: Nf -> Tm
emb J = I
emb (a :.: n) = X' a :-: emb n

-- ||-|| :: i'm calling it splay
splay :: Tm -> Nf -> Nf
splay (X' x) n = x :.: n
splay I n = n
splay (a :-: b) n = splay a (splay b n)

-- | Every object expression is assigned a normal form.
nf :: Tm -> Nf
nf a = splay a J

-- | In Lemma 3, <<->> :: i'm calling it splat
splat :: Tm -> Nf -> Rule
splat (X' _) _ = IdRule
splat I _ = La
splat (a :-: b) n = splat a (splay b n)
                    `Dot` (IdRule `Cross` splat b n `Dot` Asc)

-- | "Normalizing" map expression.
nm :: Tm -> Rule
nm a = splat a J `Dot` Rh

{-  Would this be useful? Can translate into our Tm as an intermediate, normalize, translate back?

class Bifunctor p k k k => SkewMonoidal (k :: * -> * -> *) (p :: * -> * -> *) where
    type Id (k :: * -> * -> *) (p :: * -> * -> *) :: *
    lam :: (Id k p `p` a) `k` a
    rho :: a `k` (a `p` Id k p)
    dis :: ((a `p` b) `p` c) `k` (a `p` (b `p` c))

-- Free Skew-monoidal Category includes embedding for
-- normalization.
data SkewF f where
    Id :: SkewF f
    XXX :: f -> f -> SkewF f
    Lam :: f -> SkewF f
    Rho :: f -> SkewF f
    Dis :: f -> SkewF f
    Lift :: Var -> SkewF f
    deriving Show

newtype Fix f = Fix (f (Fix f))
type Skew = Fix SkewF

-- Smart Constructors
i :: Skew
i = Fix Id
x :: (f -> SkewF Skew) -> f -> Skew
x a b = Fix $ a b
a *.* b = Fix (a `XXX` b)
l,r,d :: Skew -> Skew
l = x Lam
r = x Rho
d = x Dis
lift :: Var -> Skew
lift = Fix . Lift

instance (Show (f (Fix f))) => Show (Fix f) where
    showsPrec p (Fix x) = showParen (p >= 11) (showString "Fix " . showsPrec 11 x)
---}
