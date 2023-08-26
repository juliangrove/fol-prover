{-# LANGUAGE TupleSections #-}

module Convenience where

import Formulae
import Tableaux

depth :: Int -> [Signed] -> Bool
depth n fs = all (== Nothing) $ iterate loop [Just fs] !! n

entails :: Int -> [Formula] -> Formula -> Bool
entails n fs f = depth n $ (f, False) : map (, True) fs

-- | Messing around:

imp f0 f1 = Or (Not f0) f1
see a b = Pred 0 [a, b]
animate a = Pred 1 [a]
seeAnimate = Forall (Var 0) (Forall (Var 1) (see (V (Var 0)) (V (Var 1)) `imp` animate (V (Var 1))))

-- >>> entails 4 [see (N (Name 0)) (N (Name 1)), seeAnimate] (animate (N (Name 1)))
-- True
