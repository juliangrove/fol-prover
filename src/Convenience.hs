{-# LANGUAGE TupleSections #-}

module Convenience where

import Formulae
import Tableaux

depth :: Int -> [SignedForm] -> Bool
depth n fs = all (== Nothing) (iterate loop [Just fs] !! n)

entails :: Int -> [Form] -> Form -> Bool
entails n fs f = depth n $ (f, False) : map (, True) fs

-- | Messing around:

imp f0 f1 = Or (Not f0) f1
see a b = P 0 [a, b]
animate a = P 1 [a]
seeAnimate = Forall (Var 1) ((Exists (Var 0) (see (V (Var 0)) (V (Var 1)))) `imp` animate (V (Var 1)))


-- >>> entails 3 [seeAnimate, see (N (Name 0)) (N (Name 1))] (animate (N (Name 1)))
-- True
