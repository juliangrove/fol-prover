{-# LANGUAGE TupleSections #-}

module Convenience where

import Formulae
import Tableaux

depth :: Int -> [Signed] -> Bool
depth n fs = all (== Nothing) $ iterate loop [Just fs] !! n

entails :: Int -> [Formula] -> Formula -> Bool
entails n fs f = depth n $ (f, False) : map (, True) fs

-- | Messing around:

transitivity = Forall (Var 0) (Forall (Var 1) (Forall (Var 2) (Or (Not (And (Pred 0 [V (Var 0), V (Var 1)]) (Pred 0 [V (Var 1), V (Var 2)]))) (Pred 0 [V (Var 0), V (Var 2)]))))

hyp n0 n1 = Pred 0 [N n0, N n1]

hyp1 = hyp (Name 0) (Name 1)
hyp2 = hyp (Name 1) (Name 2)
hyp3 = hyp (Name 2) (Name 3)
hyp4 = hyp (Name 0) (Name 2)

