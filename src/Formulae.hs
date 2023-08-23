{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulae where

data Var = Var Integer deriving (Eq, Ord, Show)
data Name = Name Integer deriving (Eq, Ord, Show)
data Term = V Var | N Name deriving (Eq, Show)
data Formula = Pred Integer [Term]
             | Top
             | Bot
             | And Formula Formula
             | Or Formula Formula
             | Not Formula
             | Forall Var Formula
             | Exists Var Formula deriving (Show)

instance Eq Formula where
  Top == Top = True
  Bot == Bot = True
  And a b == And c d = a == c && b == d
  Or a b == Or c d = a == c && b == d
  Not f0 == Not f1 = f0 == f1
  Pred i l == Pred j k = i == j && l == k
  Forall v0 f0 == Forall v1 f1 = subst v0 vnew f0 == subst v1 vnew f1
    where vnew = V $ fresh $ fv f0 ++ fv f1
  Exists v0 f0 == Exists v1 f1 = subst v0 vnew f0 == subst v1 vnew f1
    where vnew = V $ fresh $ fv f0 ++ fv f1
  _ == _ = False

class Properties t where
  fv :: t -> [Var]
  names :: t -> [Name]

instance Properties Term where
  fv (V v) = [v]
  fv _ = []
  names (N n) = [n]
  names _ = []

instance Properties Formula where
  fv (Pred _ ts) = concatMap fv ts
  fv (And f0 f1) = fv f0 ++ fv f1
  fv (Or f0 f1) = fv f0 ++ fv f1
  fv (Not f) = fv f
  fv (Forall v f) = filter (/= v) $ fv f
  fv (Exists v f) = filter (/= v) $ fv f
  fv _ = []
  names (Pred _ ts) = concatMap names ts
  names (And f0 f1) = names f0 ++ names f1
  names (Or f0 f1) = names f0 ++ names f1
  names (Not f) = names f
  names (Forall v f) = names f
  names (Exists v f) = names f
  names _ = []
  
fresh :: [Var] -> Var
fresh vs = Var (n + 1)
  where Var n = maximum vs

class Subst t where
  subst :: Var -> Term -> t -> t

instance Subst Term where
  subst v t (V v0) = if v0 == v then t else (V v0)
  subst v t n@(N _) = n

instance Subst Formula where
  subst v t (Pred i ts) = Pred i $ map (subst v t) ts
  subst v t (And f0 f1) = And (sub f0) (sub f1)
    where sub = subst v t
  subst v t (Or f0 f1) = Or (sub f0) (sub f1)
    where sub = subst v t
  subst v t (Not f) = Not (subst v t f)
  subst v t t0@(Forall v0 _) | v0 == v = t0
  subst v (V v0) (Forall v1 t0) | v1 /= v0 = Forall v1 $ subst v (V v0) t0
  subst v (V v0) (Forall v1 t0) | v1 == v0 = Forall vnew $ subst v (V v0) tnew
    where vnew = fresh $ v0 : fv t0
          tnew = subst v1 (V vnew) t0
  subst v t (Forall v0 f) = Forall v0 $ subst v t f
  subst v t t0@(Exists v0 _) | v0 == v = t0
  subst v (V v0) (Exists v1 f) | v1 /= v0 = Exists v1 $ subst v (V v0) f
  subst v (V v0) (Exists v1 f) | v1 == v0 = Exists vnew $ subst v (V v0) fnew
    where vnew = fresh $ v0 : fv f
          fnew = subst v1 (V vnew) f
  subst v t (Exists v0 f) = Exists v0 $ subst v t f
  subst v t f = f
