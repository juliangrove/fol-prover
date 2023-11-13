module Formulae where

data Var = Var Integer deriving (Eq, Ord, Show)
data Name = Name Integer deriving (Eq, Ord, Show)
data Term = V Var | N Name deriving (Eq, Show)
data Form = P Integer [Term]
             | And Form Form
             | Or Form Form
             | Not Form
             | Forall Var Form
             | Exists Var Form deriving Show

instance Eq Form where
  P i ts0 == P j ts1 = i == j && ts0 == ts1
  And a b == And c d = a == c && b == d
  Or a b == Or c d = a == c && b == d
  Not f0 == Not f1 = f0 == f1
  Forall v0 f0 == Forall v1 f1 = f0new == f1new
    where vnew = freshV (fv f0 ++ fv f1)
          f0new = subst v0 (V vnew) f0
          f1new = subst v1 (V vnew) f1
  Exists v0 f0 == Exists v1 f1 = f0new == f1new
    where vnew = freshV (fv f0 ++ fv f1)
          f0new = subst v0 (V vnew) f0
          f1new = subst v1 (V vnew) f1
  _ == _ = False

class Attributes e where
  fv :: e -> [Var]
  ns :: e -> [Name]

instance Attributes Term where
  fv (V v) = [v]
  fv (N _) = []
  ns (V _) = []
  ns (N n) = [n]
  
instance Attributes Form where
  fv (P _ ts) = concatMap fv ts
  fv (And f0 f1) = fv f0 ++ fv f1
  fv (Or f0 f1) = fv f0 ++ fv f1
  fv (Not f) = fv f
  fv (Forall v f) = filter (/= v) $ fv f
  fv (Exists v f) = filter (/= v) $ fv f

  ns (P _ ts) = concatMap ns ts
  ns (And f0 f1) = ns f0 ++ ns f1
  ns (Or f0 f1) = ns f0 ++ ns f1
  ns (Not f) = ns f
  ns (Forall v f) = ns f
  ns (Exists v f) = ns f

freshV :: [Var] -> Var
freshV [] = Var 0
freshV l = Var (n + 1)
  where Var n = maximum l

freshN :: [Name] -> Name
freshN [] = Name 0
freshN l = Name (n + 1)
  where Name n = maximum l

substTerm :: Var -> Term -> Term -> Term
substTerm v0 t (V v1) = if v0 == v1 then t else (V v1)
substTerm v0 t n@(N _) = n

subst :: Var -> Term -> Form -> Form
subst v0 t (P i ts) = P i (map (substTerm v0 t) ts)
subst v0 t (And f0 f1) = And (subst v0 t f0) (subst v0 t f1)
subst v0 t (Or f0 f1) = Or (subst v0 t f0) (subst v0 t f1)
subst v0 t (Not f) = Not (subst v0 t f)
subst v0 t f@(Forall v1 f1) | v1 == v0 = f
subst v0 t@(N _) (Forall v1 f) = Forall v1 (subst v0 t f)
subst v0 t@(V v1) (Forall v2 f) | v2 == v1 = Forall vnew (subst v0 t fnew)
  where vnew = freshV (v1 : fv f)
        fnew = subst v2 (V vnew) f
subst v0 t@(V v1) (Exists v2 f) | v2 /= v1 = Exists v2 (subst v0 t f)
subst v0 t f@(Exists v1 f1) | v1 == v0 = f
subst v0 t@(N _) (Exists v1 f) = Exists v1 (subst v0 t f)
subst v0 t@(V v1) (Exists v2 f) | v2 == v1 = Exists vnew (subst v0 t fnew)
  where vnew = freshV (v1 : fv f)
        fnew = subst v2 (V vnew) f
subst v0 t@(V v1) (Exists v2 f) | v2 /= v1 = Exists v2 (subst v0 t f)
