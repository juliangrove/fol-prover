{-# LANGUAGE LambdaCase #-}

module Tableaux where

import Formulae

type SignedForm = (Form, Bool)
type Path = [SignedForm]
type Branch = [SignedForm]
type Tableau = [Maybe Path]

type PropRule = SignedForm -> [[SignedForm]]
type Rule = [SignedForm] -> PropRule

andRule, orRule, notRule :: PropRule

andRule sf@(And f0 f1, True) = [[(f0, True), (f1, True), sf]]
andRule sf@(And f0 f1, False) = [[(f0, False), sf], [(f1, False), sf]]
andRule sf = [[sf]]

orRule sf@(Or f0 f1, True) = [[(f0, True), sf], [(f1, True), sf]]
orRule sf@(Or f0 f1, False) = [[(f0, False), (f1, False), sf]]
orRule sf = [[sf]]

notRule sf@(Not f, True) = [[(f, False), sf]]
notRule sf@(Not f, False) = [[(f, True), sf]]
notRule sf = [[sf]]

gammaRule :: Rule
gammaRule p f@(Forall v0 f0, True) = [[(subst v0 nameT f0, True), f]]
  where nameT = N (name 0)
          where name i = if (subst v0 (N (Name i)) f0, True) `elem` p
                         then name (i + 1)
                         else Name i
gammaRule p f@(Exists v0 f0, False) = [[(subst v0 nameF f0, False), f]]
  where nameF = N (name 0)
          where name i = if (subst v0 (N (Name i)) f0, False) `elem` p
                         then name (i + 1)
                         else Name i
gammaRule _ f = [[f]]

deltaRule :: Rule
deltaRule p f@(Forall v0 f0, False) = [[(subst v0 frshnm f0, False), f]]
  where frshnm = N (freshN (concatMap (ns . fst) p))
deltaRule p f@(Exists v0 f0, True) = [[(subst v0 frshnm f0, True), f]]
  where frshnm = N (freshN (concatMap (ns . fst) p))
deltaRule _ f = [[f]]

applyRule :: Rule -> [SignedForm] -> [[SignedForm]]
applyRule rule p = applyRule' rule p [[]]
  where applyRule' _ [] t = t
        applyRule' rule (f:fs) t = applyRule' rule fs t'
          where t' = do p <- t
                        b <- rule (f:p ++ fs) f
                        pure (p ++ b)

allRules :: Path -> [Path]
allRules p = do p0 <- applyRule (const andRule) p
                p1 <- applyRule (const orRule) p0
                p2 <- applyRule (const notRule) p1
                p3 <- applyRule gammaRule p2
                p4 <- applyRule deltaRule p3
                return p4

contradictory :: [SignedForm] -> Bool
contradictory [] = False
contradictory ((f, b) : fs) = (f, not b) `elem` fs || contradictory fs

loop :: Tableau -> Tableau
loop tableau = do
  maybefs <- tableau
  case maybefs of
    Nothing -> pure Nothing
    Just fs -> do
      newFs <- allRules fs
      if contradictory newFs
      then pure Nothing
      else pure (Just newFs)
