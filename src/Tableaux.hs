{-# LANGUAGE LambdaCase #-}

module Tableaux where

import Formulae

type Signed = (Formula, Bool)
type Tableau = [Maybe [Signed]]

type PropositionalRule = Signed -> [[Signed]]
type QuantifierRule = [Signed] -> Signed -> [Signed]

andRule, orRule, notRule :: PropositionalRule
andRule sf@(And f0 f1, True) = [[(f0, True), (f1, True), sf]]
andRule sf@(And f0 f1, False) = [[(f0, False)], [(f1, False), sf]]
andRule sf = [[sf]]
orRule sf@(Or f0 f1, True) = [[(f0, True), sf], [(f1, True), sf]]
orRule sf@(Or f0 f1, False) = [[(f0, False), (f1, False), sf]]
orRule sf = [[sf]]
notRule sf@(Not f, True) = [[(f, False), sf]]
notRule sf@(Not f, False) = [[(f, True), sf]]
notRule sf = [[sf]]

deltaRule, gammaRule :: QuantifierRule
deltaRule fs sf = case sf of
  (Forall v f0, True) -> [(subst v (N (nameT v f0)) f0, True), sf]
  (Exists v f0, False) -> [(subst v (N (nameF v f0)) f0, False), sf]
  _ -> [sf]
  where nameT v f0 = name 0
          where name i = if (subst v (N (Name i)) f0, True) `elem` fs
                         then name (i + 1)
                         else Name i
        nameF v f0 = name 0
          where name i = if (subst v (N (Name i)) f0, False) `elem` fs
                           then name (i + 1)
                           else Name i
gammaRule fs = \case
  (Exists v f0, True) -> [(subst v (N name) f0, True)]
  (Forall v f0, False) -> [(subst v (N name) f0, False)]
  sf -> [sf]
  where name = case concatMap (names . fst) fs of
                 [] -> Name 0
                 ns -> Name (i + 1)
                   where Name i = maximum ns

applyRule :: PropositionalRule -> [Signed] -> [[Signed]]
applyRule _ [(Top, True)] = [[(Top, True)]]
applyRule _ [] = [[(Top, True)]]
applyRule rule (f:fs) = applyRule rule fs >>= justTheFirst
  where justTheFirst :: [Signed] -> [[Signed]]
        justTheFirst g = map (++ g) $ rule f

applyQuantRule :: QuantifierRule -> [Signed] -> [Signed]
applyQuantRule rule fs = applyQuantRule0 rule fs [(Top, True)]
  where applyQuantRule0 _ [(Top, True)] fs' = fs'
        applyQuantRule0 _ [] fs' = fs'
        applyQuantRule0 rule fs1@(f:fs0) fs' =
          applyQuantRule0 rule fs0 (rule (fs1 ++ fs') f ++ fs')

allRules :: [Signed] -> [[Signed]]
allRules fs = do fs <- applyRule orRule fs
                 fs <- applyRule andRule fs
                 fs <- applyRule notRule fs
                 fs <- pure (applyQuantRule deltaRule fs)
                 fs <- pure (applyQuantRule gammaRule fs)
                 pure fs

contradictory :: [Signed] -> Bool
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
