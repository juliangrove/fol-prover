{-# LANGUAGE TupleSections #-}

module Convenience where

import Formulae
import Tableaux

depth :: Int -> [Signed] -> Bool
depth n fs = all (== Nothing) $ iterate loop [Just fs] !! n

entails :: Int -> [Formula] -> Formula -> Bool
entails n fs f = depth n $ (f, False) : map (, True) fs
