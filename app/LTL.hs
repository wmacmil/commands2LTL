{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module LTL where

-- data Atomic where

type Atom = String

-- -- syntax of a LTL formulag
-- data Phi where
--   Atom                :: Atom -> Phi
--   Bottom, Top         :: Phi
--   Negate              :: Phi -> Phi
--   Join, Meet, Implies :: Phi -> Phi -> Phi
--   X, F, G             :: Phi -> Phi
--   deriving Show

  -- U, W, R             :: Phi -> Phi -> Phi
data Psi where
  Atom :: String -> Psi
  F    :: Psi -> Psi
  And  :: Psi -> Psi -> Psi
  deriving (Show)

-- can now define formulas as follows

big_blue_bear = Atom "big_blue_bear"

red_bridge = Atom "red_bridge"

--something like go2-NF
-- go2BBBthenRB = F (Meet big_blue_bear (F (red_bridge)))

