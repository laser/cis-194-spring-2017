{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week11.Assignment where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1, 6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-- #2 (there is no assignment #1, really)
battle :: Battlefield -> Rand StdGen Battlefield
battle field@(Battlefield 0 _) = return field
battle field@(Battlefield _ 0) = return field
battle (Battlefield attackerCount defenderCount) = do
  attackerRolls <- getRolls (attackerCount - 1)
  defenderRolls <- getRolls defenderCount
  let (attackerLosses, defenderLosses) = getLosses attackerRolls defenderRolls
  return (Battlefield (attackerCount - attackerLosses) (defenderCount - defenderLosses))

getLosses :: [DieValue] -> [DieValue] -> (Int, Int)
getLosses attackerRolls defenderRolls =
  let pairs = zip attackerRolls defenderRolls
      results = subtractRolls <$> pairs
      defenderLosses = count (0 <) results
      attackerLosses = count (0 >=) results
  in (attackerLosses, defenderLosses)

subtractRolls :: (DieValue, DieValue) -> Int
subtractRolls (DV n1, DV n2) = n1 - n2

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

getRolls :: Int -> Rand StdGen [DieValue]
getRolls n = sortRolls <$> replicateM n getRandom

sortRolls :: [DieValue] -> [DieValue]
sortRolls = sortBy . flip $ compare

-- #3
invade :: Battlefield -> Rand StdGen Battlefield
invade field@(Battlefield n _) | n <= 1 = return field
invade field@(Battlefield _ 0) = return field
invade field = do
  nextField <- battle field
  invade nextField

-- #4
successProb :: Battlefield -> Rand StdGen Double
successProb field = do
  results <- replicateM 1000 (invade field)
  let attackerWins = count isDefenderDead results
  return (fromIntegral attackerWins / 1000.0)

isDefenderDead :: Battlefield -> Bool
isDefenderDead (Battlefield _ 0) = True
isDefenderDead _ = False

-- #5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
