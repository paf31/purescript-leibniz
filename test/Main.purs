module Test.Main where

import Prelude

import Data.Exists
import Data.Leibniz

import Control.Monad.Eff.Console

data Z
data S n

data B depth a n = B (Leibniz depth (S n)) (Balanced n a) (Balanced n a)

data Balanced depth a
  = Leaf (Leibniz depth Z) a
  | Branch (Exists (B depth a))
  
example :: Balanced (S Z) Int
example = Branch (mkExists (B id (Leaf id 0) (Leaf id 1)))

-- Does not compile:
-- bad = Branch (mkExists (B id (Leaf id 0) example))

instance showBalanced :: (Show a) => Show (Balanced depth a) where
  show (Leaf _ a) = "Leaf " ++ show a
  show (Branch e) = runExists (\(B _ l r) -> "Branch (" ++ show l ++ ") (" ++ show r ++ ")") e 

main = print example
