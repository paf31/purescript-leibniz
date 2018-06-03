module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Exists (Exists, mkExists, runExists)
import Data.Leibniz (type (~), coerce, liftLeibniz1of2, lowerLeibniz, refute, symm)

data Z
data S n

data B depth a n = B (depth ~ S n) (Balanced n a) (Balanced n a)

data Balanced depth a
  = Leaf (depth ~ Z) a
  | Branch (Exists (B depth a))

example :: Balanced (S Z) Int
example = Branch (mkExists (B identity (Leaf identity 0) (Leaf identity 1)))

-- Does not compile:
-- bad = Branch (mkExists (B identity (Leaf identity 0) example))

instance showBalanced :: Show a => Show (Balanced depth a) where
  show (Leaf _ a) = "Leaf " <> show a
  show (Branch e) = runExists (\(B _ l r) -> "Branch (" <> show l <> ") (" <> show r <> ")") e

applyBalanced :: forall depth a b c. (a -> b -> c) -> Balanced depth a -> Balanced depth b -> Balanced depth c
applyBalanced f (Leaf x a) (Leaf _ b) = Leaf x (f a b)
applyBalanced f (Branch b1) (Branch b2) =
  b1 # runExists \(B x l1 r1) ->
    b2 # runExists \(B y l2 r2) ->
      let relabel = coerce (liftLeibniz1of2 (lowerLeibniz (x <<< symm y)))
       in Branch
            (mkExists
	            (B x
                (applyBalanced f l1 (relabel l2))
	              (applyBalanced f r1 (relabel r2))))
applyBalanced _ (Leaf x _) (Branch b) =
  b # runExists \(B y _ _) ->
    refute (x <<< symm y)
applyBalanced _ (Branch b) (Leaf x _) =
  b # runExists \(B y _ _) ->
    refute (x <<< symm y)

main :: Effect Unit
main = logShow (applyBalanced (+) example example)
