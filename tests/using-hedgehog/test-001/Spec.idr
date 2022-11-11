module Spec

import Data.Bounded

import Hedgehog

ints_eq_refl : Property
ints_eq_refl = property $ do
  x <- forAll $ int constantBounded
  assert $ x == x

main : IO ()
main = test
  [ "tests group" `MkGroup`
      [ ("ints equaliry reflexive", ints_eq_refl)
      ]
  ]
