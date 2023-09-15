module Test.Snapshot.TestSpec (class Assertable, TestSpec) where

import Prelude

import Effect.Aff (Aff)
import Test.Spec (SpecT)

class Assertable ∷ Type → Constraint
class (Eq a, Show a) ⇐ Assertable a

instance (Eq a, Show a) ⇒ Assertable a

type TestSpec = SpecT Aff Unit Aff Unit
