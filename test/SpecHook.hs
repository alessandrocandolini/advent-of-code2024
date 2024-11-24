module SpecHook where

import Test.Hspec ( parallel, Spec )

hook :: Spec -> Spec
hook = parallel