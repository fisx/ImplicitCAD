module PropertySpec
  ( propSpec,
  )
where

import PropertySpec.Exec (additionSpec, divisionSpec, multiplicationSpec, subtractionSpec)
import Test.Hspec (Spec)

propSpec :: Spec
propSpec = do
  additionSpec
  subtractionSpec
  multiplicationSpec
  divisionSpec
