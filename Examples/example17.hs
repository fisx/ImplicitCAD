-- Example 17, pulled from our benchmarking suite.

import Graphics.Implicit (rect3R, translate, union, writeSTL)
import Graphics.Implicit.Definitions (Fastℕ, SymbolicObj3, ℝ, ℝ3)
import Prelude (fmap, fromIntegral, zip3, ($), (*), (/), (<$>))

default (Fastℕ, ℝ)

object2 :: SymbolicObj3
object2 = squarePipe (10, 10, 10) 1 100
  where
    squarePipe :: ℝ3 -> ℝ -> ℝ -> SymbolicObj3
    squarePipe (x, y, z) diameter precision =
      union
        ( ( \start ->
              translate start $
                cubeR 0 False (diameter, diameter, diameter)
          )
            <$> zip3
              (fmap (\n -> (fromIntegral n / precision) * x) [0 .. 100])
              (fmap (\n -> (fromIntegral n / precision) * y) [0 .. 100])
              (fmap (\n -> (fromIntegral n / precision) * z) [0 .. 100])
        )

main = writeSTL 1 "example17.stl" object2
