-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- This file symbolicaly renders contours and contour fillings.
-- If it can't, it passes the puck to a marching-squares-like
-- algorithm...

module Graphics.Implicit.Export.SymbolicObj2 (symbolicGetContour) where

import Graphics.Implicit.Definitions (Fastℕ, ObjectContext, Polyline (Polyline), SharedObj (Scale, Translate, WithRounding), SymbolicObj2 (Circle, Shared2, Square), fromFastℕtoℝ, objectRounding, ℝ, ℝ2, (⋯*))
import Graphics.Implicit.Export.Render (getContour)
import Linear (V2 (V2))
import Prelude (ceiling, cos, fmap, max, pi, pure, sin, ($), (*), (+), (/), (==))

symbolicGetContour :: ℝ -> ObjectContext -> SymbolicObj2 -> [Polyline]
symbolicGetContour _ ctx (Square (V2 dx dy))
  | objectRounding ctx == 0 = [Polyline [V2 0 0, V2 dx 0, V2 dx dy, V2 0 dy, V2 0 0]]
-- FIXME: magic number.
symbolicGetContour res _ (Circle r) =
  [ Polyline
      [ V2 (r * cos (2 * pi * fromFastℕtoℝ m / fromFastℕtoℝ n)) (r * sin (2 * pi * fromFastℕtoℝ m / fromFastℕtoℝ n))
        | m <- [0 .. n]
      ]
  ]
  where
    n :: Fastℕ
    n = max 5 $ ceiling $ 2 * pi * r / res
symbolicGetContour res ctx (Shared2 (WithRounding r obj)) = symbolicGetContour res (ctx {objectRounding = r}) obj
symbolicGetContour res ctx (Shared2 (Translate v obj)) = appOpPolylines (+ v) $ symbolicGetContour res ctx obj
symbolicGetContour res ctx (Shared2 (Scale s@(V2 a b) obj)) = appOpPolylines (⋯* s) $ symbolicGetContour (res / sc) ctx obj
  where
    sc = max a b
symbolicGetContour res _ obj = getContour (pure res) obj

appOpPolylines :: (ℝ2 -> ℝ2) -> [Polyline] -> [Polyline]
appOpPolylines op = fmap (appOpPolyline op)

appOpPolyline :: (ℝ2 -> ℝ2) -> Polyline -> Polyline
appOpPolyline op (Polyline xs) = Polyline $ fmap op xs
