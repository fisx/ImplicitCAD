-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

{- The purpose of this file is to pass on the functionality we want
   to be accessible to an end user who is compiling objects using
   this haskell library. -}

module Graphics.Implicit
  ( -- * Types
    W.ℝ,
    W.ℝ2,
    W.ℝ3,
    SymbolicObj2 (),
    SymbolicObj3 (),
    W.ExtrudeMScale (C1, C2, Fn),

    -- * Shared operations
    P.Object (),
    P.translate,
    P.scale,
    P.mirror,
    P.complement,
    P.union,
    P.unionR,
    P.intersect,
    P.intersectR,
    P.difference,
    P.differenceR,
    P.implicit,
    P.shell,
    P.outset,
    P.emptySpace,
    P.fullSpace,
    P.withRounding,

    -- * 2D primitive shapes
    P.square,
    P.rect,
    P.circle,
    P.polygon,

    -- * 2D operations
    P.rotate,
    P.pack2,

    -- * 3D primitive shapes
    P.cube,
    P.rect3,
    P.sphere,
    P.cylinder,
    P.cylinder2,

    -- * 3D operations
    P.rotate3,
    P.rotate3V,
    P.pack3,

    -- * Extrusions into 3D
    P.extrude,
    P.extrudeM,
    P.extrudeOnEdgeOf,
    P.rotateExtrude,

    -- * OpenScad support
    E.runOpenscad,

    -- * 2D exporters
    writeSVG,
    writePNG2,
    writeDXF2,
    writeSCAD2,
    writeGCodeHacklabLaser,

    -- * 3D exporters
    writeSTL,
    writeBinSTL,
    writeOBJ,
    writeTHREEJS,
    writeSCAD3,
    writePNG3,

    -- * Linear re-exports
    L.V2 (V2),
    L.V3 (V3),
  )
where

-- The primitive objects, and functions for manipulating them.
-- MAYBEFIXME: impliment slice operation, regularPolygon and zsurface primitives.

-- The Extended OpenScad interpreter.

-- typesclasses and types defining the world, or part of the world.
import Graphics.Implicit.Definitions as W (ExtrudeMScale (C1, C2, Fn), SymbolicObj2, SymbolicObj3, ℝ, ℝ2, ℝ3)
-- Functions for writing files based on the result of operations on primitives.
import qualified Graphics.Implicit.Export as Export (writeBinSTL, writeDXF2, writeGCodeHacklabLaser, writeOBJ, writePNG, writeSCAD2, writeSCAD3, writeSTL, writeSVG, writeTHREEJS)
import Graphics.Implicit.ExtOpenScad as E (runOpenscad)
import Graphics.Implicit.Primitives as P (Object, circle, complement, cube, cylinder, cylinder2, difference, differenceR, emptySpace, extrude, extrudeM, extrudeOnEdgeOf, fullSpace, implicit, intersect, intersectR, mirror, outset, pack2, pack3, polygon, rect, rect3, rotate, rotate3, rotate3V, rotateExtrude, scale, shell, sphere, square, translate, union, unionR, withRounding)
import Linear as L (Quaternion (Quaternion), V2 (V2), V3 (V3))
import Prelude (FilePath, IO)

-- We want Export to be a bit less polymorphic
-- (so that types will collapse nicely)

writeSVG ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-2)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj2 ->
  IO ()
writeSVG =
     Export.writeSVG

writeDXF2 ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-2)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj2 ->
  IO ()
writeDXF2 = Export.writeDXF2

writeSTL ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-3)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj3 ->
  IO ()
writeSTL = Export.writeSTL

writeBinSTL ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-3)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj3 ->
  IO ()
writeBinSTL = Export.writeBinSTL

writeOBJ ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-3)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj3 ->
  IO ()
writeOBJ = Export.writeOBJ

writeSCAD2 ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-2)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj2 ->
  IO ()
writeSCAD2 = Export.writeSCAD2

writeSCAD3 ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-3)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj3 ->
  IO ()
writeSCAD3 = Export.writeSCAD3

writeTHREEJS ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-3)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj3 ->
  IO ()
writeTHREEJS = Export.writeTHREEJS

writeGCodeHacklabLaser ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-2)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj2 ->
  IO ()
writeGCodeHacklabLaser = Export.writeGCodeHacklabLaser

writePNG2 ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-2)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj2 ->
  IO ()
writePNG2 = Export.writePNG

-- | Export a PNG of the 'SymbolicObj3'. The projection is with a front-facing
-- camera, so the coordinate system is @(left to right, front to back, down to
-- up)@.
writePNG3 ::
  -- | Rendering resolution, in millimeters. Smaller values produce
  -- exports more faithful to the implicit model, at the expense of
  -- taking /O(n^-3)/ more time.
  ℝ ->
  FilePath ->
  SymbolicObj3 ->
  IO ()
writePNG3 = Export.writePNG
