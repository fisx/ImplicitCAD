-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Copyright (C) 2015 2016, Mike MacHenry (mike.machenry@gmail.com)
-- Released under the GNU AGPLV3+, see LICENSE
-- Allow us to use real types in the type constraints.
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Implicit.Export (writeObject, formatObject, writeSVG, writeSTL, writeBinSTL, writeOBJ, writeTHREEJS, writeGCodeHacklabLaser, writeDXF2, writeSCAD2, writeSCAD3, writePNG) where

-- The types of our objects (before rendering), and the type of the resolution to render with.

-- functions for outputing a file, and one of the types.

-- Import instances of DiscreteApproxable...

-- Output file formats.

import qualified Codec.Picture as ImageFormatCodecs (DynamicImage, savePngImage)
import qualified Data.ByteString.Lazy as LBS (writeFile)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Graphics.Implicit.Definitions (NormedTriangleMesh, Polyline, SymbolicObj2, SymbolicObj3, TriangleMesh, ℝ)
import Graphics.Implicit.Export.DiscreteAproxable (DiscreteAproxable, discreteAprox)
import qualified Graphics.Implicit.Export.NormedTriangleMeshFormats as NormedTriangleMeshFormats (obj)
import qualified Graphics.Implicit.Export.PolylineFormats as PolylineFormats (dxf2, hacklabLaserGCode, svg)
import qualified Graphics.Implicit.Export.SymbolicFormats as SymbolicFormats (scad2, scad3)
import qualified Graphics.Implicit.Export.TriangleMeshFormats as TriangleMeshFormats (binaryStl, jsTHREE, stl)
import Prelude (FilePath, IO, ($), (.))

-- | Write an object to a file with LazyText IO, using the given format writer function.
writeObject ::
  (DiscreteAproxable obj aprox) =>
  -- | Resolution
  ℝ ->
  -- | File Format Writer (Function that formats)
  (aprox -> Text) ->
  -- | File Name
  FilePath ->
  -- | Object to render
  obj ->
  -- | Writing Action!
  IO ()
writeObject res formatWriter filename obj =
  let aprox = formatObject res formatWriter obj
   in LT.writeFile filename aprox

-- | Serialize an object using the given format writer, which takes the filename and writes to it..
writeObject' ::
  (DiscreteAproxable obj aprox) =>
  -- | Resolution
  ℝ ->
  -- | File Format writer
  (FilePath -> aprox -> IO ()) ->
  -- | File Name
  FilePath ->
  -- | Object to render
  obj ->
  -- | Writing Action!
  IO ()
writeObject' res formatWriter filename obj =
  formatWriter filename (discreteAprox res obj)

-- | Serialize an object using the given format writer. No file target is implied.
formatObject ::
  (DiscreteAproxable obj aprox) =>
  -- | Resolution
  ℝ ->
  -- | File Format Writer (Function that formats)
  (aprox -> Text) ->
  -- | Object to render
  obj ->
  -- | Result
  Text
formatObject res formatWriter = formatWriter . discreteAprox res

writeSVG :: DiscreteAproxable obj [Polyline] => ℝ -> FilePath -> obj -> IO ()
writeSVG res = writeObject res PolylineFormats.svg

writeDXF2 :: DiscreteAproxable obj [Polyline] => ℝ -> FilePath -> obj -> IO ()
writeDXF2 res = writeObject res PolylineFormats.dxf2

writeSTL :: DiscreteAproxable obj TriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeSTL res = writeObject res TriangleMeshFormats.stl

writeBinSTL :: DiscreteAproxable obj TriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeBinSTL res file obj = LBS.writeFile file $ TriangleMeshFormats.binaryStl $ discreteAprox res obj

writeOBJ :: DiscreteAproxable obj NormedTriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeOBJ res = writeObject res NormedTriangleMeshFormats.obj

writeTHREEJS :: DiscreteAproxable obj TriangleMesh => ℝ -> FilePath -> obj -> IO ()
writeTHREEJS res = writeObject res TriangleMeshFormats.jsTHREE

writeGCodeHacklabLaser :: DiscreteAproxable obj [Polyline] => ℝ -> FilePath -> obj -> IO ()
writeGCodeHacklabLaser res = writeObject res PolylineFormats.hacklabLaserGCode

writeSCAD3 :: ℝ -> FilePath -> SymbolicObj3 -> IO ()
writeSCAD3 res filename obj = LT.writeFile filename $ SymbolicFormats.scad3 res obj

writeSCAD2 :: ℝ -> FilePath -> SymbolicObj2 -> IO ()
writeSCAD2 res filename obj = LT.writeFile filename $ SymbolicFormats.scad2 res obj

writePNG :: DiscreteAproxable obj ImageFormatCodecs.DynamicImage => ℝ -> FilePath -> obj -> IO ()
writePNG res = writeObject' res ImageFormatCodecs.savePngImage
