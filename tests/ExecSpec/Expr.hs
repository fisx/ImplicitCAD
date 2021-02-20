-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module ExecSpec.Expr (exprExec) where

-- Be explicit about what we import.

-- Hspec, for writing specs.

-- The type used for variables, in ImplicitCAD.

-- Our utility library, for making these tests easier to read.
import ExecSpec.Util (num, (-->))
import Graphics.Implicit.Definitions (ℝ)
import Test.Hspec (Spec, describe, it)
import Prelude (($))

-- Default all numbers in this file to being of the type ImplicitCAD uses for values.
default (ℝ)

exprExec :: Spec
exprExec =
  describe "arithmatic" $ do
    it "performs simple addition" $
      "1+1" --> num 2
    it "performs multiple addition" $
      "1+1+1" --> num 3
