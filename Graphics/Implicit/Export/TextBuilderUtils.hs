-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- This module exists to re-export a coherent set of functions to define
-- Data.Text.Lazy builders with.

module Graphics.Implicit.Export.TextBuilderUtils
  ( -- From Data.Text.Lazy
    module DTL,
    -- From Data.Text.Lazy.Builder
    module DTLB,
    toLazyText,
    -- some special case Builders.
    bf,
    buildTruncFloat,
    buildℕ,
    buildInt,
  )
where

import Data.Text.Internal.Lazy (defaultChunkSize)
import Data.Text.Lazy as DTL (Text, pack)
import Data.Text.Lazy.Builder as DTLB (Builder, fromLazyText, toLazyTextWith)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (FPFormat (Exponent, Fixed), formatRealFloat)
import Graphics.Implicit.Definitions (fromℝtoFloat, ℕ, ℝ)
import Prelude (Int, Maybe (Just, Nothing), ($))

-- The chunk size for toLazyText is very small (128 bytes), so we export
-- a version with a much larger size (~16 K)
toLazyText :: Builder -> Text
toLazyText = toLazyTextWith defaultChunkSize

-- | Serialize a value as a single precision float with an exponent attached.
bf :: ℝ -> Builder
bf value = formatRealFloat Exponent Nothing $ fromℝtoFloat value

-- | Serialize a float with four decimal places
buildTruncFloat :: ℝ -> Builder
buildTruncFloat = formatRealFloat Fixed $ Just 4

buildℕ :: ℕ -> Builder
buildℕ = decimal

buildInt :: Int -> Builder
buildInt = decimal
