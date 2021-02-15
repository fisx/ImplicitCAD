-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014-2019, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE
-- Use existing instances for the wrapped types rather than manually manking them
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Implicit.FastIntUtil (Fastℕ (Fastℕ), toFastℕ, fromFastℕ) where

import Prelude (Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, id)

class FastN n where
  fromFastℕ :: Fastℕ -> n
  toFastℕ :: n -> Fastℕ

instance FastN Int where
  fromFastℕ (Fastℕ a) = a
  {-# INLINEABLE fromFastℕ #-}
  toFastℕ = Fastℕ
  {-# INLINEABLE toFastℕ #-}

instance FastN Fastℕ where
  fromFastℕ = id
  {-# INLINEABLE fromFastℕ #-}
  toFastℕ = id
  {-# INLINEABLE toFastℕ #-}

-- System integers, meant to go fast, and have no chance of wrapping 2^31.
newtype Fastℕ = Fastℕ Int
  deriving (Show, Read, Eq, Ord, Num, Enum, Integral, Real)
