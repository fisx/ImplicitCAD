-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE
-- FIXME: required. why?
{-# LANGUAGE ViewPatterns #-}

module Graphics.Implicit.ExtOpenScad.Util.OVal (OTypeMirror, (<||>), fromOObj, toOObj, divideObjs, caseOType, oTypeStr, getErrors) where

import Control.Monad (msum)
-- for some minimal paralellism.
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Data.Maybe (fromMaybe, maybe)
import Data.Text.Lazy (Text)
import Data.Traversable (traverse)
import Graphics.Implicit.Definitions (ExtrudeMScale (C1, C2, Fn), SymbolicObj2, SymbolicObj3, V2, fromℕtoℝ, ℕ, ℝ, ℝ2)
import Graphics.Implicit.ExtOpenScad.Definitions (OVal (OBool, OError, OFunc, OList, ONModule, ONum, OObj2, OObj3, OString, OUModule, OUndefined, OVargsModule))
-- To build vectors of ℝs.
import Linear (V2 (V2), V3 (V3))
import Prelude (Bool (False, True), Either (Left, Right), Maybe (Just, Nothing), error, filter, flip, floor, fmap, fromInteger, not, return, show, ($), (.), (<>), (==))

-- Convert OVals (and Lists of OVals) into a given Haskell type
class OTypeMirror a where
  fromOObj :: OVal -> Maybe a
  fromOObjList :: OVal -> Maybe [a]
  fromOObjList (OList list) = traverse fromOObj list
  fromOObjList _ = Nothing
  {-# INLINEABLE fromOObjList #-}
  toOObj :: a -> OVal

instance OTypeMirror OVal where
  fromOObj = Just
  {-# INLINEABLE fromOObj #-}
  toOObj a = a

instance OTypeMirror ℝ where
  fromOObj (ONum n) = Just n
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj = ONum

instance OTypeMirror ℕ where
  fromOObj (ONum n) = if n == fromInteger (floor n) then Just (floor n) else Nothing
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj = ONum . fromℕtoℝ

instance OTypeMirror Bool where
  fromOObj (OBool b) = Just b
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj = OBool

instance (OTypeMirror a) => OTypeMirror [a] where
  fromOObj = fromOObjList
  {-# INLINEABLE fromOObj #-}
  toOObj list = OList $ fmap toOObj list

instance OTypeMirror Text where
  fromOObj (OString str) = Just str
  fromOObj _ = Nothing
  toOObj a = OString a

instance (OTypeMirror a) => OTypeMirror (Maybe a) where
  fromOObj a = Just $ fromOObj a
  {-# INLINEABLE fromOObj #-}
  toOObj (Just a) = toOObj a
  toOObj Nothing = OUndefined

instance (OTypeMirror a, OTypeMirror b) => OTypeMirror (a, b) where
  fromOObj (OList [fromOObj -> Just a, fromOObj -> Just b]) = Just (a, b)
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj (a, b) = OList [toOObj a, toOObj b]

instance (OTypeMirror a) => OTypeMirror (V2 a) where
  fromOObj (OList [fromOObj -> Just a, fromOObj -> Just b]) = Just (V2 a b)
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj (V2 a b) = OList [toOObj a, toOObj b]

instance (OTypeMirror a, OTypeMirror b, OTypeMirror c) => OTypeMirror (a, b, c) where
  fromOObj (OList [fromOObj -> Just a, fromOObj -> Just b, fromOObj -> Just c]) =
    Just (a, b, c)
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj (a, b, c) = OList [toOObj a, toOObj b, toOObj c]

instance (OTypeMirror a) => OTypeMirror (V3 a) where
  fromOObj (OList [fromOObj -> Just a, fromOObj -> Just b, fromOObj -> Just c]) =
    Just (V3 a b c)
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj (V3 a b c) = OList [toOObj a, toOObj b, toOObj c]

instance (OTypeMirror a, OTypeMirror b) => OTypeMirror (a -> b) where
  fromOObj (OFunc f) = Just $ \input ->
    let oInput = toOObj input
        oOutput = f oInput
        output :: Maybe b
        output = fromOObj oOutput
     in fromMaybe
          ( error $
              "coercing OVal to a -> b isn't always safe; use a -> Maybe b"
                <> " (trace: "
                <> show oInput
                <> " -> "
                <> show oOutput
                <> " )"
          )
          output
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}
  toOObj f = OFunc $ \oObj ->
    case fromOObj oObj :: Maybe a of
      Nothing -> OError "bad input type"
      Just obj -> toOObj $ f obj

instance (OTypeMirror a, OTypeMirror b) => OTypeMirror (Either a b) where
  fromOObj (fromOObj -> Just (x :: a)) = Just $ Left x
  fromOObj (fromOObj -> Just (x :: b)) = Just $ Right x
  fromOObj _ = Nothing
  {-# INLINEABLE fromOObj #-}

  toOObj (Right x) = toOObj x
  toOObj (Left x) = toOObj x

instance OTypeMirror ExtrudeMScale where
  fromOObj (fromOObj -> Just (x :: ℝ)) = Just $ C1 x
  fromOObj (fromOObj -> Just (x :: ℝ2)) = Just $ C2 x
  fromOObj (fromOObj -> Just (x :: (ℝ -> Either ℝ ℝ2))) = Just $ Fn x
  fromOObj _ = Nothing

  toOObj (C1 x) = toOObj x
  toOObj (C2 x) = toOObj x
  toOObj (Fn x) = toOObj x

-- A string representing each type.
oTypeStr :: OVal -> Text
oTypeStr OUndefined = "Undefined"
oTypeStr (OBool _) = "Bool"
oTypeStr (ONum _) = "Number"
oTypeStr (OList _) = "List"
oTypeStr (OString _) = "String"
oTypeStr (OFunc _) = "Function"
oTypeStr (OUModule _ _ _) = "User Defined Module"
oTypeStr (ONModule _ _ _) = "Built-in Module"
oTypeStr (OVargsModule _ _) = "VargsModule"
oTypeStr (OError _) = "Error"
oTypeStr (OObj2 _) = "2D Object"
oTypeStr (OObj3 _) = "3D Object"

getErrors :: OVal -> Maybe Text
getErrors (OError er) = Just er
getErrors (OList l) = msum $ fmap getErrors l
getErrors _ = Nothing

caseOType :: a -> (a -> c) -> c
caseOType = flip ($)

infixr 2 <||>

(<||>) ::
  OTypeMirror desiredType =>
  (desiredType -> out) ->
  (OVal -> out) ->
  (OVal -> out)
(<||>) f g input =
  let coerceAttempt :: OTypeMirror desiredType => Maybe desiredType
      coerceAttempt = fromOObj input
   in maybe (g input) f coerceAttempt

-- separate 2d and 3d objects from a set of OVals.
divideObjs :: [OVal] -> ([SymbolicObj2], [SymbolicObj3], [OVal])
divideObjs children =
  runEval $ do
    obj2s <- rseq [x | OObj2 x <- children]
    obj3s <- rseq [x | OObj3 x <- children]
    objs <- rpar (filter (not . isOObj) children)
    return (obj2s, obj3s, objs)
  where
    isOObj (OObj2 _) = True
    isOObj (OObj3 _) = True
    isOObj _ = False
