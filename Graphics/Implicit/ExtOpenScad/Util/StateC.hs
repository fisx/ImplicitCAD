-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Util.StateC (addMessage, getVarLookup, modifyVarLookup, lookupVar, pushVals, getVals, putVals, withPathShiftedBy, getPath, getRelPath, errorC, warnC, scadOptions) where

import Control.Monad.State (gets, modify)
import Data.Map (lookup)
import Data.Text.Lazy (Text)
import Graphics.Implicit.ExtOpenScad.Definitions (CompState (messages, oVals, scadOpts, scadVars, sourceDir), Message (Message), MessageType (Error, Warning), OVal, ScadOpts, SourcePosition, StateC, Symbol, VarLookup (VarLookup))
import System.FilePath ((</>))
import Prelude (FilePath, Maybe, pure, ($), (<>))

getVarLookup :: StateC VarLookup
getVarLookup = gets scadVars

modifyVarLookup :: (VarLookup -> VarLookup) -> StateC ()
modifyVarLookup f = modify $ \c -> c {scadVars = f $ scadVars c}

-- | Perform a variable lookup
--   FIXME: generate a warning when we look up a variable that is not present.
lookupVar :: Symbol -> StateC (Maybe OVal)
lookupVar name = do
  (VarLookup varlookup) <- getVarLookup
  pure $ lookup name varlookup

pushVals :: [OVal] -> StateC ()
pushVals vals = modify $ \c -> c {oVals = vals <> oVals c}

getVals :: StateC [OVal]
getVals = gets oVals

putVals :: [OVal] -> StateC ()
putVals vals = modify $ \c -> c {oVals = vals}

withPathShiftedBy :: FilePath -> StateC a -> StateC a
withPathShiftedBy pathShift s = do
  path <- getPath
  modify $ \c -> c {sourceDir = path </> pathShift}
  x <- s
  modify $ \c -> c {sourceDir = path}
  pure x

-- | Pure the path stored in the state.
getPath :: StateC FilePath
getPath = gets sourceDir

getRelPath :: FilePath -> StateC FilePath
getRelPath relPath = do
  path <- getPath
  pure $ path </> relPath

addMesg :: Message -> StateC ()
addMesg m = modify $ \c -> c {messages = messages c <> pure m}

addMessage :: MessageType -> SourcePosition -> Text -> StateC ()
addMessage mtype pos text = addMesg $ Message mtype pos text

errorC :: SourcePosition -> Text -> StateC ()
errorC = addMessage Error
{-# INLINEABLE errorC #-}

warnC :: SourcePosition -> Text -> StateC ()
warnC = addMessage Warning
{-# INLINEABLE warnC #-}

scadOptions :: StateC ScadOpts
scadOptions = gets scadOpts
