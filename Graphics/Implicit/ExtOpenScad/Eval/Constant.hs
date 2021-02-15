-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

module Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants, runExpr) where

import Control.Monad.State (liftIO, runStateT, (>>=))
import Data.Foldable (foldlM, traverse_)
import Data.Text.Lazy (pack)
import Graphics.Implicit.Definitions (Fastℕ)
import Graphics.Implicit.ExtOpenScad.Default (defaultObjects)
import Graphics.Implicit.ExtOpenScad.Definitions
  ( CompState (CompState, messages, scadVars),
    Expr,
    Message (Message),
    MessageType (SyntaxError),
    OVal (OUndefined),
    Pattern,
    ScadOpts (ScadOpts),
    SourcePosition (SourcePosition),
    StateC,
    VarLookup,
    varUnion,
  )
import Graphics.Implicit.ExtOpenScad.Eval.Expr (evalExpr, matchPat, rawRunExpr)
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchTok)
import Graphics.Implicit.ExtOpenScad.Parser.Util (patternMatcher)
import Graphics.Implicit.ExtOpenScad.Util.StateC (addMessage, modifyVarLookup)
import System.Directory (getCurrentDirectory)
import Text.Parsec (ParseError, SourceName, parse)
import Text.Parsec.Error (errorMessages, showErrorMessages)
import Prelude (Bool (False), Either, IO, String, either, pure, ($), (+), (.), (<$>), (<*), (<*>))

-- | Define variables used during the extOpenScad run.
addConstants :: [String] -> Bool -> IO (VarLookup, [Message])
addConstants constants withCSG = do
  path <- getCurrentDirectory
  (_, s) <- liftIO . runStateT (execAssignments constants) $ CompState (defaultObjects withCSG) [] path [] opts
  pure (scadVars s, messages s)
  where
    opts = ScadOpts False False
    show' = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
    execAssignments :: [String] -> StateC Fastℕ
    execAssignments = foldlM execAssignment 0
    execAssignment :: Fastℕ -> String -> StateC Fastℕ
    execAssignment count assignment = do
      let pos = SourcePosition count 1 "cmdline_constants"
          err = addMessage SyntaxError pos . pack . show'
          run (k, e) = evalExpr pos e >>= traverse_ (modifyVarLookup . varUnion) . matchPat k
      either err run $ parseAssignment "cmdline_constant" assignment
      pure $ count + 1
    parseAssignment :: SourceName -> String -> Either ParseError (Pattern, Expr)
    parseAssignment = parse $ (,) <$> patternMatcher <* matchTok '=' <*> expr0

-- | Evaluate an expression.
runExpr :: String -> Bool -> (OVal, [Message])
runExpr expression withCSG = do
  either oUndefined run $ parse expr0 "raw_expression" expression
  where
    run expr = rawRunExpr initPos (defaultObjects withCSG) expr
    initPos = SourcePosition 1 1 "raw_expression"
    show' = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
    oUndefined e = (OUndefined, [Message SyntaxError initPos $ pack $ show' e])
