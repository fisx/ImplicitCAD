-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE

-- An executor, which parses openscad code, and executes it.
module Graphics.Implicit.ExtOpenScad (runOpenscad) where

import Control.Monad.State.Lazy (runStateT)
import Data.Foldable (traverse_)
import Data.Text.Lazy (pack)
import Graphics.Implicit.Definitions (SymbolicObj2, SymbolicObj3)
import Graphics.Implicit.ExtOpenScad.Definitions (CompState (CompState, messages, oVals, scadVars), Message (Message), MessageType (SyntaxError), ScadOpts, StatementI, VarLookup)
import Graphics.Implicit.ExtOpenScad.Eval.Constant (addConstants)
import Graphics.Implicit.ExtOpenScad.Eval.Statement (runStatementI)
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)
import Graphics.Implicit.ExtOpenScad.Parser.Util (sourcePosition)
import Graphics.Implicit.ExtOpenScad.Util.OVal (divideObjs)
import System.Directory (getCurrentDirectory)
import Text.Parsec.Error (ParseError, errorMessages, errorPos, showErrorMessages)
import Prelude (Applicative, Bool (True), IO, String, either, pure, ($), (.), (<$>))

-- | Small wrapper of our parser to handle parse errors, etc.
runOpenscad :: ScadOpts -> [String] -> String -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
runOpenscad scadOpts constants source = do
  (initialObjects, initialMessages) <- addConstants constants True
  let err :: Applicative f => ParseError -> f (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
      err e = pure (initialObjects, [], [], mesg e : initialMessages)
      run :: [StatementI] -> IO (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
      run sts =
        rearrange <$> do
          let sts' = traverse_ runStatementI sts
          path <- getCurrentDirectory
          runStateT sts' $ CompState initialObjects [] path initialMessages scadOpts
  either err run $ parseProgram "" source
  where
    rearrange :: ((), CompState) -> (VarLookup, [SymbolicObj2], [SymbolicObj3], [Message])
    rearrange (_, s) =
      let (obj2s, obj3s, _) = divideObjs $ oVals s
       in (scadVars s, obj2s, obj3s, messages s)
    show' = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . errorMessages
    mesg e = Message SyntaxError (sourcePosition $ errorPos e) $ pack $ show' e
