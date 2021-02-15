-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright 2014 2015 2016, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE
-- Allow us to use a shorter form of Name.
{-# LANGUAGE PatternSynonyms #-}

-- The entry point for parsing an ExtOpenScad program.
module Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram) where

-- the top level of the expression parser.

-- The lexer.

-- We use parsec to parse.

import Control.Applicative ((<*), (<|>))
import Data.Functor (($>))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Text.Lazy (Text, pack)
import Graphics.Implicit.ExtOpenScad.Definitions (Expr (LamE), SourcePosition, Statement (DoNothing, If, Include, ModuleCall, NewModule, (:=)), StatementI (StatementI), Symbol (Symbol))
import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Pattern (Name))
import Graphics.Implicit.ExtOpenScad.Parser.Expr (expr0)
import Graphics.Implicit.ExtOpenScad.Parser.Lexer (matchComma, matchElse, matchFunction, matchIdentifier, matchIf, matchInclude, matchModule, matchSemi, matchTok, matchUse, surroundedBy, whiteSpace)
import Graphics.Implicit.ExtOpenScad.Parser.Util (patternMatcher, sourcePosition, (*<|>))
import Text.Parsec (ParseError, SourceName, between, char, eof, getPosition, many, noneOf, oneOf, option, optionMaybe, parse, sepBy, (<?>))
import Text.Parsec.String (GenParser)
import Prelude (Bool (False, True), Char, Either, String, filter, flip, fmap, not, pure, ($), (*>), (.), (<$), (<$>), (<*>))

-- Let us use the old syntax when defining Names.
pattern Name :: Text -> GIED.Pattern
pattern Name n = GIED.Name (Symbol n)

data CompIdx = A1 | A2

-- | all of the token parsers are lexemes which consume all trailing spaces nicely.
-- | This leaves us to deal only with the first spaces in the file.
parseProgram :: SourceName -> String -> Either ParseError [StatementI]
parseProgram = parse program
  where
    program :: GenParser Char st [StatementI]
    program = removeNoOps <$> (whiteSpace *> many (computation A1) <* eof)

-- | A computable block of code in our openscad-like programming language.
computation :: CompIdx -> GenParser Char st StatementI
computation A1 =
  computation A2
    <|> throwAway
computation A2 =
  -- suite statements: no semicolon...
  userModule
    <|> ifStatementI
    <|> userModuleDeclaration
    <|> ( include -- Non suite statements. Semicolon needed...
            <|> function
        )
      <* matchSemi
    *<|> assignment <* matchSemi

-- | A suite of s!
--   What's a suite? Consider:
--
--      union() {
--         sphere(3);
--      }
--
--  The suite was in the braces ({}). Similarily, the
--  following has the same suite:
--
--      union() sphere(3);
--
--  We consider it to be a list of computables which
--  are in turn StatementI s.
suite :: GenParser Char st [StatementI]
suite =
  ( removeNoOps . (: []) <$> computation A1
      *<|> removeNoOps <$> surroundedBy '{' (many (computation A1)) '}'
  )
    <?> "suite"

-- | Every StatementI requires a source position, thus we can build a combinator.
statementI :: GenParser Char st (Statement StatementI) -> GenParser Char st StatementI
statementI p = StatementI <$> sourcePos <*> p

-- | Commenting out a computation: use % or * before the statement, and it will not be run.
throwAway :: GenParser Char st StatementI
throwAway = statementI $ DoNothing <$ oneOf "%*" <* whiteSpace <* computation A2

-- | An include! Basically, inject another extopenscad file here...
include :: GenParser Char st StatementI
include = statementI p <?> "include/use"
  where
    p :: GenParser Char st (Statement StatementI)
    p =
      flip Include
        <$> (matchInclude $> True <|> matchUse $> False)
        -- FIXME: better definition of valid filename characters.
        <*> (pack <$> between (char '<') (matchTok '>') (many $ noneOf "<> "))

-- | An assignment (parser)
assignment :: GenParser Char st StatementI
assignment = statementI p <?> "assignment"
  where
    p :: GenParser Char st (Statement StatementI)
    p = (:=) <$> patternMatcher <* matchTok '=' <*> expr0

-- | A function declaration (parser)
function :: GenParser Char st StatementI
function = statementI p <?> "function"
  where
    p :: GenParser Char st (Statement StatementI)
    p = (:=) <$> lval <*> rval
    lval :: GenParser Char st GIED.Pattern
    lval = Name . pack <$> (matchFunction *> matchIdentifier)
    rval :: GenParser Char st Expr
    rval = LamE <$> surroundedBy '(' (sepBy patternMatcher matchComma) ')' <*> (matchTok '=' *> expr0)

-- | An if statement (parser)
ifStatementI :: GenParser Char st StatementI
ifStatementI = statementI p <?> "if"
  where
    p :: GenParser Char st (Statement StatementI)
    p = If <$> (matchIf *> surroundedBy '(' expr0 ')') <*> suite <*> option [] (matchElse *> suite)

-- | parse a call to a module.
userModule :: GenParser Char st StatementI
userModule = statementI p <?> "module call"
  where
    p :: GenParser Char st (Statement StatementI)
    p = ModuleCall <$> fmap (Symbol . pack) matchIdentifier <*> moduleArgsUnit <*> (suite *<|> (matchSemi $> []))

-- | declare a module.
userModuleDeclaration :: GenParser Char st StatementI
userModuleDeclaration = statementI p <?> "module declaration"
  where
    p :: GenParser Char st (Statement StatementI)
    p = NewModule <$> fmap (Symbol . pack) (matchModule *> matchIdentifier) <*> moduleArgsUnitDecl <*> suite

-- | parse the arguments passed to a module.
moduleArgsUnit :: GenParser Char st [(Maybe Symbol, Expr)]
moduleArgsUnit =
  surroundedBy
    '('
    ( sepBy
        ( do
            -- eg. a = 12
            symb <- matchIdentifier
            expr <- matchTok '=' *> expr0
            pure (Just (Symbol $ pack symb), expr)
            *<|> do
              -- eg. a(x,y) = 12
              symb <- matchIdentifier
              argVars <- surroundedBy '(' (sepBy matchIdentifier matchComma) ')'
              expr <- matchTok '=' *> expr0
              pure (Just (Symbol $ pack symb), LamE (fmap (Name . pack) argVars) expr)
            *<|> do
              -- eg. 12
              expr <- expr0
              pure (Nothing, expr)
        )
        matchComma
    )
    ')'

-- | parse the arguments in the module declaration.
moduleArgsUnitDecl :: GenParser Char st [(Symbol, Maybe Expr)]
moduleArgsUnitDecl =
  surroundedBy
    '('
    ( sepBy
        ( do
            symb <- matchIdentifier
            expr <- optionMaybe (matchTok '=' *> expr0)
            pure (Symbol $ pack symb, expr)
        )
        matchComma
    )
    ')'

-- | Find the source position. Used when generating errors.
sourcePos :: GenParser Char st SourcePosition
sourcePos = sourcePosition <$> getPosition

isNoOp :: StatementI -> Bool
isNoOp (StatementI _ DoNothing) = True
isNoOp _ = False

-- | Remove statements that do nothing.
removeNoOps :: [StatementI] -> [StatementI]
removeNoOps = filter $ not . isNoOp
