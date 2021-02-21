-- Allow us to use string literals for Text
{-# LANGUAGE OverloadedStrings #-}
-- Implicit CAD. Copyright (C) 2011, Christopher Olah (chris@colah.ca)
-- Copyright (C) 2014-2017, Julia Longtin (julial@turinglace.com)
-- Released under the GNU AGPLV3+, see LICENSE
-- Allow us to use shorter forms of Var and Name.
{-# LANGUAGE PatternSynonyms #-}

-- | Statement related hspec tests.
module ParserSpec.Statement (statementSpec) where

-- Parse an ExtOpenScad program.

import Data.Either (Either (Right))
import Data.Text.Lazy (Text)
import Graphics.Implicit.Definitions (Fastℕ)
import Graphics.Implicit.ExtOpenScad.Definitions (Expr (LamE, ListE, (:$)), Pattern (ListP), SourcePosition (SourcePosition), Statement (If, ModuleCall, NewModule, (:=)), StatementI (StatementI), Symbol (Symbol))
import qualified Graphics.Implicit.ExtOpenScad.Definitions as GIED (Expr (Var), Pattern (Name))
import Graphics.Implicit.ExtOpenScad.Parser.Statement (parseProgram)
import ParserSpec.Util (bool, index, minus, mult, num, plus)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Prelude (Bool (True), Maybe (Just, Nothing), String, ($))

-- Let us use the old syntax when defining Vars and Names.
pattern Var :: Text -> Expr
pattern Var s = GIED.Var (Symbol s)

pattern Name :: Text -> Pattern
pattern Name n = GIED.Name (Symbol n)

-- | an expectation that a string is equivalent to a statement.
infixr 1 -->

(-->) :: String -> [StatementI] -> Expectation
(-->) source stmts =
  parseProgram "noname" source `shouldBe` Right stmts

-- | an expectation that a string generates an error.
-- parsesAsError :: String -> Expectation
-- parsesAsError source = parseProgram "noname" source `shouldSatisfy` isLeft

-- | A single statement.
single :: Statement StatementI -> [StatementI]
single st = [StatementI (SourcePosition 1 1 "noname") st]

-- | A function call.
call :: Text -> Fastℕ -> [(Maybe Symbol, Expr)] -> [StatementI] -> StatementI
call name column args stmts = StatementI (SourcePosition 1 column "noname") (ModuleCall (Symbol name) args stmts)

-- | Test assignments.
assignmentSpec :: Spec
assignmentSpec = do
  it "handles assignment" $
    "y = -5 ; " --> single (Name "y" := num (-5))
  it "handles pattern matching" $
    "[ x , y ] = [ 1 , 2 ] ; " --> single (ListP [Name "x", Name "y"] := ListE [num 1, num 2])
  it "handles the function keyword" $
    "function foo ( x , y ) = x * y ; " --> single fooFunction
  it "handles function with let expression" $
    "function withlet ( b ) = let ( c = 5 ) b + c ; "
      --> single (Name "withlet" := LamE [Name "b"] (LamE [Name "c"] (plus [Var "b", Var "c"]) :$ [num 5]))
  it "handles nested indexing" $
    "x = [ y [ 0 ] - z * 2 ] ; "
      --> single
        ( Name "x"
            := ListE
              [ minus
                  [ index [Var "y", num 0],
                    mult [Var "z", num 2]
                  ]
              ]
        )
  where
    fooFunction :: Statement st
    fooFunction =
      Name "foo"
        := LamE
          [Name "x", Name "y"]
          (mult [Var "x", Var "y"])

-- | Test a simple if block.
ifSpec :: Spec
ifSpec = do
  it "parses" $
    "if ( true ) { a ( ) ; }"
      --> single (If (bool True) [call "a" 15 [] []] [])
  it "parses with else clause" $
    "if ( true ) { a ( ) ; } else {b();}"
      --> single (If (bool True) [call "a" 15 [] []] [call "b" 31 [] []])

-- | Our entry point. Test all of the statements.
statementSpec :: Spec
statementSpec = do
  describe "empty file" $
    it "returns an empty list" $ "" --> []
  describe "assignment" assignmentSpec
  describe "if" ifSpec
  describe "line comment" $
    it "parses as empty" $ "// foish bar \n " --> []
  describe "multiline comment" $
    it "parses as empty" $ "/* foish bar\n */ " --> []
  describe "module call" $
    it "parses" $ "foo ( ) ; " --> single (ModuleCall (Symbol "foo") [] [])
  describe "disabled module call" $
    it "parses as empty" $ "% foo ( ) ; " --> []
  describe "difference of two cylinders" $
    it "parses correctly" $
      "difference ( ) { cylinder ( r = 5 , h = 20 ) ;cylinder(r=2,h=20); } "
        --> single
          ( ModuleCall
              (Symbol "difference")
              []
              [ call
                  "cylinder"
                  18
                  [ (Just (Symbol "r"), num 5.0),
                    (Just (Symbol "h"), num 20.0)
                  ]
                  [],
                call
                  "cylinder"
                  47
                  [ (Just (Symbol "r"), num 2.0),
                    (Just (Symbol "h"), num 20.0)
                  ]
                  []
              ]
          )
  describe "module definition" $ do
    it "parses correctly" $
      "module foo_bar ( ) { }" --> single (NewModule (Symbol "foo_bar") [] [])
    it "accepts argument" $
      "module foo_bar ( x ) { }" --> single (NewModule (Symbol "foo_bar") [(Symbol "x", Nothing)] [])
    it "accepts argument with default" $
      "module foo_bar ( x = 1) { }" --> single (NewModule (Symbol "foo_bar") [(Symbol "x", Just $ num 1)] [])
    it "accepts split lines" $ do
      "module foo\n(\nbar\n)\n{}" --> single (NewModule (Symbol "foo") [(Symbol "bar", Nothing)] [])
  describe "identifiers" $ do
    it "accepts unicode" $
      "module 💩 () { }" --> single (NewModule (Symbol "💩") [] [])
