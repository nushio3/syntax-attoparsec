{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      :  Data.Syntax.Attoparsec.Text
Description :  Syntax instance for Attoparsec.Text.Parser.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Provides a Syntax instance for Attoparsec.Text.Parser.
-}
module Data.Syntax.Attoparsec.Text (
    Parser(..)
    ) where

import           Control.Applicative
import           Control.Lens.SemiIso
import           Control.Monad
import qualified Data.Attoparsec.Text as AP
import           Data.SemiIsoFunctor
import           Data.Syntax
import           Data.Text (Text)

-- | A wrapped 'Data.Attoparsec.Text.Parser'.
newtype Parser a = Parser {
    -- | Extracts the parser.
    getParser :: AP.Parser a
}

instance SemiIsoFunctor Parser where
    simapCo ai (Parser p) = Parser $ p >>= either fail return . apply ai

instance SemiIsoApply Parser where
    sipure ai = Parser $ either fail pure (unapply ai ())
    (Parser x) /*/ (Parser y) = Parser $ (,) <$> x <*> y

instance SemiIsoAlternative Parser where
    siempty = Parser empty
    (Parser x) /|/ (Parser y) = Parser $ x <|> y

instance Syntax Parser Text where
    anyChar = Parser AP.anyChar
    char = Parser . void . AP.char
    notChar = Parser . AP.notChar
    satisfy = Parser . AP.satisfy
    string = Parser . void . AP.string
    take = Parser . AP.take
    takeWhile = Parser . AP.takeWhile
    takeWhile1 = Parser . AP.takeWhile1
    takeTill = Parser . AP.takeTill
