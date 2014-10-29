{-# LANGUAGE MultiParamTypeClasses #-}
{- |
Module      :  Data.Syntax.Attoparsec.ByteString
Description :  Syntax instance for Attoparsec.ByteString.Parser.
Copyright   :  (c) Paweł Nowak
License     :  MIT

Maintainer  :  Paweł Nowak <pawel834@gmail.com>
Stability   :  experimental

Provides a Syntax instance for Attoparsec.ByteString.Parser.
-}
module Data.Syntax.Attoparsec.ByteString (
    -- * Parser.
    Parser,
    getParser
    ) where

import           Control.Applicative
import           Control.Lens.SemiIso
import           Control.Monad
import qualified Data.Attoparsec.ByteString as AP
import           Data.SemiIsoFunctor
import           Data.Syntax
import           Data.ByteString (ByteString)

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

instance Syntax Parser ByteString where
    anyChar = Parser AP.anyWord8
    char = Parser . void . AP.word8
    notChar = Parser . AP.notWord8
    satisfy = Parser . AP.satisfy
    string = Parser . void . AP.string
    take = Parser . AP.take
    takeWhile = Parser . AP.takeWhile
    takeWhile1 = Parser . AP.takeWhile1
    takeTill = Parser . AP.takeTill
