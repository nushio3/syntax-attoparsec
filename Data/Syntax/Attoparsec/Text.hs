{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    WrappedParser,
    getParser
    ) where

import           Control.Monad
import qualified Data.Attoparsec.Text as AP
import           Data.SemiIsoFunctor
import           Data.SemiIsoFunctor.Wrapped
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Text (Text)

-- | A wrapped 'Data.Attoparsec.Text.Parser'.
newtype WrappedParser a = Wrapped (WrappedCovariant AP.Parser a)
    deriving (SemiIsoFunctor, SemiIsoApply, SemiIsoAlternative, SemiIsoMonad)

pattern Parser a = Wrapped (WrappedCovariant a)

instance Syntax WrappedParser Text where
    anyChar = Parser AP.anyChar
    char = Parser . void . AP.char
    notChar = Parser . AP.notChar
    satisfy = Parser . AP.satisfy
    string = Parser . void . AP.string
    take = Parser . AP.take
    takeWhile = Parser . AP.takeWhile
    takeWhile1 = Parser . AP.takeWhile1
    takeTill = Parser . AP.takeTill

instance SyntaxChar WrappedParser Text where
    decimal = Parser AP.decimal
    scientific = Parser AP.scientific

-- | Extracts the parser.
getParser :: WrappedParser a -> AP.Parser a
getParser (Parser m) = m
