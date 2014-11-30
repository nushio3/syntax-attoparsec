{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    WrappedParser,
    getParser
    ) where

import           Control.Monad
import qualified Data.Attoparsec.ByteString as AP
import           Data.ByteString (ByteString)
import           Data.SemiIsoFunctor
import           Data.SemiIsoFunctor.Wrapped
import           Data.Syntax

-- | A wrapped 'Data.Attoparsec.ByteString.Parser'.
newtype WrappedParser a = Wrapped (WrappedCovariant AP.Parser a)
    deriving (SemiIsoFunctor, SemiIsoApply, SemiIsoAlternative, SemiIsoMonad)

pattern Parser a = Wrapped (WrappedCovariant a)

instance Syntax WrappedParser ByteString where
    anyChar = Parser AP.anyWord8
    char = Parser . void . AP.word8
    notChar = Parser . AP.notWord8
    satisfy = Parser . AP.satisfy
    string = Parser . void . AP.string
    take = Parser . AP.take
    takeWhile = Parser . AP.takeWhile
    takeWhile1 = Parser . AP.takeWhile1
    takeTill = Parser . AP.takeTill

-- | Extracts the parser.
getParser :: WrappedParser a -> AP.Parser a
getParser (Parser a) = a
