{-# LANGUAGE TypeFamilies #-}
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
    getParser,
    getParser_
    ) where

import           Control.Arrow (Kleisli(..))
import           Control.Category
import           Control.Monad
import           Control.SIArrow
import qualified Data.Attoparsec.Text as AP
import           Data.Scientific
import           Data.Syntax
import           Data.Syntax.Char
import           Data.Text (Text)
import Prelude hiding (id, (.))

-- | A wrapped 'Data.Attoparsec.Text.Parser'.
newtype WrappedParser a b = Wrapped (Kleisli AP.Parser a b)
    deriving (Category, Products, Coproducts, CategoryPlus, SIArrow)

wrap :: AP.Parser b -> WrappedParser a b
wrap = Wrapped . Kleisli . const

instance Syntax WrappedParser where
    type Seq WrappedParser = Text
    anyChar = wrap AP.anyChar
    char = wrap . void . AP.char
    notChar = wrap . AP.notChar
    satisfy = wrap . AP.satisfy
    string = wrap . void . AP.string
    take = wrap . AP.take
    takeWhile = wrap . AP.takeWhile
    takeWhile1 = wrap . AP.takeWhile1
    takeTill = wrap . AP.takeTill

instance SyntaxChar WrappedParser where
    decimal = wrap AP.decimal
    hexadecimal = wrap AP.hexadecimal
    realFloat = wrap $ fmap toRealFloat AP.scientific
    scientific = wrap AP.scientific

-- | Extracts the parser.
getParser :: WrappedParser a b -> a -> AP.Parser b
getParser (Wrapped (Kleisli f)) = f

-- | Extracts the parser.
getParser_ :: WrappedParser () b -> AP.Parser b
getParser_ (Wrapped (Kleisli f)) = f ()
