{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
    FlexibleInstances
  , OverlappingInstances
  , ScopedTypeVariables
  #-}
-- | 'XmlPickler' instance for 'Bool' which converts to and from the Strings
-- \"true\" and \"false\", and 'GXmlPickler' instance for 'K' 'String', which
-- allows whitespace. These instances are automatically used if you
-- import 'Generics.Regular.XmlPickler'.
module Generics.Regular.XmlPickler.Instances () where

import Data.Char (toLower)
import Data.Text (Text, pack, unpack)
import Generics.Regular
import Text.XML.HXT.Arrow.Pickle

import Generics.Regular.XmlPickler.Function

-- * Boolean instance for XmlPickler.

instance XmlPickler Bool where
  xpickle = (toBool, fromBool) `xpWrapEither` xpText

toBool :: String -> Either String Bool
toBool k | k' == "yes"  = Right True
         | k' == "true" = Right True
         | k' == "on"   = Right True
  where k' = map toLower k
toBool k | k' == "no"    = Right False
         | k' == "false" = Right False
         | k' == "off"   = Right False
  where k' = map toLower k
toBool k                 = Left ("XmlPickler Bool: unexpected value: " ++ k)

fromBool :: Bool -> String
fromBool True  = "true"
fromBool False = "false"

-- * Either instance for XmlPickler.

instance (XmlPickler a, XmlPickler b) => XmlPickler (Either a b) where
  xpickle = xpEither xpickle xpickle

-- * GXmlPickler instance for String, Text and Maybes.

instance GXmlPickler (K String) where
  gxpicklef _ = (K, unK) `xpWrap` xpText0

instance GXmlPickler (K Text) where
  gxpicklef _ = (K . pack, unpack . unK) `xpWrap` xpText0

instance (XmlPickler a, Selector s) => GXmlPickler (S s (K (Maybe a))) where
  gxpicklef _ = (S . K, unK . unS)
         `xpWrap` xpOption (xpElem (formatElement $ selName (undefined :: S s f r)) xpickle)

instance Selector s => GXmlPickler (S s (K (Maybe String))) where
  gxpicklef _ = (S . K, unK . unS)
         `xpWrap` xpOption (xpElem (formatElement $ selName (undefined :: S s f r)) xpText0)

instance Selector s => GXmlPickler (S s (K (Maybe Text))) where
  gxpicklef _ = (S . K . fmap pack, fmap unpack . unK . unS)
         `xpWrap` xpOption (xpElem (formatElement $ selName (undefined :: S s f r)) xpText0)
