{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
-------------------------------------------------------------------------------
-- |
-- Module : Generics.Regular.XmlPickler
-- Copyright : (c) 2009, typLAB
-- License : BSD3
--
-- Maintainer : typLAB <code@typlab.com>
-- Stability : Experimental
--
-- 'XmlPickler' instance for 'Bool' which converts to and from the Strings
-- \"true\" and \"false\", and 'GXmlPickler' instance for 'K' 'String', which
-- allows whitespace. These instances are automatically used if you
-- import 'Generics.Regular.XmlPickler'.
--
-------------------------------------------------------------------------------
module Generics.Regular.XmlPickler.Instances() where

import Data.Text
import Generics.Regular
import Generics.Regular.XmlPickler.Function
import Text.XML.HXT.Arrow.Pickle

-- * Boolean instance for XmlPickler.

instance XmlPickler Bool where
  xpickle = (toBool, fromBool) `xpWrap` xpText

toBool :: String -> Bool
toBool "true"  = True
toBool "false" = False
toBool _       = error "No parse for bool in toBool (XmlPickler)."

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
