{-# LANGUAGE
    TypeOperators
  , FlexibleContexts
  , ScopedTypeVariables
  #-}
-------------------------------------------------------------------------------
-- |
-- Module : Generics.Regular.XmlPickler.Function
-- Copyright : (c) 2009, typLAB
-- License : BSD3
--
-- Maintainer : typLAB <code@typlab.com>
-- Stability : Experimental
--
-- Generic XmlPickler. Use this module if you don't want the instances
-- from Generics.Regular.XmlPickler.Instances.
--
-------------------------------------------------------------------------------
module Generics.Regular.XmlPickler.Function (gxpickle, GXmlPickler(..)) where

import Data.Char (toLower)
import Generics.Regular
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle.Xml
import Text.XML.HXT.Arrow.Pickle.Schema

-- | The generic XmlPickler class. This gives generic xml picklers for
-- the functors from 'Generics.Regular'. These are usually not used
-- directly.
class GXmlPickler f where
  gxpicklef :: PU a -> PU (f a)

instance GXmlPickler I where
  gxpicklef pu = (xpWrap (I, unI) pu) { theSchema = ElemRef "data" }

instance XmlPickler a => GXmlPickler (K a) where
  gxpicklef _ = (K, unK) `xpWrap` xpickle

instance GXmlPickler U where
  gxpicklef _ = (const U, const ()) `xpWrap` xpUnit

instance (GXmlPickler f, GXmlPickler g) => GXmlPickler (f :+: g) where
  gxpicklef f = gxpicklef f `xpSum` gxpicklef f

instance (GXmlPickler f, GXmlPickler g) => GXmlPickler (f :*: g) where
  gxpicklef f = (uncurry (:*:), \(a :*: b) -> (a, b)) `xpWrap` (gxpicklef f `xpPair` gxpicklef f)

instance (Constructor c, GXmlPickler f) => GXmlPickler (C c f) where
  gxpicklef f = xpElem (formatElement $ conName (undefined :: C c f r)) ((C, unC) `xpWrap` (gxpicklef f))

instance (Selector s, GXmlPickler f) => GXmlPickler (S s f) where
  gxpicklef f = xpElem (formatElement $ selName (undefined :: S s f r)) ((S, unS) `xpWrap` gxpicklef f)

-- | The generic pickler. Uses a tag for each constructor with the
-- lower case constructor name, and a tag for each record field with
-- the lower case field name. Most values are pickled using their own
-- 'XmlPickler' instance, and 'String's are pickled as possibly empty
-- text nodes.
gxpickle :: (Regular a, GXmlPickler (PF a)) => PU a
gxpickle = (to, from) `xpWrap` gxpicklef gxpickle

-- * Pickling combinators

-- | Combine two picklers into a pickler for 'Either'. While pickling,
-- check if the either is a 'Left' or 'Right' and use the appropriate
-- pickler. During unpickling, first try the first, and if it fails,
-- try the second.
xpEither :: PU a -> PU b -> PU (Either a b)
xpEither ~(PU fl tl sa) ~(PU fr tr sb) = PU
  (\x st -> case x of
              Left  y -> fl y st
              Right y -> fr y st)
  (UP $ \x -> case runUP tl x of
                -- When the first fails with error message es, try the second
                (Left (es, _), _) ->
                  case runUP tr x of
                    (Left (es', _), st)  -> (Left (es ++ "\n" ++ es', st), st)
                    (Right r, st) -> (Right (Right r), st)
                (Right r, st) -> (Right (Left r), st))
  (sa `scAlt` sb)

xpSum :: PU (f r) -> PU (g r) -> PU ((f :+: g) r)
xpSum l r = (i, o) `xpWrap` xpEither l r
  where
    i (Left  x) = L x
    i (Right x) = R x
    o (L x) = Left x
    o (R x) = Right x

formatElement :: String -> String
formatElement = headToLower
              . stripLeadingAndTrailingUnderscore

headToLower :: String -> String
headToLower []     = []
headToLower (x:xs) = toLower x : xs

stripLeadingAndTrailingUnderscore :: String -> String
stripLeadingAndTrailingUnderscore = stripLeadingUnderscore
                                  . stripTrailingUnderscore

stripLeadingUnderscore :: String -> String
stripLeadingUnderscore ('_':ls) = ls
stripLeadingUnderscore ls       = ls

stripTrailingUnderscore :: String -> String
stripTrailingUnderscore ""         = ""
stripTrailingUnderscore (x:'_':[]) = [x]
stripTrailingUnderscore (x:xs)     = x : stripTrailingUnderscore xs
