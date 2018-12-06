{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module ECharts.DeriveToJSVal (toJSVal_generic, ToJSVal(..)) where

import GHC.Generics
import qualified Data.Text as T

import Language.Javascript.JSaddle
-- import GHCJS.Marshal.Internal (toJSVal_generic)
import qualified Language.Javascript.JSaddle.Object as OI (Object(..), create, setProp)
import qualified JavaScript.Array.Internal as AI (SomeJSArray(..), create, push)

class GToJSVal a where
  gToJSVal :: (String -> String) -> Bool -> a -> JSM JSVal

class GToJSProp a where
  gToJSProp :: (String -> String) -> JSVal -> a -> JSM ()

class GToJSArr a where
  gToJSArr :: (String -> String) -> MutableJSArray -> a -> JSM ()

instance (ToJSVal b) => GToJSVal (K1 a b c) where
  gToJSVal _ _ (K1 x) = toJSVal x

instance GToJSVal p => GToJSVal (Par1 p) where
  gToJSVal f b (Par1 p) = gToJSVal f b p

instance GToJSVal (f p) => GToJSVal (Rec1 f p) where
  gToJSVal f b (Rec1 x) = gToJSVal f b x

instance (GToJSVal (a p), GToJSVal (b p)) => GToJSVal ((a :+: b) p) where
  gToJSVal f _ (L1 x) = gToJSVal f True x
  gToJSVal f _ (R1 x) = gToJSVal f True x

instance (Datatype c, GToJSVal (a p)) => GToJSVal (M1 D c a p) where
  gToJSVal f b (M1 x) = gToJSVal f b x

instance (Constructor c, GToJSVal (a p)) => GToJSVal (M1 C c a p) where
  gToJSVal f True m@(M1 x) = do
    obj@(OI.Object obj') <- OI.create
    v   <- gToJSVal f (conIsRecord m) x
    OI.setProp (packJSS . f $ conName m) v obj
    return obj'
  gToJSVal f _ m@(M1 x) = gToJSVal f (conIsRecord m) x

instance (GToJSArr (a p), GToJSArr (b p), GToJSProp (a p), GToJSProp (b p)) => GToJSVal ((a :*: b) p) where
  gToJSVal f True xy = do
    (OI.Object obj') <- OI.create
    gToJSProp f obj' xy
    return obj'
  gToJSVal f False xy = do
    arr@(AI.SomeJSArray arr') <- AI.create
    gToJSArr f arr xy
    return arr'

instance GToJSVal (a p) => GToJSVal (M1 S c a p) where
  gToJSVal f b (M1 x) = gToJSVal f b x

instance {-# OVERLAPPABLE #-} (GToJSProp (S1 m (Rec0 (Maybe x)) p), GToJSProp (b p)) => GToJSProp ((S1 m (Rec0 (Maybe x)) :*: b) p) where
  gToJSProp f o (M1 (K1 Nothing) :*: y) = gToJSProp f o y
  gToJSProp f o (x@(M1 (K1 (Just _))) :*: y) = gToJSProp f o x >> gToJSProp f o y

instance  {-# OVERLAPPABLE #-} (GToJSProp (a p), GToJSProp (b p)) => GToJSProp ((a :*: b) p) where
  gToJSProp f o (x :*: y) = gToJSProp f o x >> gToJSProp f o y

instance  {-# OVERLAPPABLE #-} (Selector m, ToJSVal x) => GToJSProp (S1 m (Rec0 (Maybe x)) p) where
  gToJSProp _ _ (M1 (K1 Nothing)) = return ()
  gToJSProp f o m@(M1 x@(K1 (Just _))) = do
    r <- gToJSVal f False x
    OI.setProp (packJSS . f $ selName m) r (OI.Object o)

instance  {-# OVERLAPPABLE #-} (Selector c, GToJSVal (a p)) => GToJSProp (M1 S c a p) where
  gToJSProp f o m@(M1 x) = do
    r <- gToJSVal f False x
    OI.setProp (packJSS . f $ selName m) r (OI.Object o)

instance (GToJSArr (a p), GToJSArr (b p)) => GToJSArr ((a :*: b) p) where
  gToJSArr f a (x :*: y) = gToJSArr f a x >> gToJSArr f a y

instance GToJSVal (a p) => GToJSArr (M1 S c a p) where
  gToJSArr f a (M1 x) = do
    r <- gToJSVal f False x
    AI.push r a

instance GToJSVal (V1 p) where
  gToJSVal _ _ _ = return jsNull

instance GToJSVal (U1 p) where
  gToJSVal _ _ _ = return jsTrue

toJSVal_generic :: forall a . (Generic a, GToJSVal (Rep a ()))
                => (String -> String) -> a -> JSM JSVal
toJSVal_generic f x = gToJSVal f False (from x :: Rep a ())

packJSS :: String -> JSString
packJSS = textToStr . T.pack
