{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Discord.Internal.Types.Checks (
    BoundedData
  , unbind
  , bind
  , bindM

  , Check (..)
  , Checked
  , unChecked
  , Constrain (..)
  , Constrained
  , unConstrained
  ) where

import Data.Data (Proxy (Proxy))
import GHC.TypeNats
import Data.Aeson

import qualified Data.Text as T

natValAny :: (KnownNat n, Num m) => Proxy n -> m
natValAny = fromIntegral . natVal

-- | Type for bounded values.
--
-- Limited to positive values (lowest value is 0).
data BoundedData (min :: Nat) (max :: Nat) a where
  BoundedData :: forall min max a. (KnownNat min, KnownNat max, min <= max) => a -> BoundedData min max a

-- | Extract the value from bounded data.
unbind :: BoundedData min max a -> a
unbind (BoundedData a) = a

instance ToJSON a => ToJSON (BoundedData min max a) where
  toJSON (BoundedData a) = toJSON a

instance Show a => Show (BoundedData min max a) where
  showsPrec i (BoundedData a) = showsPrec i a

-- unsafeBind :: forall min max a. (KnownNat min, KnownNat max, min <= max) => a -> BoundedData min max a
-- unsafeBind = BoundedData

-- | Typeclass to check whether a type is within its bounds.
class Check t where
  -- | Either return the value given if valid, or a failure value.
  check :: MonadFail m => t -> m t

-- | Typeclass to constrain a type to its bounds
class (Check t) => Constrain t where
  -- | If the value is within its bounds, it is returned unchanged.
  -- Otherwise, it is changed to be correct.
  constrain :: t -> t

checkNum :: forall min max n m. (KnownNat min, KnownNat max, Num n, Ord n, MonadFail m) => BoundedData min max n -> m (BoundedData min max n)
checkNum bs@(BoundedData n) = if n <= max' && min' <= n then pure bs else fail "number not in bounds"
    where
      min' = natValAny (Proxy :: Proxy min)
      max' = natValAny (Proxy :: Proxy max)

constrainNum :: forall min max n. (KnownNat min, KnownNat max, Num n, Ord n) => BoundedData min max n -> BoundedData min max n
constrainNum (BoundedData n) = BoundedData $ min max' $ max min' n
    where
      min' = natValAny (Proxy :: Proxy min)
      max' = natValAny (Proxy :: Proxy max)

instance (KnownNat min, KnownNat max) => Check (BoundedData min max T.Text) where
  check bs@(BoundedData t) = if len <= max' && min' <= len
      then pure bs
      else fail "text not in bounds"
    where
      len = T.length t
      min' = natValAny (Proxy :: Proxy min)
      max' = natValAny (Proxy :: Proxy max)

instance (KnownNat max) => Constrain (BoundedData 0 max T.Text) where
  constrain (BoundedData t) = BoundedData $ T.take max' t
    where
      max' = natValAny (Proxy :: Proxy max)

instance (KnownNat min, KnownNat max) => Check (BoundedData min max Int) where
  check = checkNum

instance (KnownNat min, KnownNat max) => Constrain (BoundedData min max Int) where
  constrain = constrainNum

instance (KnownNat min, KnownNat max) => Check (BoundedData min max Integer) where
  check = checkNum

instance (KnownNat min, KnownNat max) => Constrain (BoundedData min max Integer) where
  constrain = constrainNum

instance (KnownNat min, KnownNat max) => Check (BoundedData min max [a]) where
  check bs@(BoundedData t) = case (drop max' t, length (take min' t) - min') of
      ([], 0) -> pure bs
      _ -> fail "list not in bounds"
    where
      min' = natValAny (Proxy :: Proxy min)
      max' = natValAny (Proxy :: Proxy max)

instance (KnownNat max) => Constrain (BoundedData 0 max [a]) where
  constrain (BoundedData t) = BoundedData $ take max' t
    where
      max' = natValAny (Proxy :: Proxy max)

bindM :: (KnownNat min, KnownNat max, min <= max, Check (BoundedData min max a), MonadFail m) => a -> m (BoundedData min max a)
bindM = check . BoundedData

bind :: (KnownNat min, KnownNat max, min <= max, Constrain (BoundedData min max a)) => a -> BoundedData min max a
bind = constrain . BoundedData

data Checked a where
  Checked :: Check a => a -> Checked a

unChecked :: Checked a -> a
unChecked (Checked a) = a

instance (Check a, FromJSON a) => FromJSON (Checked a) where
  parseJSON v = do
    a <- parseJSON v
    case check a of
      Error s -> fail s
      Success a' -> pure $ Checked a'

instance {-# OVERLAPPING #-} (KnownNat min, KnownNat max, min <= max, Check (BoundedData min max a), FromJSON a) => FromJSON (Checked (BoundedData min max a)) where
  parseJSON v = do
    a <- parseJSON v
    case check $ BoundedData a of
      Error s -> fail s
      Success a' -> pure $ Checked a'

data Constrained a where
  Constrained :: Constrain a => a -> Constrained a

unConstrained :: Constrained a -> a
unConstrained (Constrained a) = a

instance (Constrain a, FromJSON a) => FromJSON (Constrained a) where
  parseJSON v = do
    a <- parseJSON v
    pure $ Constrained $ constrain a

instance {-# OVERLAPPING #-} (KnownNat min, KnownNat max, min <= max, Constrain (BoundedData min max a), FromJSON a) => FromJSON (Constrained (BoundedData min max a)) where
  parseJSON v = do
    a <- parseJSON v
    pure $ Constrained $ constrain $ BoundedData a

-- parseCheck :: (FromJSON a, Check a) => Value -> Parser a
-- parseCheck v = do
--     v' :: a <- parseJSON v
--     case check v' of
--       Just v'' -> pure v''
--       Nothing -> fail "could not parse Check value"

-- parseConstrain :: (FromJSON a, Constrain a) => Value -> Parser a
-- parseConstrain v = do
--     v' :: a <- parseJSON v
--     pure $ constrain v'

-- getBindFromObjectCheck :: (FromJSON a, KnownNat min, KnownNat max, min <= max, Check (BoundedData min max a)) => Object -> AesonKey -> (BoundedData min max a -> b) -> Parser b
-- getBindFromObjectCheck o key cons = getAndTransform o key (fmap cons . bindM)

-- getBindFromObjectConstrain :: (FromJSON a, KnownNat min, KnownNat max, min <= max, Constrain (BoundedData min max a)) => Object -> AesonKey -> (BoundedData min max a -> b) -> Parser b
-- getBindFromObjectConstrain o key cons = getAndTransform o key (pure . cons . bind)

-- getAndTransform :: (FromJSON a) => Object -> AesonKey -> (a -> Parser b) -> Parser b
-- getAndTransform o key transform = transform =<< o .: key
