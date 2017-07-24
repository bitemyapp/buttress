-- {-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Buttress.Tuples where

import Data.Function
import Data.Proxy
import Data.Reflection
import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude
import GHC.TypeLits
import Unsafe.Coerce

-- type family TupleIx' (n :: Nat) (t :: *) where
-- -- type family TupleIx' (n :: Nat) (t :: *) where
--   TupleIx' 0 (a, _) = a
--   TupleIx' 1 (_, b) = b

--   TupleIx' 0 (a, _, _) = a
--   TupleIx' 1 (_, b, _) = b
--   TupleIx' 2 (_, _, c) = c
-- type family TupleIx' (n :: k) (t :: *)
-- newtype C a b (n :: k) = C a deriving Show

-- type family TupleIx' (n :: k) (t :: *) = r | r -> t n
-- type instance TupleIx' 0 (a, _) = a
-- type instance TupleIx' 1 (_, b) = b

-- type instance TupleIx' 0 (a, b) = C a b 0
-- type instance TupleIx' 1 (a, b) = C b a 1

-- data family TupleIx' (n :: Nat) t

-- data instance TupleIx' 0 (a, b) = TIxZ2 a
-- data instance TupleIx' 1 (a, b) = TIx12 b

-- type family TtoN t where
--   TtoN (a, b) = 0
-- type family R (x :: Bool) y where
--     R True e = (e, e)
--     R False e = (e, e, e)

-- type family ZToX z where
--     ZToX (e, e') = True
--     ZToX (e, e', e'') = False

-- type family ZToY z where
--     ZToY (e, e') = e
--     ZToY (e, e', e'') = e

 -- type family FromParams a :: *
 -- class FromParams (Parameters a) ~ a => Request a where
 -- type instance FromParams Text :: User
 -- class Request User where
 --     type Parameters User :: Text

-- type family TupleIx'2 (n :: k) (t :: *) where
--   TupleIx'2 n t = TupleIx' n a

-- data TupleIxF :: TyFun Nat * -> * -> *
-- type instance Apply (TupleIxF n a) = TupleIx' n a

-- data MyFamilySym :: TyFun MyType * -> *
-- type instance Apply MyFamilySym a = MyFamily a  

-- class ForallInst (f :: TyFun k * -> *) (c :: * -> Constraint) where
--   allInst :: Proxy '(f, c) -> Sing x -> Dict (c (f @@ x))

-- instance ForallInst MyFamilySym FromJSON where
--   allInst _ SMyValue1 = Dict
--   allInst _ SMyValue2 = Dict
--   allInst _ SMyValue3 = Dict  

-- instance FromJSON (Some MyCompoundType) where
--     parseJSON = withObject "MyCompoundType" $ \o -> do
--       cons :: String <- o .: "constructor"
--       SomeSing smt <- toSing <$> o .: "myType"
--       case cons of
--         "CompoundNoIndex" -> pure (Some smt CompoundNoIndex)
--         "CompoundWithIndex" ->
--           case allInst (Proxy :: Proxy '(MyFamilySym, FromJSON)) smt of
--             Dict -> Some smt . CompoundWithIndex <$> o .: "field"

-- class AddC m n r | m n → r
-- instance AddC Zero n n
-- instance AddC m n r ⇒ AddC (Succ m) n (Succ r)

-- class TupleIx' n t r | n t -> r
-- instance TupleIx' 0 (a, b) a
-- instance TupleIx' 1 (a, b) b

-- class TupleIx' n t where
--   type Result t n

-- instance TupleIx' n (a, b) where
--   type Result 0 (a, b) = a
--   type Result 1 (a, b) = b

-- instance TupleIx' 1 (a, b) where

-- class TupleIx a (n :: Nat) where
--   (#.) :: a -> Proxy n -> TupleIx' n a

-- class KnownNat n => TupleIx a n where
--   tix :: a -> Sing n -> TupleIx' n a

-- instance TupleIx (a, b) 0 where
--   tix (a, _) _ = a

-- instance TupleIx (a, b) 1 where
--   tix (_, b) _ = b

-- instance TupleIx (a, b, c) 0 where
--   tix (a, _, _) _ = a

-- instance TupleIx (a, b, c) 1 where
--   tix (_, b, _) _ = b

-- instance TupleIx (a, b, c) 2 where
--   tix (_, _, c) _ = c

-- class KnownNat n => TupleIx a n where
class TupleIx a n where
  -- type TupleIx' n a = r | r -> n a
  -- tix :: (TupleIx' n a ~ x) => a -> x
  data TupleIx' n a
  tix :: a -> TupleIx' n a

-- instance Rec a ~ reca => Sel a s (b -> (c, reca))

-- instance TupleIx (a, b) 0 where
--   tix (a, _) = a

-- instance TupleIx (a, b) 1 where
--   tix (_, b) = b

instance TupleIx (a, b) 0 where
  -- type TupleIx' 0 (a, b) = C a b 0
  data TupleIx' 0 (a, b) = Tix2'0 a
  tix (a, _) = Tix2'0 a

instance TupleIx (a, b) 1 where
  -- type TupleIx' 1 (a, b) = C b a 1
  data TupleIx' 1 (a, b) = Tix2'1 b
  tix (_, b) = Tix2'1 b

-- class KnownNat n => TupleIx a n where
--   tix :: a -> TupleIx' n a

-- instance TupleIx (a, b) 0 where
--   tix (a, _) = TIxZ2 a

-- instance TupleIx (a, b) 1 where
--   tix (_, b) = TIx12 b

-- instance TupleIx (a, b, c) 0 where
--   tix (a, _, _) = a

-- instance TupleIx (a, b, c) 1 where
--   tix (_, b, _) = b

-- instance TupleIx (a, b, c) 2 where
--   tix (_, _, c) = c

-- class KnownNat n => TupleIx a n where
--   tix :: TupleIx' n a => a -> Result a n

-- instance TupleIx (a, b) 0 where
--   tix (a, _) = a

-- instance TupleIx (a, b) 1 where
--   tix (_, b) = b

-- (#.) :: forall a b n' x k
--       . (DemoteRep k ~ Integer) =>
--       (a, b) -> DemoteRep k -> TupleIx' n' (a, b)
-- (#.) :: forall a b n' .
--         SingI (N t) =>
-- (#.) :: forall t n' .
--         t -> Integer -> TupleIx' n' t
-- (#.) :: forall t n' .
--         t -> Integer -> TupleIx' n' t
(#.) :: forall t n' k .
        (DemoteRep k ~ Integer) =>
        t -> DemoteRep k -> TupleIx' n' t
(#.) t n =
  case toSing n of
    SomeSing (sb :: Sing k) ->
      tix @t @k t
      -- tix @t @a t
      -- let _ = t :: forall a b . (a, b)
      -- in tix t :: TupleIx' num (a, b)
      -- tix @_ @num t :: TupleIx' num t2
      -- tix t :: TupleIx' num t2 r => r
      -- let a :: a
      --     a = sb
      -- in undefined -- tix t sb

-- liftNat :: (KnownNat n) => Integer -> Proxy n
-- liftNat i = Proxy :: Proxy 'i

-- (#.) :: (TupleIx a n) => a -> Integer -> TupleIx' n a
-- (#.) t n = tix t (reifyNat n natVal)

-- zero :: Proxy 0
-- zero = Proxy

-- one :: Proxy 1
-- one = Proxy

-- two :: Proxy 2
-- two = Proxy

-- three :: Proxy 3
-- three = Proxy

-- should work
-- main = do
--   print (('a', 'b') #. 0)
--   print (('a', 'b') #. 1)
--   print (('a', 'b', 'c') #. 2)

  -- print (('a', 'b') #. zero)
  -- print (('a', 'b') #. one)
  -- print (('a', 'b', 'c') #. two)

-- instance (KnownNat n, TupleIx' n (a, b) ~ c) => Num ((a, b) -> c) where
-- instance (KnownNat n, c ~ TupleIx' n (a, b)) => Num ((a, b) -> c) where
--   fromInteger 0 (a, b) = a
--   fromInteger 1 (a, b) = b

-- data Clock (m :: Nat) = Clock Integer -- m is the modulus
-- instance KnownNat m => Num (Clock m) where
--   fromInteger n = Clock (n `mod` natVal (Proxy :: Proxy m))

-- instance forall m a b c
--        . (KnownNat m, c ~ TupleIx' m (a, b))
--       => Num ((a, b) -> c) where
--   -- fromInteger :: Integer -> (a, b) -> c
--   fromInteger n t = t #. n

-- type family TupleIx (n :: Nat) (t :: *) where
--   TupleIx 0 (a, b) = a
--   TupleIx 1 (a, b) = b

-- instance (TupleIx 0 (a, b) ~ c) => Num ((a, b) -> c) where
--   fromInteger 0 (a, b) = a
--   fromInteger 1 (a, b) = b

-- shouldNotTC = do
--   print (('a', 'b') #. two)
--   print (('a', 'b') #. three)


-- class Indexable i p where
--   indexed :: p a b -> i -> a -> b

-- instance (b ~ DiffTime) => Num ((a, b) -> b) where
--   fromInteger i Years =
--     secondsToDiffTime (i * secondsInAYear)

-- instance (b ~ DiffTime) => Fractional (TimeConvert -> b) where
--   -- rounding to the nearest second, which doesn't seem
--   -- unreasonable when dealing with denominations of a year
--   fromRational r Years =
--     secondsToDiffTime (round (r * (toRational secondsInAYear)))
