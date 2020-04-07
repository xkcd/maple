{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Maple.Config where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Kind
import Maple.Loot
import Web.ClientSession
import Maple.Map
import Maple.Session
import Maple.Web.Admin
import Maple.Web.API
import Maple.Web.Local

-- | The configuration that defines the Maple system.
--   g is the map type.
--     is (HasBinMap)
--   l is the local
--     is (Hashable)
--   s is the list of keys we keep to enable us to tell if a loot is unlocked.
--     is (To/FromJSON)
--   i is the loot type.
--     is (To/FromJSON)
--   v is the vector and dimensionality.
--     is (R1, R2, Affine)
--   p is the storage in the dimension.
--     is (Num)
--   b is the bin type.
--     is (Eq)
--   e is extra storage, so it can be used as a reader without translation.
data MapleConfig g l s i (v::Type->Type) p b e
  = (HasBinMap g g v p b) => MC
   { _mcCSKey      :: Key
   , _mcGrigMap    :: g
   , _mcLootBoxes  :: LootBoxes s i
   , _mcMkLocal    :: MakeMapleLocal l
   , _mcCheckAdmin :: MapleAdminCheck
   , _mcBinListCachePolicy :: MapleCacheControl
   , _mcBinContentsCachePolicy :: MapleCacheControl
   , _mcAdminLoot  :: [i]
   , _mcPoolSize   :: LocalPoolSize
   , _mcImageFolders :: [String]
   , _mcPaused     :: Bool
   , _mcAllowedOrigins :: [ByteString]
   , _mcExtra      :: e
   }

makeClassy ''MapleConfig

instance Functor (MapleConfig g l s i v p b) where
  fmap f d = d { _mcExtra = f (_mcExtra d) }

instance HasBinMap g g v p b => HasBinMap (MapleConfig g l s i v p b e) g v p b where
  binMap = mcGrigMap

instance HasClientKey (MapleConfig g l s i v p b e) where
  clientKey = mcCSKey

instance HasLocalPoolSize (MapleConfig g l s i v p b e) where
  localPoolSize = mcPoolSize
