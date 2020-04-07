{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Maple.Web.Local where

import           Control.Monad.Reader
import           Crypto.Classes
import           Crypto.Skein
import           Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize as C
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding as TE
import           Data.WideWord.Word128 (word128Hi64)
import           Data.Word
import qualified Net.IP as IP
import           Net.IPv4 (getIPv4)
import           Net.IPv6 (getIPv6)
import           Network.Socket
import           Network.Wai
import           Servant
import           Servant.Server.Internal.Delayed
import           Servant.Server.Internal.DelayedIO

data MapleLocal l

-- | Function to make a localg
-- This is passed into the servant context
-- TODO better name?
newtype MakeMapleLocal l = MakeMapleLocal {
  makeMapleLocal :: (Request -> Either Text l)
}

instance HasLink api => HasLink (MapleLocal l :> api) where
  type MkLink (MapleLocal l :> api) a = MkLink api a
  toLink toA _ = toLink toA (Proxy :: Proxy api)

xffIPLocal :: MakeMapleLocal Word64
xffIPLocal = MakeMapleLocal getLocal
  where
    getLocal :: Request -> Either Text Word64
    getLocal r =
      case join $ fmap (IP.decode . TE.decodeLatin1) $ lookup "X-Forward-For" $ requestHeaders r of
        Nothing -> case remoteHost r of
                     (SockAddrInet _ addr) ->
                       first (const "could not decode locality") $
                       C.decode $ C.encode (hash' (C.encode addr)::Skein_256_128)
                     (SockAddrInet6 _ _ (addr0, addr1, _, _) _) ->
                       first (const "could not decode locality") $
                       C.decode $ C.encode (hash' (C.encode (addr0, addr1))::Skein_256_128)
                     (SockAddrUnix _) -> Left "Unable to get ipv4 address from unix socket"
        Just xff -> first (const "could not decode locality") $
                    C.decode $ C.encode (hash' (IP.case_ (C.encode . getIPv4)
                                                         (C.encode . word128Hi64 . getIPv6) xff)::Skein_256_128)

ipv4Local :: MakeMapleLocal Word32
ipv4Local = MakeMapleLocal (getIPV4 . remoteHost)
  where
    getIPV4 :: SockAddr -> Either Text Word32
    getIPV4 (SockAddrInet _ addr)  = Right addr
    getIPV4 (SockAddrInet6 _ _ _ _) = Left "Unable to get ipv4 address from ipv6 host"
    getIPV4 (SockAddrUnix _) = Left "Unable to get ipv4 address from unix socket"

instance (HasServer api ctx, HasContextEntry ctx (MakeMapleLocal l)) => HasServer (MapleLocal l :> api) ctx where
  type ServerT (MapleLocal l :> api) m = l -> ServerT api m
  route _ context subserver =
    route (Proxy :: Proxy api) context $
      addAuthCheck subserver userSession
    where
      getLocal :: MakeMapleLocal l
      getLocal = getContextEntry context
      -- Here we get the users session id using `auth`
      -- If you want to ban users from connecting or anything like that
      -- you can `delayedFailFatal` or similar from
      -- `Servant.Server.Internal.DelayedIO`
      -- If there needs to be multiple auth checks
      -- we can also use a named context entry
      userSession :: DelayedIO l
      userSession = do
        res <- makeMapleLocal getLocal <$> ask
        case res of
          -- This should probably thorw something in the accept?
          Left err -> delayedFailFatal $ err401 { errBody = BSL.fromStrict $ Text.encodeUtf8 err }
          Right l -> pure l

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s
