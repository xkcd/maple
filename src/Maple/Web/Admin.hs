{-# LANGUAGE OverloadedStrings #-}
module Maple.Web.Admin where

import Crypto.BCrypt
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Text (Text)
import Servant.API
import Servant.Server

newtype MapleAdminCheck = MapleAdminCheck {
  runMapleAdminCheck :: (Text, Text) -> Maybe MapleAdmin
}

data MapleAdmin = MapleAdmin {
  mapleAdminUsername :: Text
} deriving (Eq, Ord, Show)

mapleAdminCheck :: MapleAdminCheck -> BasicAuthCheck MapleAdmin
mapleAdminCheck adminCheck =
  BasicAuthCheck $ \(BasicAuthData user' pass') ->
    let requestUser = Text.decodeUtf8 user'
        rawPassword = Text.decodeUtf8 pass'
    in case runMapleAdminCheck adminCheck (requestUser, rawPassword) of
      Nothing -> pure NoSuchUser
      Just user -> pure $ Authorized user

nullAdminCheck :: MapleAdminCheck
nullAdminCheck = MapleAdminCheck $ \(user, _pass) -> Just $ MapleAdmin user

authCheckFromFile :: String -> IO MapleAdminCheck
authCheckFromFile fp = do
  users <- (Map.fromList . parseFile) <$> Text.readFile fp
  pure $ MapleAdminCheck $ \(user, pass) -> do
    hashedPassword <- Map.lookup user users
    if validatePassword hashedPassword (Text.encodeUtf8 pass)
      then Just $ MapleAdmin user
      else Nothing
  where
    parseFile = fmap (toTuple . Text.splitOn ":") . Text.lines
    toTuple [user,passHash] = (user, Text.encodeUtf8 passHash)
    toTuple _ = error "Unable to parse auth file"
