{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module AuthRequest where

import Data.Aeson
import GHC.Generics
import Data.Monoid

data AuthRequest = AuthRequest {
    client_id :: String
  , client_secret :: String
  , grant_type :: String
} deriving (Generic,  Show)

instance ToJSON AuthRequest where
    toJSON (AuthRequest id_ secret grant) =
      object [ "client_id" .= id_
             , "client_secret" .= secret
             , "grant_type" .= grant
             ]
    toEncoding(AuthRequest id_ secret grant) =
      pairs ("client_id" .= id_ <> "client_secret" .= secret <> "grant_type" .= grant)
