{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq
import Data.ByteString as B

clientId = "some_id" :: ByteString
clientSecret = "some_secret" :: ByteString
grantType = "credentials" :: ByteString
url = "http://localhost:8080"

postIt = post url ["client_id" := clientId, "client_secret" := clientSecret, "grant_type" := grantType]

main = postIt
