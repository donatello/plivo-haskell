{-# LANGUAGE OverloadedStrings #-}

module Plivo.RestApi
       (
         initApiSettings,
         finApiSettings,
         get_account
       ) where

import Network.HTTP.Conduit
import qualified Data.Conduit as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.Conduit.Binary (sinkFile)
import Data.Conduit.List (consume)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
--import Data.Aeson.Parser (json)

data ApiSettings = ApiSettings {
  auth_id :: B.ByteString,
  auth_pass :: B.ByteString,
  manager :: Manager
  }

plivo_version :: B.ByteString
plivo_version = "v1/"

base_url :: B.ByteString
base_url = "https://api.plivo.com/"

initApiSettings :: String -> String -> IO ApiSettings
initApiSettings user pass = do
  manager <- newManager def
  return ApiSettings {
    auth_id = BC8.pack $ user,
    auth_pass = BC8.pack $ pass,
    manager = manager
    }

finApiSettings :: ApiSettings -> IO ()
finApiSettings api_settings = closeManager $ manager api_settings

build_url :: ApiSettings -> B.ByteString -> B.ByteString
build_url settings suburl = B.concat [base_url, plivo_version, "Account/",
                            (auth_id settings), "/", suburl]

build_req settings suburl method = do
  req <- parseUrl $ BC8.unpack (build_url settings suburl)
  let r' = applyBasicAuth (auth_id settings) (auth_pass settings) req
      req' = r' {
        secure = True,
        method = method
                }
  return req'

send_request req manager = C.runResourceT $ do
    resp <- http req manager
    bodies <- responseBody resp C.$$+- consume
    let body = B.concat $ bodies
    return (statusCode $ responseStatus resp, body)

get_account :: ApiSettings -> IO (Int, B.ByteString)
get_account settings = do
  req <- build_req settings "" methodGet
  send_request req (manager settings)
