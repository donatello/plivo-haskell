{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Plivo.Types (
    TypeValidator
  , PName, PValue(..)
  , PostParam, QParam
  , toText
  , ArgsError(..)
  , ArgRequirement(..)
  , (~~)
  , WebRequest(..)
  , URL
  , ApiSettings(..)
  , ArgSpec
  , ReqParamSpec
  , ReqType(..)
  , PValues(..)
  ) where

import qualified Data.Text as T
import           Data.Aeson (ToJSON, toJSON, (.=), object)
import           Network.Wreq (Options)
import qualified Data.Map.Strict as M


data ApiSettings = ApiSettings {
  authID :: T.Text,
  authToken :: T.Text,
  reqSettings :: Options
  } deriving (Show)

-- Type for URLs
type URL = T.Text

data WebRequest = GetRequest ApiSettings URL [QParam]
                | PostRequest ApiSettings URL [QParam] [PostParam]
                | DeleteRequest ApiSettings URL [QParam]
                deriving (Show)

data PValue = IV Int | BV Bool | SV T.Text
            deriving (Show, Eq)

type PName = T.Text
type PostParam = (PName, PValue)
type QParam = (PName, PValue)

toText :: PValue -> T.Text
toText (IV v) = T.pack.show $ v
toText (BV v) = if v then "true" else "false"
toText (SV v) = v

data ArgsError = BadType PName T.Text
               | InvalidParam PName
               | MissingMandatory PName
               | AtLeastOneRequired T.Text
               | EmptyArgument T.Text
               deriving (Show, Eq)

data ArgRequirement = Optional | Mandatory
                    deriving (Show, Eq)

data ReqType = POST | GET | DELETE
             deriving (Show, Eq)

type TypeValidator = PName -> PValue -> Maybe ArgsError
type ArgSpec = (ArgRequirement, TypeValidator)
type ReqParamSpec = M.Map PName ArgSpec

instance ToJSON PValue where
  toJSON (IV a) = toJSON a
  toJSON (BV a) = toJSON a
  toJSON (SV s) = toJSON s

class PValues v where
  loadVal :: v -> PValue

instance PValues Int where
  loadVal v = IV v

instance PValues Integer where
  loadVal v = IV (fromInteger v)

instance PValues Bool where
  loadVal v = BV v

instance PValues T.Text where
  loadVal s = SV s

instance PValues String where
  loadVal s = SV (T.pack s)

loadParamKV :: PValues v => PName -> v -> PostParam
loadParamKV name value = (name, loadVal value)

(~~) :: PValues v => PName -> v -> PostParam
(~~) = loadParamKV

instance ToJSON [PostParam] where
  toJSON params = object $ map (\(x, y) -> x .= y) params

-- makeCall ["to" ~~ "12345", "from" ~~ True, "time_limit" ~~ (100::Int)]
