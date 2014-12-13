{-# LANGUAGE OverloadedStrings #-}

module Plivo.Validator (
    checkPostParams
  , checkQParams
  , checkNonEmpty
  , hasAtLeastOnePostArg
  , evalChecks
  ) where

import           Plivo.Types
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.List as L
import           Data.Maybe

postParams :: [(T.Text,
                [(PName, ArgRequirement, TypeValidator)])]
postParams = [
    ("modifyAccount", [ ("name",    Optional, isString)
                      , ("city",    Optional, isString)
                      , ("address", Optional, isString)
                      ])
  , ("createSubaccount", [ ("name",    Mandatory, isString)
                         , ("enabled", Optional,  isBool)
                         ])
  , ("modifySubaccount", [ ("name",    Mandatory, isString)
                         , ("enabled", Optional,  isBool)
                         ])
  , ("createApplication", [ ("answer_url",           Mandatory, isString)
                          , ("app_name",             Mandatory, isString)
                          , ("answer_method",        Optional,  isString)
                          , ("hangup_url",           Optional,  isString)
                          , ("hangup_method",        Optional,  isString)
                          , ("fallback_answer_url",  Optional,  isString)
                          , ("fallback_method",      Optional,  isString)
                          , ("message_url",          Optional,  isString)
                          , ("message_method",       Optional,  isString)
                          , ("default_number_app",   Optional,  isBool)
                          , ("default_endpoint_app", Optional,  isBool)
                          ])
  , ("modifyApplication", [ ("answer_url",           Mandatory, isString)
                          , ("answer_method",        Optional,  isString)
                          , ("hangup_url",           Optional,  isString)
                          , ("hangup_method",        Optional,  isString)
                          , ("fallback_answer_url",  Optional,  isString)
                          , ("fallback_method",      Optional,  isString)
                          , ("message_url",          Optional,  isString)
                          , ("message_method",       Optional,  isString)
                          , ("default_number_app",   Optional,  isBool)
                          , ("default_endpoint_app", Optional,  isBool)
                          ])
  , ("makeCall", [ ("from",                     Mandatory, isString)
                 , ("to",                       Mandatory, isString)
                 , ("answer_url",               Mandatory, isString)
                 , ("answer_method",            Optional,  isString)
                 , ("ring_url",                 Optional,  isString)
                 , ("ring_method",              Optional,  isString)
                 , ("hangup_url",               Optional,  isString)
                 , ("hangup_method",            Optional,  isString)
                 , ("fallback_url",             Optional,  isString)
                 , ("fallback_method",          Optional,  isString)
                 , ("caller_name",              Optional,  isString)
                 , ("send_digits",              Optional,  isString)
                 , ("send_on_preanwer",         Optional,  isBool)
                 , ("time_limit",               Optional,  isInt)
                 , ("hangup_on_ring",           Optional,  isInt)
                 , ("machine_detection",        Optional,  isString)
                 , ("machine_detection_time",   Optional,  isInt)
                 , ("machine_detection_url",    Optional,  isString)
                 , ("machine_detection_method", Optional,  isString)
                 , ("sip_headers",              Optional,  isString)
                 , ("ring_timeout",             Optional,  isInt)
                 ])
  , ("transferCall", [ ("legs",        Optional, isString)
                     , ("aleg_url",    Optional,  isString)
                     , ("aleg_method", Optional,  isString)
                     , ("bleg_url",    Optional,  isString)
                     , ("bleg_method", Optional,  isString)
                     ])
  , ("recordCall", [ ("time_limit",           Optional,  isInt)
                   , ("file_format",          Optional,  isString)
                   , ("transcription_type",   Optional,  isString)
                   , ("transcription_url",    Optional,  isString)
                   , ("transcription_method", Optional,  isString)
                   , ("callback_url",         Optional,  isString)
                   , ("callback_method",      Optional,  isString)
                   ])
  , ("playFile", [ ("urls",     Mandatory, isString)
                   , ("length", Optional,  isInt)
                   , ("legs",   Optional,  isString)
                   , ("loop",   Optional,  isBool)
                   , ("mix",    Optional,  isBool)
                   ])
  , ("playText", [ ("text",     Mandatory, isString)
                 , ("voice",    Optional,  isString)
                 , ("language", Optional,  isString)
                 , ("legs",     Optional,  isString)
                 , ("loop",     Optional,  isBool)
                 , ("mix",      Optional,  isBool)
                 ])
  , ("sendDTMF", [ ("digits", Mandatory, isString)
                 , ("leg",    Optional,  isBool)
                 ])
  , ("playFileToMember", [ ("url", Mandatory, isString)
                         ])
  , ("playTextToMember", [ ("text",     Mandatory, isString)
                         , ("voice",    Optional,  isString)
                         , ("language", Optional,  isString)
                         ])
  , ("startRecordingConference", [ ("file_format",          Optional, isString)
                                 , ("transcription_type",   Optional, isString)
                                 , ("transcription_url",    Optional, isString)
                                 , ("transcription_method", Optional, isString)
                                 , ("callback_url",         Optional, isString)
                                 , ("callback_method",      Optional, isString)
                                 ])
  , ("createEndpoint", [ ("username", Mandatory, isString)
                       , ("password", Mandatory, isString)
                       , ("alias",    Mandatory, isString)
                       , ("app_id",   Optional, isString)
                       ])
  , ("modifyEndpoint", [ ("password", Optional, isString)
                       , ("alias",    Optional, isString)
                       , ("app_id",   Optional, isString)
                       ])
  , ("sendMessage", [ ("src",    Mandatory, isString)
                    , ("dst",    Mandatory, isString)
                    , ("text",   Mandatory, isString)
                    , ("type",   Optional,  isString)
                    , ("url",    Optional,  isString)
                    , ("method", Optional,  isString)
                    , ("log",    Optional,  isBool)
                    ])
  , ("modifyNumber", [ ("app_id",     Optional, isString)
                     , ("subaccount", Optional, isString)
                     , ("alias",      Optional, isString)
                     ])
  , ("modifyNumber", [ ("quantity", Optional, isString)
                     , ("app_id",   Optional, isString)
                     ])
  ]

queryParams :: [(T.Text,
                 [(PName, ArgRequirement, TypeValidator)])]
queryParams = [
    ("getSubaccounts", [ ("limit", Optional, isInt)
                       , ("offset", Optional, isInt)
                       ])
  , ("getApplications", [ ("subaccount", Optional, isString)
                        , ("limit",      Optional, isInt)
                        , ("offset",     Optional, isInt)
                        ])
  , ("getCalls", [ ("subaccount", Optional, isString)
                 , ("call_direction", Optional, isString)
                 , ("from_number", Optional, isString)
                 , ("to_number", Optional, isString)
                 , ("bill_duration", Optional, isInt)
                 , ("bill_duration__gt", Optional, isInt)
                 , ("bill_duration__gte", Optional, isInt)
                 , ("bill_duration__lt", Optional, isInt)
                 , ("bill_duration__lte", Optional, isInt)
                 , ("end_time", Optional, isString)
                 , ("end_time__gt", Optional, isString)
                 , ("end_time__gte", Optional, isString)
                 , ("end_time__lt", Optional, isString)
                 , ("end_time__lte", Optional, isString)
                 , ("limit", Optional, isInt)
                 , ("offset", Optional, isInt)
                 ])
  , ("getCallDetailRecord", [ ("subaccount", Optional, isString)
                            , ("limit", Optional, isInt)
                            , ("offset", Optional, isInt)
                            ])
  , ("getLiveCalls", [("status", Mandatory, isString)])
  , ("getLiveCallDetails", [("status", Mandatory, isString)])
  , ("stopRecordingCall", [("URL", Optional, isString)])
  , ("getMessages", [ ("limit", Optional, isInt)
                    , ("offset", Optional, isInt)
                    ])
  , ("getRentedNumbers", [ ("number_type",       Optional, isString)
                         , ("number_startswith", Optional, isString)
                         , ("subaccount",        Optional, isString)
                         , ("alias",             Optional, isString)
                         , ("services",          Optional, isString)
                         , ("limit",             Optional, isInt)
                         , ("offset",            Optional, isInt)
                         ])
  , ("availableNumberGroup", [ ("country_iso", Mandatory, isString)
                             , ("number_type", Optional,  isString)
                             , ("prefix",      Optional,  isString)
                             , ("region",      Optional,  isString)
                             , ("services",    Optional,  isString)
                             , ("limit",       Optional,  isInt)
                             , ("offset",      Optional,  isInt)
                             ])
  , ("getPricing", [ ("country_iso", Mandatory, isString)
                   ])
  , ("getRecordings", [ ("subaccount", Optional, isString)
                      , ("call_uuid",  Optional, isString)
                      , ("add_time",   Optional, isString)
                      , ("limit",      Optional, isInt)
                      , ("offset",     Optional, isInt)
                      ])
  ]

mkMap :: [(T.Text, [(PName, ArgRequirement, TypeValidator)])] ->
         M.Map T.Text (M.Map T.Text ArgSpec)
mkMap mp = M.map mapTransform (M.fromList mp)
  where
    paramsToMap mp' (pname, preq, typechk) = M.insert pname (preq, typechk) mp'
    mapTransform = foldl paramsToMap M.empty

postParamMP :: M.Map T.Text (M.Map T.Text ArgSpec)
postParamMP = mkMap postParams

qParamMP :: M.Map T.Text (M.Map T.Text ArgSpec)
qParamMP = mkMap queryParams

isInt :: PName -> PValue -> Maybe ArgsError
isInt _ (IV _) = Nothing
isInt name _ = Just $ BadType name " must be of Integer type."

isBool :: PName -> PValue -> Maybe ArgsError
isBool _ (BV _) = Nothing
isBool name _ = Just $ BadType name " must be of Boolean type."

isString :: PName -> PValue -> Maybe ArgsError
isString _ (SV _) = Nothing
isString name _ = Just $ BadType name " must be of String-like type."

getMessage :: ArgsError -> T.Text
getMessage (BadType name val) = T.concat ["**", name, "**", val]
getMessage (InvalidParam name) = T.concat [
  "**", name, "**", " is not a permitted parameter."]
getMessage (MissingMandatory name) = T.concat [
  "**", name, "**", " is a missing mandatory parameter."]
getMessage (AtLeastOneRequired txt) = T.concat [
  "At least one ", txt, " is required."]
getMessage (EmptyArgument name) = T.concat [
  "**", name, "**", " should not be empty"]

checkParamType :: ReqParamSpec -> (PName, PValue) -> Maybe ArgsError
checkParamType mp (name, value) = case M.lookup name mp of
  Nothing -> Just $ InvalidParam name
  Just (_, tchecker) -> case tchecker name value of
    Just typeError -> Just typeError
    Nothing -> Nothing

checkParamReq :: ReqParamSpec -> (PName, PValue) -> Maybe ArgsError
checkParamReq mp (mArg, _) = case mArg `elem` mArgs of
  True -> Nothing
  False -> Just $ MissingMandatory mArg
  where
    mArgs = M.keys $ M.filter (\(r, _) -> r == Mandatory) mp

checkParams :: [(PName, PValue)] -> T.Text ->
               M.Map T.Text ReqParamSpec ->
               [ArgsError]
checkParams params requestName rpspec =
  let mp = M.findWithDefault M.empty requestName rpspec
      val = concatMap (\f -> map (f mp) params) [
        checkParamType, checkParamReq]
  in fromJust $ sequence $ filter isJust val

checkPostParams :: [PostParam] -> T.Text -> [ArgsError]
checkPostParams params requestName =
  checkParams params requestName postParamMP

checkQParams :: [QParam] -> T.Text -> [ArgsError]
checkQParams params requestName =
  checkParams params requestName qParamMP

hasAtLeastOnePostArg :: [PostParam] -> [ArgsError]
hasAtLeastOnePostArg ps =
  if L.null ps
    then [AtLeastOneRequired "POST parameter"]
    else []

evalChecks :: [[ArgsError]] -> [T.Text]
evalChecks = map getMessage.concat

checkNonEmpty :: T.Text -> T.Text -> [ArgsError]
checkNonEmpty name value = 
  if T.null value
    then [EmptyArgument name]
    else []

-- checkParams ["name" ~~ "abcd", "city" ~~ 3] "modifyAccount"
