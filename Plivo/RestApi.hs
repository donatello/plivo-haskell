{-# LANGUAGE OverloadedStrings #-}

module Plivo.RestApi
       (
         initApiSettings
       , getAccount
       , modifyAccount
       , createSubaccount
       , modifySubaccount
       , getSubaccount
       , getSubaccounts
       , deleteSubaccount
       , createApplication
       , getApplications
       , getApplication
       , modifyApplication
       , deleteApplication
       , makeCall
       , getCalls
       , getCallDetailRecord
       , getLiveCalls
       , getLiveCallDetails
       , hangupCall
       , transferCall
       , recordCall
       , stopRecordingCall
       , playFile
       , stopPlayFile
       , playText
       , stopPlayText
       , sendDTMF
       , hangupCallRequest
       , getConferences
       , getConference
       , hangupAllConferences
       , hangupConference
       , hangupConferenceMember
       , kickConferenceMember
       , muteConferenceMember
       , unmuteConferenceMember
       , playFileToMember
       , stopPlayFileToMember
       , playTextToMember
       , stopPlayTextToMember
       , deafMember
       , undeafMember
       , startRecordingConference
       , stopRecordingConference
       , createEndpoint
       , getEndpoint
       , getEndpoints
       , modifyEndpoint
       , deleteEndpoint
       , sendMessage
       , getMessages
       , getMessage
       , getRentedNumbers
       , getRentedNumber
       , modifyNumber
       , unrentNumber
       , availableNumberGroup
       , rentNumber
       , getPricing
       , getRecordings
       , getRecording
       , deleteRecording

       , (~~)
       ) where

import           Plivo.Types
import           Plivo.Validator

import           Network.Wreq hiding (params)
import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson (toJSON)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.List as L
import qualified Data.ByteString.Lazy as Bl

apiVersion :: URL
apiVersion = "v1/"

baseUrl :: URL
baseUrl = "https://api.plivo.com/"

runRequest :: WebRequest -> IO (Response Bl.ByteString)
runRequest req = case req of
  GetRequest apiSettings url qparams ->
    getWith (mkOpts apiSettings qparams) (mkUrl apiSettings url)
  PostRequest apiSettings url qparams postdata ->
    postWith (mkOpts apiSettings qparams) (mkUrl apiSettings url)
    (toJSON postdata)
  DeleteRequest apiSettings url qparams ->
    deleteWith (mkOpts apiSettings qparams) (mkUrl apiSettings url)
  where
    mkUrl stgs myurl = T.unpack.T.concat $ [
      baseUrl, apiVersion, "Account/", authID stgs, "/", myurl]
    mkOpts stgs qp = (L.foldl' (\s (k, v) -> s & param k .~ [toText v])
                      (reqSettings stgs) qp) &
                     auth ?~ basicAuth (encodeUtf8.authID $ stgs)
                     (encodeUtf8.authToken $ stgs)

evaluate :: ApiSettings -> T.Text -> ReqType ->
            [URL] -> [QParam] -> [PostParam] ->
            [[ArgsError]] ->
            IO (Either [T.Text] (Response Bl.ByteString))
evaluate settings reqName reqType urls qparams postparams checks =
  runExceptT $ do
    if L.null errorMsgs
      then liftIO $ runRequest req
      else throwError errorMsgs
  where
    req = case reqType of
      POST -> PostRequest settings urlFragment qparams postparams
      DELETE -> DeleteRequest settings urlFragment qparams
      GET -> GetRequest settings urlFragment qparams
    errorMsgs = evalChecks (checks ++
                            [checkQParams qparams reqName,
                             checkPostParams postparams reqName])
    urlFragment = T.concat $ concatMap (:["/"]) urls

initApiSettings :: T.Text -> T.Text -> ApiSettings
initApiSettings authId authTok = ApiSettings authId authTok defaults

getAccount :: ApiSettings ->
              IO (Either [T.Text] (Response Bl.ByteString))
getAccount settings =
  evaluate settings "getAccount" GET [] [] [] []

modifyAccount :: ApiSettings -> [PostParam] ->
                 IO (Either [T.Text] (Response Bl.ByteString))
modifyAccount settings params =
  evaluate settings "modifyAccount" POST [] [] params [
    hasAtLeastOnePostArg params]

createSubaccount :: ApiSettings -> [PostParam] ->
                    IO (Either [T.Text] (Response Bl.ByteString))
createSubaccount settings params =
  evaluate settings "createSubaccount" POST ["Subaccount"] [] params []

modifySubaccount :: ApiSettings -> T.Text -> [PostParam] ->
                    IO (Either [T.Text] (Response Bl.ByteString))
modifySubaccount settings subAccAuthId params =
  evaluate settings "modifySubaccount" POST
  ["Subaccount", subAccAuthId] [] params
  [checkNonEmpty "subAccAuthId" subAccAuthId]

getSubaccount :: ApiSettings -> T.Text ->
                 IO (Either [T.Text] (Response Bl.ByteString))
getSubaccount settings subAccAuthId =
  evaluate settings "getSubAccount" GET
  ["Subaccount", subAccAuthId] [] []
  [checkNonEmpty "subAccAuthId" subAccAuthId]

getSubaccounts :: ApiSettings -> [QParam] ->
                  IO (Either [T.Text] (Response Bl.ByteString))
getSubaccounts settings params =
  evaluate settings "getSubaccounts" GET ["Subaccount"] params [] []

deleteSubaccount :: ApiSettings -> T.Text ->
                  IO (Either [T.Text] (Response Bl.ByteString))
deleteSubaccount settings subAccAuthId =
  evaluate settings "deleteSubaccount" DELETE
  ["Subaccount", subAccAuthId] [] []
  [checkNonEmpty "subAccAuthId" subAccAuthId]

createApplication :: ApiSettings -> [PostParam] ->
                  IO (Either [T.Text] (Response Bl.ByteString))
createApplication settings params =
  evaluate settings "createApplication" POST ["Application"] [] params []

getApplications :: ApiSettings -> [QParam] ->
                  IO (Either [T.Text] (Response Bl.ByteString))
getApplications settings params =
  evaluate settings "getApplications" GET ["Application"] params [] []

getApplication :: ApiSettings -> T.Text ->
                  IO (Either [T.Text] (Response Bl.ByteString))
getApplication settings appId =
  evaluate settings "getApplication" GET
  ["Application", appId] [] []
  [checkNonEmpty "appId" appId]

modifyApplication :: ApiSettings -> T.Text -> [PostParam] ->
                     IO (Either [T.Text] (Response Bl.ByteString))
modifyApplication settings appId params =
  evaluate settings "modifyApplication" POST
  ["Application", appId] [] params
  [checkNonEmpty "appId" appId]

deleteApplication :: ApiSettings -> T.Text ->
                     IO (Either [T.Text] (Response Bl.ByteString))
deleteApplication settings appId =
  evaluate settings "modifyApplication" DELETE
  ["Application", appId] [] []
  [checkNonEmpty "appId" appId]

makeCall :: ApiSettings -> [PostParam] ->
            IO (Either [T.Text] (Response Bl.ByteString))
makeCall settings params =
  evaluate settings "makeCall" POST ["Call"] [] params []

getCalls :: ApiSettings -> [QParam] ->
            IO (Either [T.Text] (Response Bl.ByteString))
getCalls settings params =
  evaluate settings "getCalls" GET ["Call"] params [] []

getCallDetailRecord :: ApiSettings -> T.Text -> [QParam] ->
                       IO (Either [T.Text] (Response Bl.ByteString))
getCallDetailRecord settings callUUID params =
  evaluate settings "getCallDetailRecord" GET
  ["Call", callUUID] params []
  [checkNonEmpty "callUUID" callUUID]

getLiveCalls :: ApiSettings ->
                IO (Either [T.Text] (Response Bl.ByteString))
getLiveCalls settings =
  evaluate settings "getLiveCalls" GET ["Call"]
  ["status" ~~ ("live"::T.Text)] [] []

getLiveCallDetails :: ApiSettings -> T.Text ->
                      IO (Either [T.Text] (Response Bl.ByteString))
getLiveCallDetails settings callUUID =
  evaluate settings "getLiveCallDetails" GET
  ["Call", callUUID] ["status" ~~ ("live"::T.Text)] []
  [checkNonEmpty "callUUID" callUUID]

hangupCall :: ApiSettings -> T.Text ->
              IO (Either [T.Text] (Response Bl.ByteString))
hangupCall settings callUUID =
  evaluate settings "hangupCalls" DELETE
  ["Call", callUUID] [] []
  [checkNonEmpty "callUUID" callUUID]

transferCall :: ApiSettings -> T.Text -> [PostParam] ->
                IO (Either [T.Text] (Response Bl.ByteString))
transferCall settings callUUID params =
  evaluate settings "transferCall" POST
  ["Call", callUUID] [] params
  [checkNonEmpty "callUUID" callUUID, hasAtLeastOnePostArg params]

recordCall :: ApiSettings -> T.Text -> [PostParam] ->
              IO (Either [T.Text] (Response Bl.ByteString))
recordCall settings callUUID params =
  evaluate settings "recordCall" POST
  ["Call", callUUID, "Record"] [] params
  [checkNonEmpty "callUUID" callUUID]

stopRecordingCall :: ApiSettings -> T.Text -> [QParam] ->
                     IO (Either [T.Text] (Response Bl.ByteString))
stopRecordingCall settings callUUID params =
  evaluate settings "stopRecordingCall" DELETE
  ["Call", callUUID, "Record"] params []
  [checkNonEmpty "callUUID" callUUID]

playFile :: ApiSettings -> T.Text -> [PostParam] ->
            IO (Either [T.Text] (Response Bl.ByteString))
playFile settings callUUID params =
  evaluate settings "playFile" POST
  ["Call", callUUID, "Play"] [] params
  [checkNonEmpty "callUUID" callUUID]

stopPlayFile :: ApiSettings -> T.Text ->
                IO (Either [T.Text] (Response Bl.ByteString))
stopPlayFile settings callUUID =
  evaluate settings "stopPlayFile" DELETE
  ["Call", callUUID, "Play"] [] []
  [checkNonEmpty "callUUID" callUUID]

playText :: ApiSettings -> T.Text -> [PostParam] ->
             IO (Either [T.Text] (Response Bl.ByteString))
playText settings callUUID params =
  evaluate settings "playText" POST
  ["Call", callUUID, "Speak"] [] params
  [checkNonEmpty "callUUID" callUUID]

stopPlayText :: ApiSettings -> T.Text ->
                IO (Either [T.Text] (Response Bl.ByteString))
stopPlayText settings callUUID =
  evaluate settings "stopPlayText" DELETE
  ["Call", callUUID, "Speak"] [] []
  [checkNonEmpty "callUUID" callUUID]

sendDTMF :: ApiSettings -> T.Text -> [PostParam] ->
            IO (Either [T.Text] (Response Bl.ByteString))
sendDTMF settings callUUID params =
  evaluate settings "sendDTMF" POST
  ["Call", callUUID, "DTMF"] [] params
  [checkNonEmpty "callUUID" callUUID]

hangupCallRequest :: ApiSettings -> T.Text ->
                     IO (Either [T.Text] (Response Bl.ByteString))
hangupCallRequest settings requestUUID =
  evaluate settings "hangupCallRequest" DELETE
  ["Request", requestUUID] [] []
  [checkNonEmpty "requestUUID" requestUUID]

getConferences :: ApiSettings ->
                  IO (Either [T.Text] (Response Bl.ByteString))
getConferences settings =
  evaluate settings "getConferences" GET
  ["Conference"] [] [] []

getConference :: ApiSettings -> T.Text ->
                 IO (Either [T.Text] (Response Bl.ByteString))
getConference settings conferenceName =
  evaluate settings "getConference" GET
  ["Conference", conferenceName] [] []
  [checkNonEmpty "conferenceName" conferenceName]

hangupAllConferences :: ApiSettings ->
                        IO (Either [T.Text] (Response Bl.ByteString))
hangupAllConferences settings =
  evaluate settings "hangupAllConferences" DELETE
  ["Conference"] [] [] []

hangupConference :: ApiSettings -> T.Text ->
                    IO (Either [T.Text] (Response Bl.ByteString))
hangupConference settings conferenceName =
  evaluate settings "conferenceName" DELETE
  ["Conference", conferenceName] [] []
  [checkNonEmpty "conferenceName" conferenceName]

hangupConferenceMember :: ApiSettings -> T.Text -> T.Text ->
                          IO (Either [T.Text] (Response Bl.ByteString))
hangupConferenceMember settings conferenceName memberId =
  evaluate settings "hangupConferenceMember" DELETE
  ["Conference", conferenceName, "Member", memberId] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

kickConferenceMember :: ApiSettings -> T.Text -> T.Text ->
                        IO (Either [T.Text] (Response Bl.ByteString))
kickConferenceMember settings conferenceName memberId =
  evaluate settings "kickConferenceMember" POST
  ["Conference", conferenceName, "Member", memberId, "Kick"] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

muteConferenceMember :: ApiSettings -> T.Text -> T.Text ->
                        IO (Either [T.Text] (Response Bl.ByteString))
muteConferenceMember settings conferenceName memberId =
  evaluate settings "muteConferenceMember" POST
  ["Conference", conferenceName, "Member", memberId, "Mute"] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

unmuteConferenceMember :: ApiSettings -> T.Text -> T.Text ->
                          IO (Either [T.Text] (Response Bl.ByteString))
unmuteConferenceMember settings conferenceName memberId =
  evaluate settings "unmuteConferenceMember" DELETE
  ["Conference", conferenceName, "Member", memberId, "Mute"] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

playFileToMember :: ApiSettings -> T.Text -> T.Text -> [PostParam] ->
                    IO (Either [T.Text] (Response Bl.ByteString))
playFileToMember settings conferenceName memberId params =
  evaluate settings "playFileToMember" POST
  ["Conference", conferenceName, "Member", memberId, "Play"] [] params
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

stopPlayFileToMember :: ApiSettings -> T.Text -> T.Text ->
                        IO (Either [T.Text] (Response Bl.ByteString))
stopPlayFileToMember settings conferenceName memberId =
  evaluate settings "stopPlayFileToMember" DELETE
  ["Conference", conferenceName, "Member", memberId, "Play"] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

playTextToMember :: ApiSettings -> T.Text -> T.Text -> [PostParam] ->
                    IO (Either [T.Text] (Response Bl.ByteString))
playTextToMember settings conferenceName memberId params =
  evaluate settings "playTextToMember" POST
  ["Conference", conferenceName, "Member", memberId, "Speak"] [] params
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

stopPlayTextToMember :: ApiSettings -> T.Text -> T.Text ->
                        IO (Either [T.Text] (Response Bl.ByteString))
stopPlayTextToMember settings conferenceName memberId =
  evaluate settings "stopPlayTextToMember" DELETE
  ["Conference", conferenceName, "Member", memberId, "Speak"] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

deafMember :: ApiSettings -> T.Text -> T.Text ->
              IO (Either [T.Text] (Response Bl.ByteString))
deafMember settings conferenceName memberId =
  evaluate settings "deafMember" POST
  ["Conference", conferenceName, "Member", memberId, "Deaf"] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

undeafMember :: ApiSettings -> T.Text -> T.Text ->
                IO (Either [T.Text] (Response Bl.ByteString))
undeafMember settings conferenceName memberId =
  evaluate settings "undeafMember" DELETE
  ["Conference", conferenceName, "Member", memberId, "Deaf"] [] []
  [checkNonEmpty "conferenceName" conferenceName,
   checkNonEmpty "memberId" memberId]

startRecordingConference :: ApiSettings -> T.Text -> [PostParam] ->
                            IO (Either [T.Text] (Response Bl.ByteString))
startRecordingConference settings conferenceName params =
  evaluate settings "startRecordingConference" POST
  ["Conference", conferenceName, "Record"] [] params
  [checkNonEmpty "conferenceName" conferenceName]

stopRecordingConference :: ApiSettings -> T.Text ->
                           IO (Either [T.Text] (Response Bl.ByteString))
stopRecordingConference settings conferenceName =
  evaluate settings "stopRecordingConference" DELETE
  ["Conference", conferenceName, "Record"] [] []
  [checkNonEmpty "conferenceName" conferenceName]

createEndpoint :: ApiSettings -> [PostParam] ->
                  IO (Either [T.Text] (Response Bl.ByteString))
createEndpoint settings params =
  evaluate settings "createEndpoint" POST
  ["Endpoint"] [] params []

getEndpoint :: ApiSettings -> T.Text ->
               IO (Either [T.Text] (Response Bl.ByteString))
getEndpoint settings endpointId =
  evaluate settings "createEndpoint" GET
  ["Endpoint", endpointId] [] []
  [checkNonEmpty "endpointId" endpointId]

getEndpoints :: ApiSettings ->
                IO (Either [T.Text] (Response Bl.ByteString))
getEndpoints settings =
  evaluate settings "getEndpoints" GET
  ["Endpoint"] [] [] []

modifyEndpoint :: ApiSettings -> T.Text -> [PostParam] ->
                  IO (Either [T.Text] (Response Bl.ByteString))
modifyEndpoint settings endpointId params =
  evaluate settings "modifyEndpoint" POST
  ["Endpoint", endpointId] [] params
  [checkNonEmpty "endpointId" endpointId]

deleteEndpoint :: ApiSettings -> T.Text ->
                  IO (Either [T.Text] (Response Bl.ByteString))
deleteEndpoint settings endpointId =
  evaluate settings "deleteEndpoint" DELETE
  ["Endpoint", endpointId] [] []
  [checkNonEmpty "endpointId" endpointId]

sendMessage :: ApiSettings -> [PostParam] ->
               IO (Either [T.Text] (Response Bl.ByteString))
sendMessage settings params =
  evaluate settings "sendMessage" POST
  ["Message"] [] params []

getMessages :: ApiSettings -> [QParam] ->
               IO (Either [T.Text] (Response Bl.ByteString))
getMessages settings params =
  evaluate settings "getMessages" GET
  ["Message"] params [] []

getMessage :: ApiSettings -> T.Text ->
              IO (Either [T.Text] (Response Bl.ByteString))
getMessage settings messageUUID =
  evaluate settings "getMessage" GET
  ["Message", messageUUID] [] []
  [checkNonEmpty "messageUUID" messageUUID]

getRentedNumbers :: ApiSettings -> [QParam] ->
                    IO (Either [T.Text] (Response Bl.ByteString))
getRentedNumbers settings params =
  evaluate settings "getRentedNumbers" GET
  ["Number"] params [] []

getRentedNumber :: ApiSettings -> T.Text ->
                   IO (Either [T.Text] (Response Bl.ByteString))
getRentedNumber settings number =
  evaluate settings "getRentedNumber" GET
  ["Number", number] [] [] [checkNonEmpty "number" number]

modifyNumber :: ApiSettings -> T.Text -> [PostParam] ->
                IO (Either [T.Text] (Response Bl.ByteString))
modifyNumber settings number params =
  evaluate settings "modifyNumber" POST
  ["Number", number] [] params []

unrentNumber :: ApiSettings -> T.Text ->
                IO (Either [T.Text] (Response Bl.ByteString))
unrentNumber settings number =
  evaluate settings "unrentNumber" DELETE
  ["Number", number] [] [] [checkNonEmpty "number" number]

availableNumberGroup :: ApiSettings -> [QParam] ->
                        IO (Either [T.Text] (Response Bl.ByteString))
availableNumberGroup settings params =
  evaluate settings "availableNumberGroup" GET
  ["AvailableNumberGroup"] params [] []

rentNumber :: ApiSettings -> T.Text -> [PostParam] ->
              IO (Either [T.Text] (Response Bl.ByteString))
rentNumber settings groupId params =
  evaluate settings "rentNumber" POST
  ["AvailableNumberGroup", groupId] [] params
  [checkNonEmpty "groupId" groupId]

getPricing :: ApiSettings -> [PostParam] ->
              IO (Either [T.Text] (Response Bl.ByteString))
getPricing settings params =
  evaluate settings "getPricing" GET
  ["Pricing"] params [] []

getRecordings :: ApiSettings -> [QParam] ->
                 IO (Either [T.Text] (Response Bl.ByteString))
getRecordings settings params =
  evaluate settings "getRecordings" GET
  ["Recording"] params [] []

getRecording :: ApiSettings -> T.Text ->
                 IO (Either [T.Text] (Response Bl.ByteString))
getRecording settings recordingId =
  evaluate settings "getRecording" GET
  ["Recording", recordingId] [] [] [checkNonEmpty "recordingId" recordingId]

deleteRecording :: ApiSettings -> T.Text ->
                   IO (Either [T.Text] (Response Bl.ByteString))
deleteRecording settings recordingId =
  evaluate settings "deleteRecording" DELETE
  ["Recording", recordingId] [] [] [checkNonEmpty "recordingId" recordingId]
