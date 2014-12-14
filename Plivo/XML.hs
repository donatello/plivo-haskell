{-# LANGUAGE OverloadedStrings #-}

module Plivo.XML
       (
         PlivoXMLError
       , Node(..)
       , addChild
       , makeResponse
       , makePlay
       , makeSpeak
       , makeConference
       , makeDTMF
       , makeHangup
       , makeWait
       , makeRedirect
       , makeUser
       , makeNumber
       , makeMessage
       , makeDial
       , makeRecord
       , makeGetDigits
       , buildXML
       ) where

import qualified Data.Set as S
import           Text.XML.Generator
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import           Data.Either ()

data NodeType = Speak | Play | GetDigits | Record | Dial | Message | Number | User
              | Redirect | Wait | Hangup | Conference | DTMF | Response
              deriving (Show, Eq, Ord)

type Attrib = (T.Text, T.Text)

data Node = Node {
  name :: NodeType,
  attributes :: [Attrib],
  content :: T.Text,
  children :: [Node]
  }
  deriving (Show, Eq)

data PlivoXMLError =
  InvalidAttribute T.Text |
  InvalidNesting T.Text |
  OtherError T.Text
  deriving (Show, Eq)

nestingRulesMap :: M.Map NodeType (S.Set NodeType)
nestingRulesMap = M.map S.fromList $ M.fromList [
    (Response,  [Speak, Play, GetDigits, Record, Dial, Message,
                 Redirect, Wait, Hangup, Conference, DTMF])
  , (GetDigits, [Speak, Play, Wait])
  , (Dial,      [Number, User])
  ]

attrRulesMap :: M.Map NodeType (S.Set T.Text)
attrRulesMap = M.map S.fromList $ M.fromList [
    (Speak,      ["voice", "language", "loop"])
  , (Play,       ["loop"])
  , (Wait,       ["length", "silence"])
  , (Redirect,   ["method"])
  , (Hangup,     ["schedule", "reason"])
  , (GetDigits,  ["action", "method", "timeout", "digitTimeout", "finishOnKey",
                  "numDigits", "retries", "invalidDigitsSound", "validDigits",
                  "playBeep", "redirect", "digitTimeout"])
  , (Number,     ["sendDigits", "sendOnPreanswer"])
  , (User,       ["sendDigits", "sendOnPreanswer", "sipHeaders",
                  "webrtc"])
  , (Dial,       ["action","method","timeout","hangupOnStar",
                  "timeLimit","callerId", "callerName", "confirmSound",
                  "dialMusic", "confirmKey", "redirect",
                  "callbackUrl", "callbackMethod", "digitsMatch",
                  "sipHeaders"])
  , (Conference, ["muted","beep","startConferenceOnEnter",
                  "endConferenceOnExit","waitSound","enterSound", "exitSound",
                  "timeLimit", "hangupOnStar", "maxMembers",
                  "record", "recordFileFormat", "action", "method", "redirect",
                  "digitsMatch", "callbackUrl", "callbackMethod",
                  "stayAlone", "floorEvent"])
  , (Record,     ["action", "method", "timeout","finishOnKey",
                  "maxLength", "playBeep", "recordSession",
                  "startOnDialAnswer", "redirect", "fileFormat",
                  "callbackUrl", "callbackMethod"])
  , (Message,    ["src", "dst", "type", "callbackUrl", "callbackMethod"])
  ]

isValidAttr :: NodeType -> T.Text -> Bool
isValidAttr nodeType fieldName = S.member fieldName $
  M.findWithDefault S.empty nodeType attrRulesMap

isValidNesting :: NodeType -> NodeType -> Bool
isValidNesting child parent = S.member child $
  M.findWithDefault S.empty parent nestingRulesMap

allowedAttribs :: NodeType -> [Attrib] -> Either PlivoXMLError [Attrib]
allowedAttribs ntype attr
  | T.null errMsg = return attr
  | otherwise = Left (InvalidAttribute errMsg)
  where
    errMsg = T.intercalate " " $ filter (not.isValidAttr ntype) (map fst attr)

addChild :: Node -> Node -> Either PlivoXMLError Node
addChild child@(Node cn _ _ _) parent@(Node pn _ _ pc)
  | isValidNesting cn pn = return $ parent {children = pc ++ [child]}
  | otherwise = Left (InvalidNesting $ T.concat.map T.pack $
                      [(show cn), " in ", (show pn)])

makeNode :: NodeType -> [Attrib] -> T.Text -> Either PlivoXMLError Node
makeNode ntype attribs body =
  do
    attrs <- allowedAttribs ntype attribs
    return $ Node ntype attrs body []

makeResponse :: Either PlivoXMLError Node
makeResponse = makeNode Response [] ""

makePlay :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makePlay attrs body = makeNode Play attrs body

makeSpeak :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makeSpeak attrs body = makeNode Speak attrs body

makeConference :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makeConference attrs confName = makeNode Conference attrs confName

makeDTMF :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makeDTMF attrs digits = makeNode DTMF attrs digits

makeHangup :: [Attrib] -> Either PlivoXMLError Node
makeHangup attrs = makeNode Hangup attrs ""

makeWait :: [Attrib] -> Either PlivoXMLError Node
makeWait attrs = makeNode Wait attrs ""

makeRedirect :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makeRedirect attrs body = makeNode Redirect attrs body

makeUser :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makeUser attrs user = makeNode User attrs user

makeNumber :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makeNumber attrs number = makeNode Number attrs number

makeMessage :: [Attrib] -> T.Text -> Either PlivoXMLError Node
makeMessage attrs message = makeNode Message attrs message

makeDial :: [Attrib] -> Either PlivoXMLError Node
makeDial attrs = makeNode Dial attrs ""

makeRecord :: [Attrib] -> Either PlivoXMLError Node
makeRecord attrs = makeNode Record attrs ""

makeGetDigits :: [Attrib] -> Either PlivoXMLError Node
makeGetDigits attrs = makeNode GetDigits attrs ""

buildXMLElem :: Node -> Xml Elem
buildXMLElem node@(Node n a b c)
  | children node == [] = 
    xelem (T.pack $ show n) $ xattribs <#> xtext b
  | otherwise = xelem (T.pack $ show n) $ xattribs <#> ch
  where
    xattribs = xattrs . map (\(x, y) -> xattr x y) $ a
    ch = xelems . map buildXMLElem $ c

buildXML :: Either PlivoXMLError Node -> Either PlivoXMLError B.ByteString
buildXML val = case val of
  Left e -> Left e
  Right node -> return $ xrender $ doc defaultDocInfo $ buildXMLElem node
