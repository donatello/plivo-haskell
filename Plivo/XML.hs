module Plivo.XML where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Text.Printf as TP
import Text.XML.Generator
import qualified Data.ByteString as B

data NodeType = Speak | Play | GetDigits | Record | Dial | Message | Number | User
              | Redirect | Wait | Hangup | PreAnswer | Conference | DTMF | Response
              deriving (Show, Eq, Ord)

type Attrib = (String, String)

data Node = Node {
  name :: NodeType,
  attrs :: [Attrib],
  body :: String,
  children :: [Node]
  }
  deriving (Show, Eq)

data PlivoXMLError =
  InvalidAttribute String |
  InvalidNesting String |
  OtherError String
  deriving (Show, Eq)

attrRules :: NodeType -> S.Set String
attrRules Speak = S.fromList ["voice", "language", "loop"]
attrRules Play = S.fromList ["loop"]
attrRules Wait = S.fromList ["length", "silence"]
attrRules Redirect = S.fromList ["method"]
attrRules Hangup = S.fromList ["schedule", "reason"]
attrRules GetDigits = S.fromList ["action", "method", "timeout", "digitTimeout", "finishOnKey",
                                  "numDigits", "retries", "invalidDigitsSound", "validDigits",
                                  "playBeep", "redirect", "digitTimeout"]
attrRules Number = S.fromList ["sendDigits", "sendOnPreanswer"]
attrRules User = S.fromList ["sendDigits", "sendOnPreanswer", "sipHeaders",
                             "webrtc"]
attrRules Dial = S.fromList ["action","method","timeout","hangupOnStar",
                             "timeLimit","callerId", "callerName", "confirmSound",
                             "dialMusic", "confirmKey", "redirect",
                             "callbackUrl", "callbackMethod", "digitsMatch",
                             "sipHeaders"]
attrRules Conference = S.fromList ["muted","beep","startConferenceOnEnter",
                                   "endConferenceOnExit","waitSound","enterSound", "exitSound",
                                   "timeLimit", "hangupOnStar", "maxMembers",
                                   "record", "recordFileFormat", "action", "method", "redirect",
                                   "digitsMatch", "callbackUrl", "callbackMethod",
                                   "stayAlone", "floorEvent"]
attrRules Record = S.fromList ["action", "method", "timeout","finishOnKey",
                               "maxLength", "playBeep", "recordSession",
                               "startOnDialAnswer", "redirect", "fileFormat",
                               "callbackUrl", "callbackMethod"]
attrRules Message = S.fromList ["src", "dst", "type", "callbackUrl", "callbackMethod"]
attrRules _ = S.empty

nestingRules :: NodeType -> S.Set NodeType
nestingRules Response = S.fromList [Speak, Play, GetDigits, Record, Dial, Message,
                                    Redirect, Wait, Hangup, PreAnswer, Conference, DTMF]
nestingRules GetDigits = S.fromList [Speak, Play, Wait]
nestingRules Dial = S.fromList [Number, User]
nestingRules PreAnswer = S.fromList [Play, Speak, GetDigits, Wait, Redirect, Message, DTMF]
nestingRules _ = S.empty

allowedAttribs :: NodeType -> [Attrib] -> Either PlivoXMLError [Attrib]
allowedAttribs ntype attr | (fieldSet attr) `S.isSubsetOf` (attrRules ntype) = Right attr
                          | otherwise = Left (InvalidAttribute errMsg)
  where
    fieldSet = S.fromList . map fst
    errAttrs = S.toList $ S.difference (fieldSet attr) (attrRules ntype)
    errMsg = L.intercalate " " errAttrs

addChild :: Node -> Node -> Either PlivoXMLError Node
addChild parent@(Node pn _ _ pc) child@(Node cn _ _ _) 
  | cn `S.member` (nestingRules pn) = Right $ parent {children = pc ++ [child]}
  | otherwise = Left (InvalidNesting $ TP.printf "%s in %s" (show cn) (show pn))

(<+>) :: Either PlivoXMLError Node -> Either PlivoXMLError Node -> Either PlivoXMLError Node
(<+>) (Right a) (Right b) = addChild a b
(<+>) (Left a) _ = (Left a)
(<+>) (Right _) (Left b) = (Left b)

makeNode :: NodeType -> [Attrib] -> String -> Either PlivoXMLError Node
makeNode ntype attribs body = 
  do
    attrs <- allowedAttribs ntype attribs
    return $ Node ntype attrs body []

makeResponse = makeNode Response [] ""

makePlay attrs body = makeNode Play attrs body

makeSpeak attrs body = makeNode Speak attrs body

makePreAnswer body = makeNode PreAnswer [] body

makeConference attrs confName = makeNode Conference attrs confName

makeDTMF attrs digits = makeNode DTMF attrs digits

makeHangup attrs = makeNode Hangup attrs ""

makeWait attrs = makeNode Wait attrs ""

makeRedirect attrs body = makeNode Redirect attrs body

makeUser attrs user = makeNode User attrs user

makeNumber attrs number = makeNode Number attrs number

makeMessage attrs message = makeNode Message attrs message

makeDial attrs = makeNode Dial attrs ""

makeRecord attrs = makeNode Record attrs ""

makeGetDigits attrs = makeNode GetDigits attrs ""

buildXMLElem :: Node -> Xml Elem
buildXMLElem node@(Node n a b c)
  | children node == [] = 
    xelem (show n) $ xattribs <#> xtext b
  | otherwise = xelem (show n) $ xattribs <#> ch
  where
    xattribs = xattrs . map (\(x, y) -> xattr x y) $ a
    ch = xelems . map buildXMLElem $ c

buildXMLDoc :: Node -> Xml Doc
buildXMLDoc node = doc defaultDocInfo elem
  where
    elem = buildXMLElem node

buildXML :: Node -> B.ByteString
buildXML node = xrender $ buildXMLDoc node
