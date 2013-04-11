import Plivo.XML
import Plivo.RestApi
import qualified Data.ByteString as B


testResponse :: Either PlivoXMLError Node
testResponse = do
  resp <- makeResponse
  speak <- makeSpeak [("loop", "2")] "Hello World"
  play <- makePlay [("loop", "3")] "http://something.com/thingy.mp3"
  preAnswer <- makePreAnswer "lol"
  preAnswerSpeak <- addChild speak preAnswer
  return resp >>=
    addChild preAnswerSpeak >>=
    addChild speak >>=
    addChild play

main :: IO ()
main = do
  case testResponse of
    Left a -> print a
    Right a -> B.putStr . buildXML $ a
  putStrLn ""
  putStrLn . show $ testResponse

  settings <- initApiSettings "MAOTBINZLKMJDLZDK2ZD" "YmY5YTI2YmIzNWFmMmYyZjY3NzkxNGQ3M2Y3OTBm"

  (c, r) <- get_account settings
  putStrLn (show c)
  B.putStrLn r
