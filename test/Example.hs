import Plivo.XML
import Control.Applicative (pure)
import qualified Data.ByteString as B


testResponse :: Either PlivoXMLError Node
testResponse = do
  r <- makeResponse
  s <- makeSpeak [("loop", "2")] "Hello World"
  p <- makePlay [("loop", "3")] "http://something.com/thingy.mp3"
  pr <- makePreAnswer "lol"
  prr <- (pure pr) <+> (pure s)
  --r <- foldM addChild r [s, p]
  res <- (pure r) <+> (pure prr) <+> (pure s) <+> (pure p)
  return res

main :: IO ()
main = do
  let resp = testResponse
  case resp of
    Left a -> print a
    Right a -> B.putStr . buildXML $ a
  putStrLn ""
  putStrLn . show $ testResponse

