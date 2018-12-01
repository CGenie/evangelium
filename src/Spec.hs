module Spec (xml, parseXml) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.XML.HXT.Core


xml = TE.decodeUtf8 "<root><a><b></b><c>bb</c></a><a><c>cc</c></a></root>"

parseXml :: IO ()
parseXml = do
     --let selector = (deep $ (hasName "c"))
     let selector = (deep $ (hasName "a") </ (hasName "b")) /> hasName "c"
     --let selector = (deep $ (hasName "a" >>> filterAxis >>> descendantAxis >>> hasName "b"))
     let cs = runLA (hread >>> selector /> getText) $ T.unpack xml
     mapM_ (\c -> putStrLn $ c ++ "\n-----") cs
     putStrLn $ "Length: " ++ (show $ length cs)
