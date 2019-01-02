module Spec (xml, parseXml) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.XML.HXT.Core


xml = TE.decodeUtf8 "<root><a v='1'><b></b><c p='11'>bb</c></a><a v='2'><c p='22'>cc</c></a></root>"

parseXml = cs
    where
      --let selector = (deep $ (hasName "c"))
      selector = (deep $ (hasName "a") </ (hasName "b")) /> hasName "c"
      --let selector = (deep $ (hasName "a" >>> filterAxis >>> descendantAxis >>> hasName "b"))
      cs = runLA (hread >>> selector >>> ((this /> getText) &&& getAttrValue "p")) $ T.unpack xml
     --mapM_ (\c -> putStrLn $ c ++ "\n-----") cs
     --putStrLn $ "Length: " ++ (show $ length cs)
