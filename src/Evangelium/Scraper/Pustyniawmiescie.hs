module Evangelium.Scraper.Pustyniawmiescie (
    PustyniawmiescieScraper(..)
  )

where

import qualified Evangelium as E

import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.String.Utils as DSU
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.HTTP.Simple (Request(..), parseRequest, setRequestMethod, setRequestBodyURLEncoded)
import Text.XML.HXT.Core


data PustyniawmiescieScraper = PustyniawmiescieScraper

ewangeliaTextFormat :: T.Text -> T.Text
ewangeliaTextFormat = T.strip . T.toLower

-- https://stackoverflow.com/a/9723976/941471
mapTuple3 f (a, b, c) = (f a, f b, f c)

instance E.Scraper PustyniawmiescieScraper where
  scraperName _ = "Pustynia w mieście"

-- url = "http://www.pustyniawmiescie.pl/czytania" POST with data: terazdzien, terazmc, terazrok

  getUrl _ day = E.Url $ "http://www.pustyniawmiescie.pl/czytania/" ++ dayF
      where
        dayF = formatTime defaultTimeLocale "%F" day

  prepareRequest s day = do
    let url = E.getUrl s day
    req' <- parseRequest $ E.unUrl url
    --let reqPost = setRequestMethod "POST" req'
    --let (d', m', y') = toGregorian day
    --let (d, m, y) = mapTuple3 (B.pack . show) (fromIntegral d', fromIntegral m', y')
    --let req = setRequestBodyURLEncoded [ ("terazdzien", d)
    --                                   , ("terazmc", m)
    --                                   , ("terazrok", y)] reqPost
    return req'

  scrapeHtml _ html = (E.Verse verse, E.Gospel gospel)
      where
        -- Sometimes the text is right inside <p><a id="ewangelia_0">...</p>
        -- and sometimes in the next <p>...
        selector = deep $ (hasName "div" >>> hasAttrValue "id" (== "boxmain")) /> (hasName "p")
          -- </ (hasName "a" >>> hasAttrValue "id" (== "#ewangelia_0"))
        cs = map (\(t, c) -> (T.strip . T.pack $ t, c)) $ runLA (hread >>> selector >>> ((this //> getText) &&& (getAttrValue "class"))) $ T.unpack html

        cs' = filter (\(t, _) -> (t /= "")) $ dropWhile ((/= "ewangelia:") . ewangeliaTextFormat . fst) cs

        verse = T.unpack $ fst $ cs' !! 1

        gospel = T.unpack $ T.unlines $ drop 2 $ map fst cs'

