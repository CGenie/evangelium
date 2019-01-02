module Evangelium.Scraper.Mateusz (
    MateuszScraper(..)
  )

where

import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Evangelium as E

import qualified Data.String.Utils as DSU
import qualified Data.Text as T
import Text.XML.HXT.Core


data MateuszScraper = MateuszScraper

instance E.Scraper MateuszScraper where
  scraperName _ = "Mateusz"

  getUrl _ day = E.Url $ "http://www.mateusz.pl/czytania/" ++ yearF ++ "/" ++ dayF ++ ".html"
      where
        dayF = formatTime defaultTimeLocale "%0Y%m%d" day
        yearF = formatTime defaultTimeLocale "%0Y" day

  scrapeHtml _ html = (E.Verse verse, E.Gospel gospel)
      where
        selector = (deep $ (hasName "section") </ (hasName "a" >>> hasAttrValue "name" (== "czytania"))) /> hasName "p"
        cs = runLA (hread >>> selector /> getText) $ T.unpack html
        -- last 2 entities should be: verse number ++ Gospel text
        dropped = drop (length cs - 2) $ map (DSU.strip . unwords . words) cs
        (verse, gospel) = case dropped of
            [] -> (E.notFoundText, E.notFoundText)
            (_:[]) -> (E.notFoundText, E.notFoundText)
            (v:g:[]) -> (v, g)

