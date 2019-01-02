module Evangelium.Scraper.Wiara (
    WiaraScraper(..)
  )

where

import Data.List (intercalate)
import qualified Data.String.Utils as DSU
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Evangelium as E

import Data.Char (toLower)
import qualified Data.Text as T
import Text.XML.HXT.Core

{-
Structure of the HTML is:

<p class=block-title />
<p class=bible-verse />
<p class=indent />
...

and this is repeated couple of times.
We are interested in the last such block.

So: reverse the whole p list, takeWhile class!="block-title" and reverse again.

EDIT: safer to takeWhile text != "EWANGELIA" because Sunday's page doesn't have the classes...

For the Gospel:
- first <p class="indent" /> is the Gospel "motto"
- second <p class="indent" /> is "Słowa Ewangelii wg..."
- third and later <p class="indent" /> is the Gospel text
- last <p class="indent" /> is "Oto Słowo Pańskie"

NOTE: The whole HTML is unstable.
For example: Sundays are different, 2018-12-26 doesn't have blocks...
-}


data WiaraScraper = WiaraScraper

lowercase :: String -> String
lowercase "" = ""
lowercase (x:xs) = (toLower x):(lowercase xs)

instance E.Scraper WiaraScraper where
  scraperName _ = "liturgia.wiara.pl"

-- url = "https://liturgia.wiara.pl/kalendarz/67b53.Czytania-mszalne/2019-01-16"

  getUrl _ day = E.Url $ "https://liturgia.wiara.pl/kalendarz/67b53.Czytania-mszalne/" ++ dayF
      where
        dayF = formatTime defaultTimeLocale "%F" day

  scrapeHtml _ html = (E.Verse verse, E.Gospel gospel)
      where
        selector = (deep $ (hasName "div") </ (hasName "div" >>> hasAttrValue "class" (== "content_index"))) /> hasName "p"
        cs = runLA (hread >>> selector >>> ((this //> getText) &&& (getAttrValue "class"))) $ T.unpack html
        --lastBlock = reverse $ takeWhile (\(_, c) -> c /= "block-title") $ reverse cs
        -- TODO: fix "ewangelia krótsza" and <em> and <i> elements
        -- TODO: Sometimes there are no blocks, then use text find and replace (2018-12-26)
        lastBlock = reverse $ takeWhile (\(t, _) -> not $ DSU.startswith "ewangelia" (lowercase t)) $ reverse cs

        --findVerse [] = E.notFoundText
        --findVerse ((x, "bible-verse"):_) = x
        --findVerse (_:xs) = findVerse xs
        --verse = findVerse lastBlock
        verse = fst $ lastBlock !! 0

        gospel = intercalate "\n" $ map fst $ reverse $ drop 1 $ reverse $ drop 3 lastBlock -- drop 3 because "bible-verse" is the first element
        --gospel = show cs
        -- last 2 entities should be: verse number ++ Gospel text
        --dropped = drop (length cs - 2) $ map (DSU.strip . unwords . words) cs
        --(verse, gospel) = case dropped of
        --    [] -> (E.notFoundText, E.notFoundText)
        --    (_:[]) -> (E.notFoundText, E.notFoundText)
        --    (v:g:[]) -> (v, g)

