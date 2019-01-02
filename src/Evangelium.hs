module Evangelium
    ( getDay
    , currentDay
    , getFullMonth
    , Verse(..)
    , Gospel(..)
    , Url(..)
    , DailyGospel(..)
    , Scraper(..)
    , notFoundText
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (addDays, toGregorian, fromGregorian, isLeapYear, Day(..))
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, localDay, utcToLocalTime)
import Network.HTTP.Simple (getResponseBody, httpBS, Request(..), parseRequest)
--import Text.XML.HXT.Core (readString, withParseHTML, withWarnings, yes, no, (>>>), (/>), hasName, hasAttrValue, (//>), getText, runX, xshow, runLA, hread, traceTree, withTraceLevel)

newtype Verse = Verse { unVerse :: String }
newtype Gospel = Gospel { unGospel :: String }
newtype Url = Url { unUrl :: String }

notFoundText = "-------"

class Scraper a where
  scraperName :: a -> String
  getUrl :: a -> Day -> Url
  prepareRequest :: a -> Day -> IO Request
  prepareRequest s day = do
    let url = getUrl s day
    req <- parseRequest $ unUrl url
    return req
  scrapeHtml :: a -> T.Text -> (Verse, Gospel)
  getDay :: a -> Day -> IO DailyGospel
  getDay s day = do
     let url = getUrl s day
     req <- prepareRequest s day
     html <- openUrl req
     let (verse, gospel) = scrapeHtml s html
     return $ DailyGospel day url verse gospel

data DailyGospel = DailyGospel {
    gDay :: Day
  , gUrl :: Url
  , gVerse :: Verse
  , gGospel :: Gospel
}


openUrl :: Request -> IO T.Text
openUrl x = do
      content <- httpBS x
      return $ TE.decodeUtf8 $ getResponseBody content

currentDay :: IO Day
currentDay = do
  currentUTC <- getCurrentTime
  currentTimeZone <- getCurrentTimeZone
  let lt = utcToLocalTime currentTimeZone currentUTC
  return $ localDay lt

-- given a day, return all days from that month
fullMonthRange :: Day -> [Day]
fullMonthRange day = map (\i -> addDays i firstDay) [0..(ml - 1)]
  where
    (y, m, d) = toGregorian day
    firstDay = fromGregorian y m 1
    ml = fromIntegral $ monthLength (isLeapYear y) m

-- Downloads the full month given a day in the month
getFullMonth :: Scraper s => s -> Day -> IO [DailyGospel]
getFullMonth s day = do
  -- TODO: parallelize this
  let days = fullMonthRange day
  vgs <- mapM (getDay s) days
  return vgs

