module Evangelium
    ( getDay
    , currentDay
    , getFullMonth
    , Verse(..)
    , Gospel(..)
    , URL(..)
    , DailyGospel(..)
    ) where

import qualified Data.String.Utils as DSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (addDays, toGregorian, fromGregorian, isLeapYear, Day(..))
import Data.Time.Calendar.MonthDay (monthLength)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, localDay, utcToLocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.HTTP.Simple (getResponseBody, httpBS, Request(..), parseRequest)
--import Text.XML.HXT.Core (readString, withParseHTML, withWarnings, yes, no, (>>>), (/>), hasName, hasAttrValue, (//>), getText, runX, xshow, runLA, hread, traceTree, withTraceLevel)
import Text.XML.HXT.Core

newtype Verse = Verse { unVerse :: String }
newtype Gospel = Gospel { unGospel :: String }
newtype URL = URL { unUrl :: String }

data DailyGospel = DailyGospel {
    gDay :: Day
  , gURL :: URL
  , gVerse :: Verse
  , gGospel :: Gospel
}

-- The URL we're going to search
--url = "https://www.paulus.org.pl/czytania?data=2018-12-1"
--url = "http://www.mateusz.pl/czytania/2018/20181124.html"

notFoundText = "-------"

mateuszURL :: Day -> URL
mateuszURL day = URL $ "http://www.mateusz.pl/czytania/" ++ yearF ++ "/" ++ dayF ++ ".html"
    where
      dayF = formatTime defaultTimeLocale "%0Y%m%d" day
      yearF = formatTime defaultTimeLocale "%0Y" day

openURL :: Request -> IO T.Text
openURL x = do
      content <- httpBS x
      return $ TE.decodeUtf8 $ getResponseBody content

getDay :: Day -> IO DailyGospel
getDay day = do
     let url = mateuszURL day
     req <- parseRequest $ unUrl url
     html <- openURL req
     --let selector = (deep $ (hasName "a" >>> hasAttrValue "name" (== "czytania")) </ (hasName "section"))
     let selector = (deep $ (hasName "section") </ (hasName "a" >>> hasAttrValue "name" (== "czytania"))) /> hasName "p"
     let cs = runLA (hread >>> selector /> getText) $ T.unpack html
     -- last 2 entities should be: verse number ++ Gospel text
     let dropped = drop (length cs - 2) $ map (DSU.strip . unwords . words) cs
     let (verse, gospel) = case dropped of
            [] -> (notFoundText, notFoundText)
            (_:[]) -> (notFoundText, notFoundText)
            (v:g:[]) -> (v, g)
     --mapM_ (\c -> putStrLn $ c ++ "\n-----") cs
     --putStrLn $ "Length: " ++ (show $ length cs)

     return $ DailyGospel day url (Verse verse) (Gospel gospel)

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
getFullMonth :: Day -> IO [DailyGospel]
getFullMonth day = do
  let days = fullMonthRange day
  vgs <- mapM getDay days
  return vgs

