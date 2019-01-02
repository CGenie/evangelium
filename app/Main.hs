module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Time.Calendar (Day(..))

import Evangelium (currentDay, getDay, getFullMonth, Scraper(..))
import Evangelium.Format (render)
import Evangelium.Scraper.Mateusz (MateuszScraper(..))
import Evangelium.Scraper.Paulus (PaulusScraper(..))
import Evangelium.Scraper.Pustyniawmiescie (PustyniawmiescieScraper(..))
import Evangelium.Scraper.Wiara (WiaraScraper(..))

data Args = Args {
    date :: Day
  , path :: String
    }


main :: IO ()
main = do
     today <- currentDay
     --dg <- getDay today
     --dgs <- getFullMonth MateuszScraper today
     --dgs <- getFullMonth PaulusScraper today
     dgs <- getFullMonth PustyniawmiescieScraper today
     --dg <- getDay PustyniawmiescieScraper today
     putStrLn $ render PustyniawmiescieScraper dgs
     --dgs <- getFullMonth WiaraScraper today
     --putStrLn $ render WiaraScraper dgs
     --dg <- getDay WiaraScraper today
     --putStrLn $ render WiaraScraper [dg]
