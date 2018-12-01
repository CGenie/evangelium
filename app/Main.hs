module Main where

import Evangelium (currentDay, getDay, getFullMonth)
import Evangelium.Format (render)

main :: IO ()
main = do
     today <- currentDay
     --dg <- getDay today
     dgs <- getFullMonth today
     putStrLn $ render dgs
