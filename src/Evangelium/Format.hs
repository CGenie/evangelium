module Evangelium.Format (
    formatHtml
  , render
  )
where

import Data.List (minimum, maximum)
import qualified Data.Text as T
import Data.Time.Calendar()
import Data.Time.Format (formatTime, defaultTimeLocale)
import Evangelium (Verse(..), Gospel(..), URL(..), DailyGospel(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Internal (textValue)


isoDay day = formatTime defaultTimeLocale "%F" day

formatDG :: DailyGospel -> H.Html
formatDG DailyGospel {gDay=d, gURL=URL u, gVerse=Verse v, gGospel=Gospel g} = H.section H.! A.class_ "section" $ do
  H.div H.! A.class_ "container" $ do
    H.h1 H.! A.class_ "title" $ H.toHtml $ isoDay d
    H.div H.! A.class_ "subtitle" $ do
      H.h3 $ H.toHtml v
      H.h4 $ do
        H.a H.! A.href (textValue $ T.pack u) $ H.toHtml u
    H.p $ H.toHtml g

formatHtml :: [DailyGospel] -> H.Html
formatHtml dgs = H.docTypeHtml $ do
    H.head $ do
      H.meta H.! A.charset "utf-8"
      H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
      H.script H.! A.src "https://use.fontawesome.com/releases/v5.3.1/js/all.js" $ ""
      H.title $ H.toHtml $ "Ewangelie " ++ (isoDay minDay) ++ " -- " ++ (isoDay maxDay)
      H.link H.! A.href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.2/css/bulma.min.css" H.! A.rel "stylesheet"
    H.body $ do
      H.section H.! A.class_ "hero" $ do
        H.div H.! A.class_ "hero-body" $ do
          H.div H.! A.class_ "container" $ do
            H.h1 H.! A.class_ "title" $ H.preEscapedToHtml $ "Ewangelie " ++ (isoDay minDay) ++ " &ndash; " ++ (isoDay maxDay)
      mapM_ formatDG dgs
  where
    minDay = minimum $ map gDay dgs
    maxDay = maximum $ map gDay dgs

render :: [DailyGospel] -> String
render = renderHtml . formatHtml
