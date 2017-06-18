{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Page where

import           Control.Lens
import qualified Data.Foldable   as F (for_)
import           Data.Monoid     ((<>))
import           Data.Ord        (Down (..))
import           Data.Sequence   (Seq)
import qualified Data.Text       as T (Text, intercalate, pack, words)
import           Data.Time
import           ISO8601Duration
import           Lucid
import           Types

compsByYear :: Site t -> [(Year, [(Comp, _)])]
compsByYear (Site s) =
  fmap (_1 %~ getDown) $ fmap (fmap (fmap fst)) s
  where
  getDown (Down a) = a

contentPage :: T.Text -> Site (Seq Video) -> Html ()
contentPage subTitle site@(Site s) = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Into the Circle"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [ rel_ "stylesheet" , href_ "https://unpkg.com/purecss@0.6.2/build/pure-min.css" ]
      link_ [ rel_ "stylesheet", href_ "https://unpkg.com/purecss@0.6.2/build/grids-responsive-min.css"]
      link_ [ rel_ "stylesheet", href_ "https://s3-us-west-2.amazonaws.com/colors-css/2.2.0/colors.min.css"]
      link_ [ rel_ "stylesheet", href_ "basics.css"]
      link_ [ rel_ "stylesheet", href_ "content.css"]

    body_ [class_ ""] $ do
      -- This is the header
      div_ [class_ "title pure-g"] $ do
        div_ [class_ "white bg-navy pure-u-1"] $ do
          div_ [class_ "pure-u-1 centered"] $ do
            (h1_ [class_ "mega-biggen"] "Into the Circle")
            p_ [class_ "embiggen"] $ toHtml subTitle
            h3_ [class_ "small-caps"] $
                a_ [href_ "index.html", class_ "embiggen bold white"] "back to the index"

      -- This is the sidebar
      div_ $ do
        div_ [class_ "sidebar navy"] $
          div_ [class_ "centered"] $ do
            a_ [href_ "index.html"] $
              img_ [class_ "pt-1", width_ "100px", height_ "100px", src_ "circles.svg" ]
            div_ [class_ "pure-menu"] $ do
              F.for_ (compsByYear site) $ \(Year y, cs) -> do
                ul_ [class_ "pure-menu-list"] $
                  li_ [class_ "pure-menu-item"] $ do
                    a_ [class_ "pure-menu-link navy bold hover-white embiggen", href_ ("#"<> (T.pack (show y))) ] (toHtml (show y))
                    F.for_ cs $ \(Comp c, _) ->
                      a_ [class_ "pure-menu-link blue hover-white", href_ ("#"<> anchor y c ) ] (toHtml c)

      -- This is the main body
      div_ [class_ "content pure-g navy"] $
        F.for_ s renderYear

      footer
      rawTracking


indexPage :: [Query] -> [Year] -> [Band] -> Html ()
indexPage qs allYears allBands = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Into the Circle"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [ rel_ "stylesheet" , href_ "https://unpkg.com/purecss@0.6.2/build/pure-min.css" ]
      link_ [ rel_ "stylesheet", href_ "https://unpkg.com/purecss@0.6.2/build/grids-responsive-min.css"]
      link_ [ rel_ "stylesheet", href_ "https://s3-us-west-2.amazonaws.com/colors-css/2.2.0/colors.min.css"]
      link_ [ rel_ "stylesheet", href_ "basics.css"]
      link_ [ rel_ "stylesheet", href_ "index.css"]
    body_ [class_ ""] $ do
      -- This is the header
      div_ [class_ "title pure-g"] $ do
        div_ [class_ "white bg-navy pure-u-1"] $
         do div_ [class_ "pure-u-1 centered"] $ do
              img_ [class_ "pt-2", width_ "120px", height_ "120px", src_ "circles-white.svg" ]
              (h1_ [class_ "mega-biggen"] "Into the Circle")

            div_ [class_ "pure-u-1 pure-g"] $ do
              div_ [class_ "pure-u-1-5"] $ " "
              div_ [class_ "silver pure-u-3-5"] $ do
                details qs
              div_ [class_ "pure-u-1-5"] $ " "

      div_ [class_ "content pure-g navy"] $ do
        div_ [class_ "pure-u-1 pure-u-md-1-2 centered"] $ do
          div_ [class_ "squish-big-left  bg-maroon white"] $ do
            div_ [class_ "pure-u-1"] $ h1_ "Years"
          div_ [class_ "squish-big-left"] $ do
            div_ $ do
                F.for_ allYears $ \(Year y) -> do
                  p_ $
                    a_ [class_ "bold embiggen", href_ (T.pack (show y) <> ".html")] (toHtml (show y))

        div_ [class_ "pure-u-1 pure-u-md-1-2 centered"] $ do
          div_ [class_ "squish-big-right bg-maroon white"] $ do
            div_ [class_ "pure-u-1"] $ h1_ "Bands"
          div_ [class_ "squish-big-right"] $ do
            div_ $ do
                F.for_ allBands $ \band -> do
                  p_ $
                    a_ [class_ "bold nowrap", href_ (shortBand band <> ".html")] (toHtml (longBand band))
      footer
      rawTracking

details :: [Query] -> Html ()
details qs =
  let r :: Int -> Query -> Html ()
      r i q = middy i <> a_ [class_ "bold white", href_ $ queryUri q] (toHtml (queryView q))
      middy i | i == 0 = mempty
              | i == numQs - 1 = " and "
              | otherwise = ", "
      numQs = length qs
  in
  do
  p_ "This is an index of recordings made of pipe bands at major and domestic competitions."
  p_ $
   do "Here you can find content by "
      sequence_ (imap r $ qs)
      "."
  p_ "I've attempted to automatically classify the band (and solo) recordings by these fantastic video producers, however the process doesn't always get it right. Contact me if you spot an error!"
  p_ "- Nick Partridge (nkpart@gmail.com)"

renderYear :: (Down Year, _) -> Html ()
renderYear (Down (Year y), inner) =
      div_ [class_ "pure-u-1"] $ do
        a_ [name_ (T.pack (show y))] mempty
        div_ [class_ "pure-u-1 pure-u-md-1-3 align-right bg-maroon white"] (h1_ (toHtml (show y)))
        div_ [class_ "pure-u-1 pure-u-md-2-3 bg-maroon jobby"] (h1_ blankSpan)
        div_ (foldMap (renderComp (Year y)) inner)

blankSpan :: Html ()
blankSpan = span_ (toHtmlRaw ("&nbsp;"::String))

renderComp :: Year -> ((Comp, UTCTime), _) -> Html ()
renderComp (Year y) ((Comp c, _), inner) =
  do
    a_ [name_ (anchor y c)] mempty
    div_ [class_ "pure-u-1 pure-u-md-1-3 align-right upper"] $ do
     h2_ (toHtml c)
     -- h3_ (toHtml $ formatTime defaultTimeLocale "%Y-%m-%d" cd )
    div_ [class_ "pure-u-1 pure-u-md-2-3"] (div_ [class_ "pl-2"] $ foldMap renderBand inner)

anchor :: Int -> T.Text -> T.Text
anchor y c =
  T.pack (show y) <> "-" <> T.intercalate "-" (T.words c)

renderBand :: (Maybe Band, _) -> Html ()
renderBand (mb, inner) =
  do
  F.for_ mb $ \b ->
    h3_ [class_ ""] (toHtml $ longBand b)
  div_ (ul_ $ foldMap renderCorp inner)

renderCorp :: (Corp, _) -> Html ()
renderCorp (c,v) = foldMap (renderSet c) v

renderSet :: Corp -> (Set, Seq Video) -> Html ()
renderSet c (s, v) =
  foldMap (renderVid s c) v

renderVid :: Set -> Corp -> Video -> Html ()
renderVid s c vid =
  let deets = _videoExt vid
  in
   li_ [class_ "pb-03"] $ do
                             div_ $ prefix
                              <>
                              (a_ [href_ (videoUrl vid)] (toHtml $ _videoTitle vid))

                             div_ [class_ "smaller-text gray"] $ do
                               toHtml (durationString (_deetsDuration deets))
                               " - "
                               toHtml (_deetsViews deets <> " views")
                               " - "
                               showSource (view videoChannel vid)
  where
        showSource = toHtml . unChannel
        prefix :: Html ()
        prefix = (case s of
                    MSR     -> "MSR "
                    Medley  -> "Medley "
                    Unknown -> mempty) <>
                 (case c of
                   FullBand -> mempty
                   Pipe     -> span_ [class_ "bold"] "Pipes "
                   Drum     -> span_ [class_ "bold"] "Drums "
                   )


rawTracking :: Html ()
rawTracking = toHtmlRaw $ (id :: String -> String) "\
 \<script>\
 \ (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){ \
 \ (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o), \
 \ m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m) \
 \ })(window,document,'script','https://www.google-analytics.com/analytics.js','ga'); \
 \\
 \ ga('create', 'UA-100591266-1', 'auto'); \
 \ ga('send', 'pageview');\
 \\
 \</script>"

footer :: Html ()
footer =  div_ [class_ "pure-u-1 bg-silver border-top border--grey p1 centered gray footer"]
            "Built by nkpart@gmail.com, 2017."
