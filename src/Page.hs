{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Page where

import           Control.Lens
import qualified Data.Foldable   as F (for_)
import           Data.List       (sort)
import qualified Data.Map        as M
import           Data.Maybe      (isJust)
import           Data.Monoid     ((<>))
import           Data.Ord        (Down (..))
import           Data.Sequence   (Seq)
import qualified Data.Text       as T (Text, intercalate, pack, words)
import           Data.Time
import           Extractor.Bands
import           ISO8601Duration
import           Lucid
import           Types

compsByYear :: Site t -> [(Year, [(Comp, _)])]
compsByYear (Site s) =
  fmap (_1 %~ getDown) $ fmap (fmap (fmap fst)) s
  where
  getDown (Down a) = a

contentPage :: T.Text -> [Year] -> [Band] -> Site (Seq Video) -> Html ()
contentPage subTitle years bands site@(Site s) = do
  doctype_
  html_ $ do
    head_ $ do
      title_ "Into the Circle"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [ rel_ "stylesheet" , href_ "https://unpkg.com/purecss@0.6.2/build/pure-min.css" ]
      link_ [ rel_ "stylesheet", href_ "https://unpkg.com/purecss@0.6.2/build/grids-responsive-min.css"]
      link_ [ rel_ "stylesheet", href_ "https://s3-us-west-2.amazonaws.com/colors-css/2.2.0/colors.min.css"]
      link_ [ rel_ "stylesheet", href_ "https://opensource.keycdn.com/fontawesome/4.7.0/font-awesome.min.css"]
      link_ [ rel_ "stylesheet", href_ "basics.css"]
      link_ [ rel_ "stylesheet", href_ "content.css"]

    body_ [class_ ""] $ do
      div_ [class_ "uptown"] $ upper years bands
      div_ [class_ "downtown"] $ do
      -- This is the header
        div_ [class_ "title pure-g"] $ do
          div_ [class_ "white bg-navy pure-u-1"] $ do
            div_ [class_ "pure-u-1 centered"] $ do
              (h1_ [class_ "mega-biggen"] $ toHtml subTitle)

        -- This is the sidebar
        div_ $ do
          div_ [class_ "lightest-grey sidebar navy"] $ do
            div_ [class_ ""] $ do
              div_ [class_ "pure-menu"] $ do
                F.for_ (compsByYear site) $ \(Year y, cs) -> do
                  --- YEAR ---
                  ul_ [class_ "pure-menu-list"] $
                    li_ [class_ "pure-menu-item"] $ do
                      a_ [class_ "pure-menu-link navy bold hover-white embiggen", href_ ("#"<> (T.pack (show y))) ] (toHtml (show y))
                      F.for_ cs $ \(Comp c, _) ->
                        --- COMP ---
                        a_ [class_ "pure-menu-link blue hover-white", href_ ("#"<> anchor y c ) ] (toHtml c)
            div_ [class_ "footer"] $
              "Built by nkpart@gmail.com, 2017."

        -- This is the main body
        div_ [class_ "content pure-g navy"] $
          F.for_ s renderYear

        rawTracking


indexPage :: [Query] -> [Year] -> [Band] -> Html ()
indexPage qs years bands = do
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
      div_ [class_ "uptown"] $ upper years bands
      div_ [class_ "downtown"] $ do
        -- This is the sidebar
        div_ $ do
          div_ [class_ "lightest-grey sidebar navy"] $
            div_ [class_ "footer"] $
              "Built by nkpart@gmail.com, 2018."

        -- This is the main body
        div_ [class_ "content pure-g navy"] $
          div_ [class_ "bg-white pure-u-1 pl-2"] $
            do div_ [class_ "pure-u-1"] $ do
                  --  img_ [class_ "pt-2", width_ "120px", height_ "120px", src_ "circles.svg" ]
                   (h1_ [class_ "mega-biggen"] "Into the Circle")
               div_ [class_ "pure-u-1 pure-g"] $ do
                div_ [class_ "details"] $ details qs

      rawTracking

yearPageUri :: Year -> T.Text
yearPageUri (Year y) =
  (T.pack (show y) <> ".html")

bandPageUri :: Band -> T.Text
bandPageUri bb = shortBand bb <> ".html"

upper :: [Year] -> [Band] -> Html ()
upper years bands = do
  let
      after2011 = filter (\(Year x) -> x >= 2011) years
      from2001_2010 = filter (\(Year x) -> x >= 2001 && x < 2011) years
      before2001 = filter (\(Year x) -> x < 2001) years

      rr OtherBand    = (Nothing, [OtherBand])
      rr b@(Band _ r) | b == soloist = (Nothing, [b])
                      | otherwise = (Just r, [b])
      allByRegion =
        M.fromListWith (<>) (fmap rr bands)

      (bandsByRegion, rests) = M.partitionWithKey (\k _ -> isJust k) allByRegion
      others :: [Band]
      others = M.elems rests >>= id

  div_ [class_ "test-logo"] $
    a_ [href_ "index.html"] $ img_ [class_ "pt-1", width_ "100px", height_ "100px", src_ "circles.svg" ]
  div_ [class_ "test-menus black bold"] $ do
    div_ [class_ "pure-menu year-menu", style_ "display: inline-block;"] $ do
      li_ [class_ "pure-menu-item pure-menu-has-children pure-menu-allow-hover"] $ do
        a_ [href_ "#", class_ "pure-menu-link"] "2011-2017"
        ul_ [class_ "pure-menu-children"] $
          F.for_ after2011 $ \y -> do
            li_ [class_ "pure-menu-item"] $ a_ [href_ (yearPageUri y), class_ "pure-menu-link"] (toHtml . show . unYear $ y)
      li_ [class_ "pure-menu-item pure-menu-has-children pure-menu-allow-hover"] $ do
         a_ [href_ "#", class_ "pure-menu-link"] "2001-2010"
         ul_ [class_ "pure-menu-children"] $
          F.for_ from2001_2010 $ \y -> do
            li_ [class_ "pure-menu-item"] $ a_ [href_ (yearPageUri y), class_ "pure-menu-link"] (toHtml . show . unYear $ y)

      li_ [class_ "pure-menu-item pure-menu-has-children pure-menu-allow-hover"] $ do
        a_ [href_ "#", class_ "pure-menu-link"] "Pre 2001"
        ul_ [class_ "pure-menu-children"] $
          F.for_ before2001 $ \y -> do
            li_ [class_ "pure-menu-item"] $ a_ [href_ (yearPageUri y), class_ "pure-menu-link"] (toHtml . show . unYear $ y)

    div_ [class_ "pure-menu band-menu", style_ "display: inline-block;"] $ do
      F.for_ (M.toList bandsByRegion) $ \(region, bbs) -> do
        li_ [class_ "pure-menu-item pure-menu-has-children pure-menu-allow-hover"] $ do
          a_ [href_ "#", class_ "pure-menu-link"] (toHtml $ maybe "Other Bands" unRegion region)
          ul_ [class_ "pure-menu-children"] $
            F.for_ (sort bbs) $ \b -> do
              li_ [class_ "pure-menu-item"] $ a_ [href_ (bandPageUri b), class_ "pure-menu-link"] (toHtml . longBand $ b)

    div_ [class_ "pure-menu rest-menu", style_ "display: inline-block;"] $ do
      F.for_ (sort others) $ \b -> do
        li_ [class_ "pure-menu-item"] $ a_ [href_ (bandPageUri b), class_ "pure-menu-link"] (toHtml . longBand $ b)

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
  p_ $ b_ "Use the menus at the top to browse by Year, or by Band (split by region to make the lists a little shorter)."
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
        div_ (foldMap (renderComp (Year y)) inner)

blankSpan :: Html ()
blankSpan = span_ (toHtmlRaw ("&nbsp;"::String))

renderComp :: Year -> ((Comp, UTCTime), _) -> Html ()
renderComp (Year y) ((Comp c, _), inner) =
  div_ $
  do
    div_ [class_ "pure-u-1 bg-maroon white sticky"] $ do
      div_ [class_ "pl-2"] $ (h1_ ((toHtml (show y) <> " - " <> toHtml c)))
    a_ [name_ (anchor y c)] mempty
    div_ [class_ "pure-u-1"] (div_ [class_ ""] $ foldMap renderBand inner)

anchor :: Int -> T.Text -> T.Text
anchor y c =
  T.pack (show y) <> "-" <> T.intercalate "-" (T.words c)

renderBand :: (Maybe Band, _) -> Html ()
renderBand (mb, inner) =
  div_ $
  do
  F.for_ mb $ \b ->
    div_ [class_ "sticky sticky-2 pure-u-1 lightest-grey"] $ do
      h3_ [class_ "pl-2"] (toHtml $ longBand b)
  div_ [class_ "pl-2"] (foldMap renderCorp inner)

renderCorp :: (Corp, _) -> Html ()
renderCorp (c,v) = foldMap (renderSet c) v

renderSet :: Corp -> (Set, Seq Video) -> Html ()
renderSet c (s, v) =
  foldMap (renderVid s c) v

renderVid :: Set -> Corp -> Video -> Html ()
renderVid _s _c vid =
  let deets = _videoExt vid
  in
   div_ [class_ "pb-03 test"] $ do
                             div_ $ (a_ [href_ (videoUrl vid)] $ img_ [src_ (_videoThumbnail vid)] )
                             div_ (a_ [href_ (videoUrl vid)] (toHtml $ _videoTitle vid))
                             div_ [class_ "smaller-text gray"] $ do
                               toHtml (durationString (_deetsDuration deets))
                               " - "
                               toHtml (_deetsViews deets <> " views")
                               " - "
                               a_ [href_ "#"] $ toHtml (unChannel $ view videoChannel vid)

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
