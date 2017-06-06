{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Page where

import           Control.Lens
import qualified Data.Foldable   as F (for_)
import qualified Data.Map.Strict as S
import           Data.Monoid     ((<>))
import           Data.Ord        (Down (..))
import qualified Data.Text       as T (Text, intercalate, pack, words)
import           Lucid
import           Types

template :: [Query] -> Site [Video] -> Html ()
template qs (Site s) = do
  let years = fmap (_1 %~ getDown) $ S.toList (fmap (S.keys) s)
  doctype_
  html_ $ do
    head_ $ do
      title_ "Into the Circle"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [ rel_ "stylesheet" , href_ "https://unpkg.com/purecss@0.6.2/build/pure-min.css" ]
      link_ [ rel_ "stylesheet", href_ "https://unpkg.com/purecss@0.6.2/build/grids-responsive-min.css"]
      link_ [ rel_ "stylesheet", href_ "https://s3-us-west-2.amazonaws.com/colors-css/2.2.0/colors.min.css"]
      link_ [ rel_ "stylesheet", href_ "site.css"]
    body_ [class_ ""] $ do
      div_ [class_ "title pure-g"] $ do
        div_ [class_ "white bg-navy pure-u-1"] $
         do div_ [class_ "pure-u-1-3 align-right"] (h1_ [class_ "mega-biggen"] "Into the Circle")
            div_ [class_ "pure-u-2-3"] $ do
              div_ [class_ "pl-2 silver"] $ do
                details qs

      div_ [id_ "layout", class_ ""] $ do
        div_ [class_ "sidebar navy"] $
          div_ [class_ "centered"] $ do
            a_ [href_"/"] $ img_ [class_ "pt-1", width_ "100px", height_ "100px", src_ "circles.svg" ]
            -- h1_ [class_ "brand-title navy p1"] "Pick a year"
            div_ [class_ "pure-menu"] $
                  F.for_ years $ \(Year y, cs) -> do
                    ul_ [class_ "pure-menu-list"] $
                        li_ [class_ "pure-menu-item"] $ do
                          a_ [class_ "pure-menu-link navy bold hover-white embiggen", href_ ("#"<> (T.pack (show y))) ] (toHtml (show y))
                          F.for_ cs $ \(Comp c) ->
                            a_ [class_ "pure-menu-link blue hover-white", href_ ("#"<> anchor y c ) ] (toHtml c)

      div_ [class_ "content pure-g navy"] $
        S.foldMapWithKey renderYear s

details :: [Query] -> Html ()
details qs =
  let r :: Int -> Query -> Html ()
      r i (Username u) = middy i <> a_ [class_ "bold white", href_ $ "https://youtube.com/user/" <> u] (toHtml u)
      r i (ChannelId c d) = middy i <> a_ [class_ "bold white", href_ $ "https://youtube.com/channel/" <> c] (toHtml d)
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
  p_ "I've attempted to automatically classify the band (and solo) recordings by these fantastic video producers, however the process doesn't always work. Contact me if you spot an error!"
  p_ "- Nick Partridge (nkpart@gmail.com)"

getDown :: Down t -> t
getDown (Down a) = a

renderYear :: Down Year -> _ -> Html ()
renderYear (Down (Year y)) inner =
  do
      div_ [class_ "pure-u-1"] $ do
        a_ [name_ (T.pack (show y))] mempty
        div_ [class_ "pure-u-1-3 align-right bg-maroon white"] (h1_ (toHtml (show y)))
        div_ [class_ "pure-u-2-3 bg-maroon"] (h1_ x)
        div_ (S.foldMapWithKey (renderComp (Year y)) inner)

x :: Html ()
x = span_ (toHtmlRaw ("&nbsp;"::String))

renderComp :: Year -> Comp -> _ -> Html ()
renderComp (Year y) (Comp c) inner =
  do
    a_ [name_ (anchor y c)] mempty
    div_ [class_ "pure-u-1 pure-u-md-1-3 align-right upper border-top border--olive"] (h2_ (toHtml c))
    div_ [class_ "pure-u-1 pure-u-md-2-3 border-top border--olive"] (div_ [class_ "pl-2"] $ S.foldMapWithKey renderBand inner)

anchor :: Int -> T.Text -> T.Text
anchor y c =
  T.pack (show y) <> "-" <> T.intercalate "-" (T.words c)

renderBand :: Band -> _ -> Html ()
renderBand (b) inner =
  h3_ [class_ "border-bottom border--red"] (f b) <> div_ (ul_ $ S.foldMapWithKey renderCorp inner)
  where f (Band b')=  toHtml b'
        f (OtherBand) = "Other Bands"

renderCorp :: Corp -> _ -> Html ()
renderCorp c = S.foldMapWithKey (renderSet c)

renderSet :: Corp -> Set -> [Video] -> Html ()
renderSet c s =
  foldMap (renderVid s c)

renderVid :: Set -> Corp -> Video -> Html ()
renderVid s c vid = li_ [] $ prefix <> (a_ [href_ (videoUrl vid)] (toHtml $ _videoTitle vid)) <> " [" <> showSource (_videoSource vid) <>  "]"
  where
        showSource (Username u)       = toHtml u
        showSource (ChannelId _ desc) = toHtml desc
        prefix :: Html ()
        prefix = span_ [] (case s of
                    MSR     -> "MSR "
                    Medley  -> "Medley "
                    Unknown -> mempty) <>
                    span_ [class_ "bold"]
                 (case c of
                   FullBand -> mempty
                   Pipe     -> "Pipes "
                   Drum     -> "Drums "
                   )
