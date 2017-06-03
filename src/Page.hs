{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}

module Page where

import           Control.Applicative (empty, pure, (<|>))
import           Control.Lens
import qualified Data.Foldable       as F (for_)
import qualified Data.Map.Strict     as S
import           Data.Monoid         ((<>))
import           Data.Ord            (Down (..))
import qualified Data.Text           as T (Text, intercalate, pack, words)
import           Lucid
import           Types

template :: Site [Video] -> Html ()
template (Site s) = do
  let years = fmap (_1 %~ getDown) $ S.toList (fmap (S.keys) s)
  doctype_
  html_ $ do
    head_ $ do
      title_ "Stuff"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [ rel_ "stylesheet" , href_ "https://unpkg.com/purecss@0.6.2/build/pure-min.css" ]
      link_ [ rel_ "stylesheet", href_ "https://unpkg.com/purecss@0.6.2/build/grids-responsive-min.css"]
      link_ [ rel_ "stylesheet", href_ "https://s3-us-west-2.amazonaws.com/colors-css/2.2.0/colors.min.css"]
      link_ [ rel_ "stylesheet", href_ "site.css"]
    body_ [class_ ""] $ do
      div_ [id_ "layout", class_ ""] $ do
        div_ [class_ "sidebar bg-navy"] $
          div_ [class_ "centered"] $ do
            h1_ [class_ "brand-title white p1"] "Pick a year"
            -- h2_ [class_ "brand-tagline"] "some stuff goes here"
            div_ [class_ "pure-menu"] $
                  F.for_ years $ \(Year y, cs) -> do
                    ul_ [class_ "pure-menu-list"] $
                        li_ [class_ "pure-menu-item"] $ do
                          a_ [class_ "pure-menu-link white bold hover-blue embiggen", href_ ("#"<> (T.pack (show y))) ] (toHtml (show y))
                          F.for_ cs $ \(Comp c) ->
                            a_ [class_ "pure-menu-link aqua hover-blue", href_ ("#"<> anchor y c ) ] (toHtml c)

      div_ [class_ "content pure-g bg-navy white"] $
        S.foldMapWithKey renderYear s

getDown (Down a) = a

renderYear :: Down Year -> _ -> Html ()
renderYear (Down (Year y)) inner =
  do
      div_ [class_ "pure-u-1"] $ do
        a_ [name_ (T.pack (show y))] mempty
        (div_ [class_ "pure-u-1-3 align-right bg-red white"] (h1_ $ toHtml (show y)) <>
           div_ [class_ "pure-u-2-3 bg-red"] (h1_ x)) <>
          div_ (S.foldMapWithKey (renderComp (Year y)) inner)


x = span_ (toHtmlRaw ("&nbsp;"::String))

renderComp :: Year -> Comp -> _ -> Html ()
renderComp (Year y) (Comp c) inner =
  do
    a_ [name_ (anchor y c)] mempty
    div_ [class_ "pure-u-1 pure-u-md-1-3 align-right upper"] (h2_ (toHtml c))
    div_ [class_ "pure-u-1 pure-u-md-2-3"] (div_ [class_ "pl-2"] $ S.foldMapWithKey renderBand inner)

anchor y c =
  T.pack (show y) <> "-" <> T.intercalate "-" (T.words c)

renderBand :: Band -> _ -> Html ()
renderBand (Band b) inner =
  h3_ [class_ "border-bottom border--red"] (toHtml b) <> div_ (S.foldMapWithKey renderCorp inner)

renderCorp :: Corp -> _ -> Html ()
renderCorp c inner = div_ (S.foldMapWithKey (renderSet c) inner)
 where prefix = case c of
                  FullBand -> mempty
                  Pipe     -> h5_ "Pipe Corps"
                  Drum     -> h5_ "Drum Corps"

renderSet :: Corp -> Set -> [Video] -> Html ()
renderSet c s vids =
  --h5_ (toHtml $ show s) <>
  ul_ (foldMap (renderVid s c) vids)

renderVid :: Set -> Corp -> Video -> Html ()
renderVid s c vid = li_ [] $ prefix <> (a_ [href_ (videoUrl vid)] (toHtml $ _videoTitle vid)) <> " [" <> showSource (_videoSource vid) <>  "]"
  where mk :: [T.Text] -> Html ()
        mk xs = toHtml (T.intercalate ", " xs)
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
