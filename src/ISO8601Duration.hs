module ISO8601Duration where

import           Control.Applicative
import           Data.Monoid
import           Data.Text                    (Text, pack, unpack)
import           Text.ParserCombinators.ReadP

data Duration =
  Duration (Maybe Hour) Minute Second
  deriving (Eq, Show, Read)

newtype Hour = Hour Int
 deriving (Eq, Show, Read)
newtype Minute = Minute Int
 deriving (Eq, Show, Read)
newtype Second = Second Int
 deriving (Eq, Show, Read)

durationString :: Text -> Text
durationString d =
  pack .
  showDuration $
  readDuration (unpack d)

fromJust :: Maybe a -> a
fromJust (Just v) = v
fromJust Nothing  = error "fromJust"

_ptms :: ReadP Duration
_ptms = do
  _ <- string "PT"
  m <- (Minute <$> readS_to_P reads) <* string "M"
  s <- Second <$> readS_to_P reads <* string "S"
  pure (Duration Nothing m s)

_pts :: ReadP Duration
_pts = do
  _ <- string "PT"
  s <- Second <$> readS_to_P reads <* string "S"
  pure (Duration Nothing (Minute 0) s)

_ptm :: ReadP Duration
_ptm = do
  _ <- string "PT"
  m <- Minute <$> readS_to_P reads <* string "M"
  pure (Duration Nothing m (Second 0))

readDuration :: String -> Duration
readDuration t =
  case readP_to_S (_ptms <|> _pts <|> _ptm) t of
    (x, _):_ -> x
    _        -> error t

-- TODO: NICK THIS IS TOTES WRONG MATE
showDuration :: Duration -> String
showDuration (Duration _ (Minute m) (Second s))
 = ri m <> ":" <> ri s
 where ri n | n < 10 = "0" <> show n
            | otherwise = show n

-- PT#M#S
-- PT#H#M#S
-- P#DT#H#M#S
