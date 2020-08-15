{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
    DateMayTime,
    formatZettelLocalTime,
    -- formatZettelDay,
    parseZettelDate,
    parseZettelLocalTime,
  )
where

import Data.TagTree (Tag)
import Data.Time
import Data.YAML
import Relude

type DateMayTime = Either Day LocalTime

-- | YAML metadata in a zettel markdown file
data Meta = Meta
  { title :: Maybe Text,
    tags :: Maybe [Tag],
    -- | Creation day
    date :: Maybe DateMayTime,
    -- | List in the z-index
    unlisted :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance FromYAML Meta where
  parseYAML =
    withMap "Meta" $ \m ->
      Meta
        <$> m .:? "title"
        -- "keywords" is an alias for "tags"
        <*> (liftA2 (<|>) (m .:? "tags") (m .:? "keywords"))
        <*> m .:? "date"
        <*> m .:? "unlisted"

-- NOTE: Not using this instance because it generates "tags: null" when tags is
-- Nothing.
-- instance ToYAML Meta where
--   toYAML Meta {..} =
--     mapping
--       [ "title" .= title,
--         "tags" .= tags,
--         "created" .= date
--       ]

instance FromYAML DateMayTime where
  parseYAML =
    parseZettelDate <=< parseYAML @Text

instance ToYAML (Either Day LocalTime) where
  toYAML =
    toYAML . formatZettelLocalTime

data ZettelDateFormat
  = ZettelDateFormat_Day
  | ZettelDateFormat_DateTime

zettelDateFormat :: ZettelDateFormat -> String
zettelDateFormat = \case
  ZettelDateFormat_Day -> "%Y-%m-%d"
  ZettelDateFormat_DateTime -> "%Y-%m-%dT%H:%M" -- the default format in which we decode and encode zettel creation-datetime

formatZettelLocalTime :: DateMayTime -> Text
formatZettelLocalTime =
  toText . format
  where
    format v =
      case v of
        Left day -> formatTime defaultTimeLocale (zettelDateFormat ZettelDateFormat_Day) day
        Right localtime -> formatTime defaultTimeLocale (zettelDateFormat ZettelDateFormat_DateTime) localtime

-- formatZettelDay :: Day -> Text
-- formatZettelDay =
--   toText . formatTime defaultTimeLocale (zettelDateFormat ZettelDateFormat_Day)

parseZettelLocalTime :: MonadFail m => Text -> m LocalTime
parseZettelLocalTime =
  parseTimeM False defaultTimeLocale (zettelDateFormat ZettelDateFormat_DateTime) . toString

parseZettelDate :: (MonadFail m, Alternative m) => Text -> m DateMayTime
parseZettelDate t =
  parseDateM $ toString t

parseDateM :: MonadFail m => String -> m DateMayTime
parseDateM s =
  let day = parseTimeM False defaultTimeLocale (zettelDateFormat ZettelDateFormat_Day) s
      localtime = parseTimeM False defaultTimeLocale (zettelDateFormat ZettelDateFormat_DateTime) s
   in case day of
        Just d -> return $ Left d
        _ -> case localtime of
          Just lt -> return $ Right lt
          _ -> fail "error parsing"