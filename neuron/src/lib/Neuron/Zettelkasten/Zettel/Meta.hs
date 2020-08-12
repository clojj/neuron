{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
    formatZettelLocalTime,
    formatZettelDay,
    parseZettelDate,
  )
where

import Data.TagTree (Tag)
import Data.Time
import Data.YAML
import Relude

-- | YAML metadata in a zettel markdown file
data Meta = Meta
  { title :: Maybe Text,
    tags :: Maybe [Tag],
    -- | Creation day
    created :: Maybe LocalTime,
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
        <*> (m .:? "created" <|> m .:? "date")
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

instance FromYAML LocalTime where
  parseYAML =
    parseZettelDate <=< parseYAML @Text

instance ToYAML LocalTime where
  toYAML =
    toYAML . formatZettelLocalTime

data ZettelDateFormat
  = ZettelDateFormat_Day
  | ZettelDateFormat_DateTime

zettelDateFormat :: ZettelDateFormat -> String
zettelDateFormat = \case
  ZettelDateFormat_Day -> "%Y-%m-%d"
  ZettelDateFormat_DateTime -> "%Y-%m-%dT%H:%M" -- the default format in which we decode and encode zettel creation-datetime

-- | All formats of user-edited tags 'created', that can be parsed.
zettelDateFormats :: NonEmpty ZettelDateFormat
zettelDateFormats = ZettelDateFormat_DateTime :| [ZettelDateFormat_Day]

formatZettelLocalTime :: LocalTime -> Text
formatZettelLocalTime =
  toText . formatTime defaultTimeLocale (zettelDateFormat ZettelDateFormat_DateTime)

formatZettelDay :: Day -> Text
formatZettelDay =
  toText . formatTime defaultTimeLocale (zettelDateFormat ZettelDateFormat_Day)

parseZettelDate :: (MonadFail m, Alternative m) => Text -> m LocalTime
parseZettelDate t =
  asum $ (flip parseDateM $ toString t) . zettelDateFormat <$> zettelDateFormats

parseDateM :: MonadFail m => String -> String -> m LocalTime
parseDateM fmt = parseTimeM False defaultTimeLocale fmt