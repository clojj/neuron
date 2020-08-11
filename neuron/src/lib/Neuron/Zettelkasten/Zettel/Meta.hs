{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Zettel.Meta
  ( Meta (..),
    formatZettelLocalTime,
    formatZettelDate,
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
        <*> (m .:? "created")
          .!= ( case parseEither (m .:? "date") of
                  Left _ -> Nothing
                  Right v -> v
              )
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

-- | The default format in which we decode and encode zettel dates.
defaultZettelDateFormat :: String
defaultZettelDateFormat = "%Y-%m-%dT%H:%M"

-- | All formats of user-edited tags 'created', that can be parsed.
zettelDateFormats :: [String]
zettelDateFormats = ["%Y-%m-%d", defaultZettelDateFormat]

formatZettelLocalTime :: LocalTime -> Text
formatZettelLocalTime =
  toText . formatTime defaultTimeLocale defaultZettelDateFormat

formatZettelDate :: Day -> Text
formatZettelDate =
  toText . formatTime defaultTimeLocale "%Y-%m-%d"

parseZettelDate :: (MonadFail m, Alternative m) => Text -> m LocalTime
parseZettelDate t =
  let s = toString t
      pars = parseDateM s
   in asum $ pars <$> zettelDateFormats

parseDateM :: MonadFail m => String -> String -> m LocalTime
parseDateM s = \fmt -> parseTimeM False defaultTimeLocale fmt s