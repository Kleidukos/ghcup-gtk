-- | Field parsers shared by the GHC and HLS compile forms. The models
-- themselves live in "Presentation.CompileForm.Ghc" and
-- "Presentation.CompileForm.Hls": each keeps its record fields alone in
-- scope, so record updates stay unambiguous.
module Presentation.CompileForm
  ( nonEmpty
  , whenEmpty
  , versionOrPath
  , parsedJobs
  , parsedPatches
  , parsedOverwrite
  ) where

import Data.Bifunctor (bimap, first, second)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Versions (Version)
import GHCup.Input.Parsers (absolutePathParser, overWriteVersionParser, toolVersionEither, uriParser)
import GHCup.Types (VersionPattern)
import System.FilePath (isPathSeparator)
import Text.Read (readMaybe)
import URI.ByteString (URI)

nonEmpty :: Text -> Maybe Text
nonEmpty text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

whenEmpty :: a -> (Text -> Either Text a) -> Text -> Either Text a
whenEmpty emptyValue f text = maybe (Right emptyValue) f (nonEmpty text)

versionOrPath :: Text -> Either Text (Either Version FilePath)
versionOrPath input
  | Text.any isPathSeparator input =
      bimap Text.pack Right (absolutePathParser (Text.unpack input))
  | otherwise =
      bimap Text.pack Left (toolVersionEither (Text.unpack input))

parsedJobs :: Text -> Either Text (Maybe Int)
parsedJobs = whenEmpty Nothing $ \input ->
  case readMaybe (Text.unpack input) of
    Just n | n > 0 -> Right (Just n)
    _ -> Left "Jobs must be a positive integer"

parsedPatches :: Text -> Either Text (Maybe (Either FilePath [URI]))
parsedPatches = whenEmpty Nothing $ \input ->
  let asDir = second (Just . Left) (absolutePathParser (Text.unpack input))
      asUris =
        second (Just . Right) $
          traverse (uriParser . Text.unpack) (Text.words input)
  in first Text.pack (asDir `orElse` asUris)

parsedOverwrite :: Text -> Either Text (Maybe [VersionPattern])
parsedOverwrite = whenEmpty Nothing $ \input ->
  Text.unpack input & overWriteVersionParser & bimap Text.pack Just

orElse :: Either e a -> Either e a -> Either e a
orElse (Right a) _ = Right a
orElse (Left _) fallback = fallback
