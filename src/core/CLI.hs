-- | Command-line interface of the application.
module CLI
  ( Options (..)
  , getOptions
  ) where

import Options.Applicative

import Config (ViewMode (..))

data Options = Options
  { forcedView :: Maybe ViewMode
  , gtkArgs :: [String]
  }
  deriving stock (Eq, Ord, Show)

getOptions :: IO Options
getOptions = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "A GTK4 frontend for the ghcup toolchain manager"
        <> forwardOptions
    )

parser :: Parser Options
parser =
  Options
    <$> optional
      ( option
          viewMode
          ( long "view"
              <> metavar "VIEW"
              <> help "Force the view: \"list\" or \"table\""
          )
      )
    <*> many (strArgument (metavar "GTK_ARGS"))

viewMode :: ReadM ViewMode
viewMode = eitherReader $ \case
  "list" -> Right Simple
  "table" -> Right Advanced
  other -> Left ("invalid view " <> show other <> ", expected \"list\" or \"table\"")
