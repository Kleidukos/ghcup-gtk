-- | Command-line interface of the application.
module CLI
  ( Options (..)
  , getOptions
  ) where

import Options.Applicative

newtype Options = Options
  { gtkArgs :: [String]
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
    <$> many (strArgument (metavar "GTK_ARGS"))
