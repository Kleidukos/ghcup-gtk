module UI.Compiler where

import Data.GI.Base
import Data.Text (Text)
import Data.Versions (prettyVer)
import GHCup
import GHCup.Types
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import UI.Install

getVersionsFor :: Tool -> [ListResult] -> [ListResult]
getVersionsFor t = filter ((== t) . lTool)

getGHCVersions :: [ListResult] -> Adw.ToastOverlay -> Adw.ApplicationWindow -> IO [Adw.ActionRow]
getGHCVersions toolVersions toastOverlay app = traverse (toActionRow toastOverlay app "GHC") compilerList
  where
    compilerList = getVersionsFor GHC toolVersions

getCabalVersions :: [ListResult] -> Adw.ToastOverlay -> Adw.ApplicationWindow -> IO [Adw.ActionRow]
getCabalVersions toolVersions toastOverlay app = traverse (toActionRow toastOverlay app "Cabal") cabalVersions
  where
    cabalVersions = getVersionsFor Cabal toolVersions

getHLSVersions :: [ListResult] -> Adw.ToastOverlay -> Adw.ApplicationWindow -> IO [Adw.ActionRow]
getHLSVersions toolVersions toastOverlay app = traverse (toActionRow toastOverlay app "HLS") hlsVersions
  where
    hlsVersions = getVersionsFor HLS toolVersions

toActionRow :: Adw.ToastOverlay -> Adw.ApplicationWindow -> Text -> ListResult -> IO Adw.ActionRow
toActionRow toastOverlay app toolLabel ListResult{..} = do
  let versionLabel =
        mconcat
          [ prettyVer lVer
          , if lSet then " (set)" else ""
          , if hlsPowered then " (HLS-powered)" else ""
          , if Latest `elem` lTag then " (latest)" else ""
          , if Recommended `elem` lTag then " (recommended)" else ""
          ]

  installButton <-
    new
      Gtk.Switch
      [ #valign := Gtk.AlignCenter
      , #active := lInstalled
      , #state := lInstalled
      ]
  on installButton #stateSet $ \state -> do
    mockInstall installButton toastOverlay app state (toolLabel <> " " <> versionLabel)

  actionRow <-
    new
      Adw.ActionRow
      [ #title := versionLabel
      ]

  -- setToggle <- new Gtk.CheckButton
  --   [ #label := "Set as default"
  --   ]

  Adw.actionRowAddSuffix actionRow installButton
  -- Adw.actionRowAddSuffix actionRow setToggle
  pure actionRow
