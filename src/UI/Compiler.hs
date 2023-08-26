module UI.Compiler where

import Control.Monad (when)
import Data.GI.Base
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Versions (prettyVer)
import GHCup
import GHCup.Types
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import UI.Install

getVersionsFor :: Tool -> [ListResult] -> [ListResult]
getVersionsFor t = filter $ do
  correctTool <- (== t) . lTool
  notOld <- (Old `notElem`) . lTag
  pure $ correctTool && notOld

getGHCVersions :: [ListResult] -> Adw.ToastOverlay -> Adw.ApplicationWindow -> AppState -> IO [Adw.ActionRow]
getGHCVersions toolVersions toastOverlay app appState =
  traverse (toActionRow toastOverlay app GHC appState) $
    getVersionsFor GHC toolVersions

getCabalVersions :: [ListResult] -> Adw.ToastOverlay -> Adw.ApplicationWindow -> AppState -> IO [Adw.ActionRow]
getCabalVersions toolVersions toastOverlay app appState =
  traverse (toActionRow toastOverlay app Cabal appState) $
    getVersionsFor Cabal toolVersions

getHLSVersions :: [ListResult] -> Adw.ToastOverlay -> Adw.ApplicationWindow -> AppState -> IO [Adw.ActionRow]
getHLSVersions toolVersions toastOverlay app appState =
  traverse (toActionRow toastOverlay app HLS appState) $
    getVersionsFor HLS toolVersions

toActionRow
  :: Adw.ToastOverlay
  -> Adw.ApplicationWindow
  -> Tool
  -> AppState
  -> ListResult
  -> IO Adw.ActionRow
toActionRow toastOverlay app tool appState ListResult{..} = do
  let versionLabel = prettyVer lVer
      subtitle =
        Text.intercalate ", " $
          catMaybes
            [ toMaybe "<span color='red'>latest</span>" $ Latest `elem` lTag
            , toMaybe "<span color='green'>recommended</span>" $ Recommended `elem` lTag
            , toMaybe "<span color='green'>HLS-powered</span>" hlsPowered
            ]

  installButton <-
    new
      Gtk.Switch
      [ #valign := Gtk.AlignCenter
      , #active := lInstalled
      , #state := lInstalled
      ]
  on installButton #stateSet $ \state -> do
    let ds = _ghcupDownloads $ ghcupInfo appState
    install installButton toastOverlay app appState ds state tool lVer

  setIcon <-
    new
      Gtk.Image
      [ #iconName := "object-select-symbolic"
      , #iconSize := Gtk.IconSizeLarge
      ]

  actionRow <-
    new
      Adw.ActionRow
      [ #title := versionLabel
      , #subtitle := subtitle
      ]

  -- setToggle <- new Gtk.CheckButton
  --   [ #label := "Set as default"
  --   ]

  when lSet $
    Adw.actionRowAddSuffix actionRow setIcon
  Adw.actionRowAddSuffix actionRow installButton
  -- Adw.actionRowAddSuffix actionRow setToggle
  pure actionRow
  where
    toMaybe _ False = Nothing
    toMaybe a True = Just a
