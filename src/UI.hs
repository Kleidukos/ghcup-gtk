{-# LANGUAGE ImplicitParams #-}

module UI where

import Control.Monad.Reader
import Data.Foldable
import Data.Functor
import Data.GI.Base
import Data.Maybe
import GHCup
import GI.Adw qualified as Adw
import GI.Adw.Objects.ExpanderRow
import GI.Gtk qualified as Gtk
import System.Environment (getArgs, getProgName)
import System.Exit

import Driver.GHCup
import UI.Compiler
import UI.HeaderBar

main :: IO ()
main = do
  app <-
    new
      Adw.Application
      [ #applicationId := "ghcup.gui"
      , On #activate (activate ?self)
      ]

  args <- getArgs
  progName <- getProgName
  void (app.run $ Just $ progName : args)

tools
  :: AppState
  -> Adw.ToastOverlay
  -> Adw.ApplicationWindow
  -> IO Gtk.Widget
tools appState toastOverlay app = do
  toolsList <- new Gtk.ListBox [#showSeparators := True]

  ghcRow <-
    new
      ExpanderRow
      [ #title := "Glasgow Haskell Compiler"
      , #expanded := True
      ]

  cabalRow <-
    new
      ExpanderRow
      [ #title :=> pure "Cabal project manager"
      , #expanded := True
      ]

  hlsRow <-
    new
      ExpanderRow
      [ #title :=> pure "Haskell Language Server"
      , #expanded := True
      ]

  toolVersions <- runReaderT (listVersions Nothing Nothing) appState

  compilers <- getGHCVersions toolVersions toastOverlay app appState
  traverse_ (expanderRowAddRow ghcRow) compilers

  cabalVersions <- getCabalVersions toolVersions toastOverlay app appState
  traverse_ (expanderRowAddRow cabalRow) cabalVersions

  hlsVersions <- getHLSVersions toolVersions toastOverlay app appState
  traverse_ (expanderRowAddRow hlsRow) hlsVersions

  toolsList.append ghcRow
  toolsList.append cabalRow
  toolsList.append hlsRow

  toolsListContainer <-
    new
      Gtk.ScrolledWindow
      [ #halign := Gtk.AlignFill
      , #valign := Gtk.AlignFill
      , #hexpand := True
      , #vexpand := True
      ]
  toolsListContainer.setChild (Just toolsList)

  Gtk.toWidget toolsListContainer

activate :: Adw.Application -> IO ()
activate app = do
  window <-
    new
      Adw.ApplicationWindow
      [ #application := app
      , #defaultWidth := 400
      , #title := "Haskell Toolchain Installer"
      ]

  content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  titlebar <- let ?self = app in genHeaderbar
  content.append titlebar

  toastOverlay <- new Adw.ToastOverlay []

  initAppState >>= \case
    Left e -> exitDialogWithError window e
    Right appState -> do
      toolsContainer <- tools appState toastOverlay window
      content.append toolsContainer
      content.append toastOverlay

  set window [#content := content]

  -- menuUI <- Text.readFile "./ui/menu.ui"
  -- builder <- Gtk.builderNewFromString menuUI (-1)
  -- menu <- getCastedObjectFromBuilder builder "menu" Gio.MenuModel

  window.present
  where
    exitDialogWithError window e = do
      messageDialog <-
        Adw.messageDialogNew
          (Just window)
          (Just "Error")
          (Just e)
      messageDialog.addResponse "ok" "OK"
      messageDialog.setModal True

      on messageDialog #response $ \_ ->
        exitWith (ExitFailure 1)
      messageDialog.present
      pure ()

castWOMaybe :: forall o o'. (GObject o, GObject o') => (ManagedPtr o' -> o') -> o -> IO o'
castWOMaybe typeToCast obj = castTo typeToCast obj <&> fromJust

-- getCastedObjectFromBuilder :: forall a. GObject a => Gtk.Builder -> Text -> (ManagedPtr a -> a) -> IO a
-- getCastedObjectFromBuilder builder name typeToCast = _-- #getObject builder name >>= castWOMaybe typeToCast . fromJust
