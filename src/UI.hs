{-# LANGUAGE ImplicitParams #-}

module UI where

import Data.Foldable
import Data.Functor
import Data.GI.Base
import Data.Maybe
import Data.Text (Text)
import Data.Text.IO qualified as Text
import GI.Adw qualified as Adw
import GI.Adw.Objects.ExpanderRow
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Environment (getArgs, getProgName)

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

tools :: Adw.ToastOverlay -> IO Gtk.ListBox
tools toastOverlay = do
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

    compilers <- getGHCVersions toastOverlay
    traverse_ (\compiler -> expanderRowAddRow ghcRow compiler) compilers

    cabalVersions <- getCabalVersions toastOverlay
    traverse_ (\cabal -> expanderRowAddRow cabalRow cabal) cabalVersions

    hlsVersions <- getHLSVersions toastOverlay
    traverse_ (\hls -> expanderRowAddRow hlsRow hls) hlsVersions

    toolsList.append ghcRow
    toolsList.append cabalRow
    toolsList.append hlsRow

    pure toolsList

activate :: Adw.Application -> IO ()
activate app = do
    let ?self = app
    content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    titlebar <- genHeaderbar
    content.append titlebar

    toastOverlay <- new Adw.ToastOverlay []

    toolsContainer <- tools toastOverlay
    content.append toolsContainer
    content.append toastOverlay

    -- menuUI <- Text.readFile "./ui/menu.ui"
    -- builder <- Gtk.builderNewFromString menuUI (-1)
    -- menu <- getCastedObjectFromBuilder builder "menu" Gio.MenuModel

    window <-
        new
            Adw.ApplicationWindow
            [ #application := app
            , #content := content
            , #defaultWidth := 400
            , #title := "Haskell Toolchain Installer"
            ]

    window.present
castWOMaybe :: forall o o'. (GObject o, GObject o') => (ManagedPtr o' -> o') -> o -> IO o'
castWOMaybe typeToCast obj = castTo typeToCast obj <&> fromJust

-- getCastedObjectFromBuilder :: forall a. GObject a => Gtk.Builder -> Text -> (ManagedPtr a -> a) -> IO a
-- getCastedObjectFromBuilder builder name typeToCast = _-- #getObject builder name >>= castWOMaybe typeToCast . fromJust
