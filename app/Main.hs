{-# LANGUAGE ImplicitParams #-}

module Main where

import Control.Monad (void)
import Data.GI.Base
import GI.Adw qualified as Adw
import GI.Adw.Objects.ExpanderRow
import GI.Gtk qualified as Gtk
import System.Environment (getArgs, getProgName)

import Data.Foldable
import UI.Compiler

tools :: Adw.ToastOverlay -> IO Gtk.ListBox
tools toastOverlay = do
    toolsList <- new Gtk.ListBox [#showSeparators := True]

    ghcRow <-
        new
            ExpanderRow
            [ #title := "Glasgow Haskell Compiler"
            , #subtitle := "GHC"
            ]

    cabalRow <-
        new
            ExpanderRow
            [ #title :=> pure "Cabal"
            , #subtitle :=> pure "Haskell project manager"
            ]

    hlsRow <-
        new
            ExpanderRow
            [ #title :=> pure "Haskell Language Server"
            , #subtitle :=> pure "LSP Server for IDEs"
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
    content <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    title <-
        new
            Adw.WindowTitle
            [ #title := "Haskell Toolchain Installer"
            , #subtitle := "ghcup"
            ]
    titlebar <- new Adw.HeaderBar [#titleWidget := title]
    content.append titlebar

    toastOverlay <- new Adw.ToastOverlay []

    toolsContainer <- tools toastOverlay
    content.append toolsContainer
    content.append toastOverlay

    window <-
        new
            Adw.ApplicationWindow
            [ #application := app
            , #content := content
            , #defaultWidth := 400
            ]
    window.present

main :: IO ()
main = do
    app <-
        new
            Adw.Application
            [ #applicationId := "haskell-gi.Adw.test"
            , On #activate (activate ?self)
            ]

    args <- getArgs
    progName <- getProgName
    void (app.run $ Just $ progName : args)
