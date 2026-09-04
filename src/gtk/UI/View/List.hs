module UI.View.List (build) where

import Control.Monad (forM, forM_)
import Data.GI.Base
import Data.Vector qualified as Vector
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk

import Presentation.Row (RowSpec (..), ToolRows (..))
import UI.View (View (..))
import UI.View.List.Row (RowHandle (..))
import UI.View.List.Row qualified as Row

-- | The simple renderer: a boxed list of 'Adw.ActionRow's in a clamp. The
-- clamp lives here rather than in 'UI.ToolPanes' because the advanced table
-- wants the full pane width.
build :: Adw.ApplicationWindow -> IO View
build window = do
  listBox <- new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone]
  listBox.addCssClass "boxed-list"
  clamp <-
    new
      Adw.Clamp
      [ #child := listBox
      , #maximumSize := 600
      , #tighteningThreshold := 400
      , #marginTop := 24
      , #marginBottom := 24
      , #marginStart := 12
      , #marginEnd := 12
      ]
  scrolled <-
    new
      Gtk.ScrolledWindow
      [ #child := clamp
      , #vexpand := True
      , #hscrollbarPolicy := Gtk.PolicyTypeNever
      ]
  widget <- Gtk.toWidget scrolled

  let setRows callbacks toolRows = do
        listBox.removeAll
        handles <- forM toolRows.rows $ \spec -> do
          handle <- Row.build window spec callbacks
          listBox.append handle.row
          pure (spec.key, handle)
        -- One radio group per pane, anchored on the first installed row.
        case Vector.uncons (Vector.mapMaybe ((.defaultCheck) . snd) handles) of
          Just (anchor, rest) -> forM_ rest $ \check -> check.setGroup (Just anchor)
          Nothing -> pure ()

      setSensitive b = set listBox [#sensitive := b]

  pure View {widget, setRows, setSensitive}
