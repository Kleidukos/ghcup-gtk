module Effects.Notify
  ( Notify (..)
  , emit
  , runNotifyIO
  ) where

import Effectful
import Effectful.Dispatch.Dynamic

import Toolchain.Types (UiMsg)

-- | Emission of messages towards the UI thread.
data Notify :: Effect where
  Emit :: UiMsg -> Notify m ()

type instance DispatchOf Notify = Dynamic

emit :: (Notify :> es) => UiMsg -> Eff es ()
emit = send . Emit

runNotifyIO :: (IOE :> es) => (UiMsg -> IO ()) -> Eff (Notify : es) a -> Eff es a
runNotifyIO notify = interpret $ \_ -> \case
  Emit msg -> liftIO (notify msg)
