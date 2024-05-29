{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module FRP.Yampa.Brick (
    reactInitBrick,
    BrickWidget,
    VTYEvent,
)
where

import qualified Brick
import Brick.BChan as Brick
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import FRP.Yampa
import qualified FRP.Yampa as Yampa
import Graphics.Vty hiding (Event)
import qualified Graphics.Vty as VTY

-----------------------------------------------------------
type BrickWidget = Brick.Widget ()

-----------------------------------------------------------
type VTYEvent = VTY.Event

-----------------------------------------------------------

{- | Starts the handle for the SF and exposes it together
with a chan where the terminal events can be received.
-}
reactInitBrick ::
    (MonadIO m) =>
    -- | How to render stuff to the terminal over time.
    SF s BrickWidget ->
    -- | Called when we have an event.
    (VTYEvent -> IO ()) ->
    -- | Renders stuff given a state s and a dt, with function `Yampa.react`.
    m (ReactHandle (Maybe s) BrickWidget)
{-# INLINEABLE reactInitBrick #-}
reactInitBrick sf callback = do
    liftIO $ do
        brickChan <- newBChan 10
        () <- initBrick brickChan
        Yampa.reactInit
            (pure Nothing)
            (actuate brickChan)
            ( proc ms -> do
                case ms of
                    Nothing -> Yampa.returnA -< Brick.txt "Waiting data..."
                    Just s -> sf -< s
            )
  where
    initBrick brickChan = do
        makeVty <- mkVty <$> standardIOConfig
        vty <- makeVty
        void
            $ forkIO
            $ void
            $ Brick.customMain
                vty
                makeVty
                (Just brickChan)
                (Brick.simpleApp Brick.emptyWidget)
                    { Brick.appDraw = return -- we use the state for widgets
                    , Brick.appHandleEvent = \e ->
                        case e of
                            Brick.AppEvent wid -> Brick.put wid -- triggers Brick.appDraw
                            Brick.VtyEvent vte -> liftIO (callback vte)
                            __________________ -> pure ()
                    }
                Brick.emptyWidget

    actuate brickChan _ updated widget = do
        -- sends the widget to be rendered by the brick thread
        when updated (Brick.writeBChan brickChan widget)
        pure updated
