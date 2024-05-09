{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
module FRP.Yampa.Brick where


import qualified FRP.Yampa as Yampa
import qualified Brick
import FRP.Yampa ((^>>), DTime, SF, Event(..),ReactHandle)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Brick.BChan as Brick
import Graphics.Vty hiding (Event)
import qualified Graphics.Vty as VTY


-----------------------------------------------------------
reactInitBrick ::
    forall n s.
    Ord n =>
    s ->
    SF s (Brick.Widget n) ->
    IO (Chan VTY.Event, ReactHandle s (Brick.Widget n))
reactInitBrick s sf = do
    brickChan <- newBChan 100
    eventChan <- newChan
    () <- do
        makeVty <- mkVty <$> standardIOConfig
        vty     <- makeVty
        void
            $ forkIO 
            $ void 
            $ Brick.customMain 
                @n
                vty 
                makeVty 
                (Just brickChan) 
                (Brick.simpleApp Brick.emptyWidget) { 
                    Brick.appDraw        = return,
                    Brick.appHandleEvent = \case
                        Brick.AppEvent wid -> Brick.put wid -- triggers Brick.appDraw
                        Brick.VtyEvent e   -> liftIO (writeChan eventChan e)
                        __________________ -> pure ()
                }
                Brick.emptyWidget
    handle <- Yampa.reactInit (pure s) (actuate brickChan) sf
    return (eventChan, handle)
  where
    actuate brickChan handle updated widget = do
        -- sends the widget to be rendered by the brick thread
        when updated do Brick.writeBChan brickChan widget 
        -- keeps the yampa handler alive afterwards
        pure False

