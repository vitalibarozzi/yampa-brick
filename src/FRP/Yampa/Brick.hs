module FRP.Yampa.Brick
    (reactInitBrick, BrickWidget, VTYEvent)
where


import qualified FRP.Yampa as Yampa
import qualified Brick
import FRP.Yampa (SF, ReactHandle)
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Brick.BChan as Brick
import Graphics.Vty hiding (Event)
import qualified Graphics.Vty as VTY


-----------------------------------------------------------
type BrickWidget = Brick.Widget ()


-----------------------------------------------------------
type VTYEvent = VTY.Event


-----------------------------------------------------------
-- | Starts the handle for the SF and exposes it together
-- with a chan where the terminal events can be received.
reactInitBrick ::

    (MonadIO m) =>

    -- | Some initial state.
    s ->

    -- | How to render stuff to the terminal over time.
    SF s BrickWidget ->

    -- | Called when we have an event.
    (VTYEvent -> IO ()) ->

    -- | Renders stuff given a state s and a dt, with function `Yampa.react`.
    m (ReactHandle s BrickWidget)

{-# INLINABLE reactInitBrick #-}

reactInitBrick s sf callback = do

    liftIO $ do
        brickChan <- newBChan 10
        ()        <- initBrick brickChan
        Yampa.reactInit 
            (pure s) 
            (actuate brickChan) 
            sf

  where

    initBrick brickChan = do
        makeVty <- mkVty <$> standardIOConfig
        vty     <- makeVty
        void -- TODO could be done with immortal instead?
            $ forkIO 
            $ void 
            $ Brick.customMain 
                vty 
                makeVty 
                (Just brickChan) 
                (Brick.simpleApp Brick.emptyWidget) {
                    Brick.appDraw        = return, -- we use the state for widgets
                    Brick.appHandleEvent = \e -> 
                        case e of
                            Brick.AppEvent wid -> Brick.put wid -- triggers Brick.appDraw
                            Brick.VtyEvent vte -> liftIO (callback vte)
                            __________________ -> pure ()
                }
                Brick.emptyWidget

    actuate brickChan _ updated widget = do
        -- sends the widget to be rendered by the brick thread
        when updated $ do 
            threadDelay 1
            Brick.writeBChan brickChan widget 
        pure updated

