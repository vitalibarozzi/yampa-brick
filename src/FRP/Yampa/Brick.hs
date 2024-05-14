module FRP.Yampa.Brick
    (reactInitBrick)
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
-- | Starts the handle for the SF and exposes it together
-- with a chan where the terminal events can be received.
reactInitBrick ::
    s ->
    SF s (Brick.Widget ()) ->
    ReactHandle (Yampa.Event VTY.Event) a ->
    IO (ReactHandle s (Brick.Widget ()))
{-# INLINABLE reactInitBrick #-}
reactInitBrick s sf callbackHandle = do
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
                            Brick.VtyEvent vte -> void (liftIO (Yampa.react callbackHandle (0, Just (Yampa.Event vte))))
                            __________________ -> pure ()
                }
                Brick.emptyWidget
    actuate brickChan _ updated widget = do
        -- sends the widget to be rendered by the brick thread
        when updated $ do 
            threadDelay 1
            Brick.writeBChan brickChan widget 
        pure False

