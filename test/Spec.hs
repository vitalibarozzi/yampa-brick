{-# LANGUAGE Arrows #-}
import FRP.Yampa.Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick
import FRP.Yampa (returnA, (<<<),(>>>),arr)
import qualified FRP.Yampa as Yampa
import qualified Brick
import qualified Data.Text as Text
import Control.Concurrent
import Control.Monad


main :: IO ()
main = do
    handle  <- reactInitBrick sf (const (pure ())) -- print
    forever $ do
        threadDelay    100000
        Yampa.react handle (0.1, Just (Just ()))
        Yampa.react handle (0.1, Just (Just ()))
        Yampa.react handle (0.1, Just (Just ()))
  where
    sf = proc foo -> do
        t <- Yampa.time -< foo
        f <- progressBar (Just "...") <<< arr (sin . realToFrac) -< t 
        g <- arr (Brick.txt . Text.pack . show) -< t
        returnA -< f <=> g
        --Yampa.time 
        --    >>> 
        --    >>> progressBar (Just "hey wtf")
        --    >>> arr hCenter 
        --    >>> arr border
        --Yampa.constant 0.8 >>> progressBar (Just "hey wtf")
