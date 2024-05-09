{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
import FRP.Yampa.Brick
import qualified FRP.Yampa as Yampa
import FRP.Yampa (Event(..))
import qualified Brick
import qualified Data.Text as Text
import Data.IORef
import System.IO.Unsafe
import System.IO
import System.Exit
import System.Timeout
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Graphics.Vty.Input.Events


main :: IO ()
main = do
    (chan,handle) <- reactInitBrick @() () do
        proc s -> do
            t <- Yampa.time -< s
            let f = Brick.txt 
                        $  Text.pack 
                        $ "State: " 
                        <> show s 
                        <> "\n Time  : " 
                        <> show t
            Yampa.returnA -< f
    forever do
        threadDelay    100000
        mev <- timeout 1000 $ readChan chan
        case mev of
            Nothing -> pure ()
            Just ev -> undefined
        Yampa.react handle (0.1, Just ())
