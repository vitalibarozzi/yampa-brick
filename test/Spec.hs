import FRP.Yampa.Brick
import FRP.Yampa ((>>>),arr)
import qualified FRP.Yampa as Yampa
import qualified Brick
import qualified Data.Text as Text
import System.Timeout
import Control.Concurrent
import Control.Monad


main :: IO ()
main = do
    (chan,handle) <- reactInitBrick () $ Yampa.time >>> arr (Brick.txt . Text.pack . show)
    forever $ do
        threadDelay    100000
        mev <- timeout 1000 $ readChan chan
        case mev of
            Nothing -> pure ()
            Just ev -> error ("received input from Vty: "<>show ev)
        Yampa.react handle (0.1, Just ())
