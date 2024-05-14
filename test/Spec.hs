import FRP.Yampa.Brick
import FRP.Yampa ((>>>),arr)
import qualified FRP.Yampa as Yampa
import qualified Brick
import qualified Data.Text as Text
import Control.Concurrent
import Control.Monad


main :: IO ()
main = do
    callbackHandle <- Yampa.reactInit (pure Yampa.NoEvent) (\_ updated ev -> when updated (print ev) >> pure False) Yampa.returnA
    handle  <- reactInitBrick () (Yampa.time >>> arr (Brick.txt . Text.pack . show)) callbackHandle
    forever $ do
        threadDelay    100000
        Yampa.react handle (0.1, Just ())
