module Main where
import Sound.ALSA.Mixer
import qualified Control.Monad as CM

changeVolumeBy :: Integer -> IO ()
changeVolumeBy i =
    withMixer "default" $ \mixer ->
      do Just control <- getControlByName mixer "Master"
         let Just playbackVolume = playback $ volume control
         (min, max) <- getRange playbackVolume
         Just volLeft <- getChannel FrontLeft $ value $ playbackVolume
         Just volRight <- getChannel FrontRight $ value $ playbackVolume
         CM.when True
                 (setChannel FrontLeft (value $ playbackVolume) $ volLeft + i) >>
                 (setChannel FrontRight (value $ playbackVolume) $ volRight + i)

main :: IO ()
main = do
  putStrLn "atur-polum"


