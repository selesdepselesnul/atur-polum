module Main where
import Sound.ALSA.Mixer
import qualified Control.Monad as CM
import qualified System.Environment as Environment

changeVolumeBy :: Integer -> IO ()
changeVolumeBy i =
    withMixer "default" $ \mixer ->
      do Just control <- getControlByName mixer "Master"
         let Just playbackVolume = playback $ volume control
         (min, max) <- getRange playbackVolume
         vol <- getChannel FrontLeft $ value $ playbackVolume
         case vol of
           Just x -> 
             CM.when True
                 (setChannel FrontLeft (value $ playbackVolume) (x + i) )
           _ -> putStrLn "failed"

main :: IO ()
main = do
    args <- Environment.getArgs
    if length args == 0 then
      putStrLn "please, fill the argument"
    else
      handlePolum $ head args
    where handlePolum arg = 
           case arg of
           "+" -> changeVolumeBy 1
           _ -> putStrLn "argument doesnt valid" 

