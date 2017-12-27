module Main where
import Sound.ALSA.Mixer
import qualified Control.Monad as CM
import qualified System.Environment as Environment

adjustVolume :: Integer -> IO ()
adjustVolume i =
    withMixer "default" $ \mixer ->
      do Just control <- getControlByName mixer "Master"
         let Just playbackVolume = playback $ volume control
         (min, max) <- getRange playbackVolume
         oldVolume <- getChannel FrontLeft $ value playbackVolume
                 
         case oldVolume of 
           Just x -> do
             let newVolume = x + i
             if (newVolume >= min ) && (newVolume <= max) then do 
                 setChannel FrontLeft (value $ playbackVolume) newVolume
                 putStrLn $ show newVolume
             else
                 putStrLn "volume reach bound"
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
           "+" -> adjustVolume 1
           "-" -> adjustVolume (-1)
           _ -> putStrLn "argument doesnt valid" 
