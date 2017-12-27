module Main where
import Sound.ALSA.Mixer
import qualified Control.Monad as CM
import qualified System.Environment as Environment
import Text.Read

adjustVolume :: Integer -> IO ()
adjustVolume i =
    withMixer "default" $ \mixer ->
      do Just control <- getControlByName mixer "Master"
         let Just playbackVolume = playback $ volume control
         (minVolume, maxVolume) <- getRange playbackVolume
         oldVolume <- getChannel FrontLeft $ value playbackVolume
         
         case oldVolume of 
           Just x -> do
             let newVolume = x + i
             if (newVolume >= minVolume ) && (newVolume <= maxVolume) then do 
                 setChannel FrontLeft (value playbackVolume) newVolume
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
      case (readMaybe (head args)) :: Maybe Integer of
      Just x -> adjustVolume x
      Nothing -> putStrLn "argument doesnt valid"



