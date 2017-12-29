module Main where
import Sound.ALSA.Mixer
import qualified Control.Monad as CM
import qualified System.Environment as Environment
import Text.Read
import qualified Data.List as List
import qualified Data.List.Split as Split

data VolumeInfo = VolumeInfo {min::Integer,
                              max::Integer,
                              vol::Maybe Integer,
                              playbackVol::Volume}

withVolumeDo :: (VolumeInfo -> IO a) -> IO a
withVolumeDo f =
      withMixer "default" $ \mixer -> do
         Just control <- getControlByName mixer "Master"
         let Just playbackVolume = playback $ volume control
         (minVolume, maxVolume) <- getRange playbackVolume
         oldVolume <- getChannel FrontLeft $ value playbackVolume

         f $ VolumeInfo minVolume maxVolume oldVolume playbackVolume

adjustVolume :: Integer -> IO ()
adjustVolume i = withVolumeDo (\(VolumeInfo minVol maxVol oldVol playbackVol) ->
                 case oldVol of 
                     Just x -> do
                         let newVolume = x + i
                         if newVolume >= minVol  && newVolume <= maxVol then do 
                             setChannel FrontLeft (value playbackVol) newVolume
                             putStrLn $ show newVolume
                         else
                             putStrLn "volume reach bound"
                     _ -> putStrLn "failed")

setVolume :: Integer -> IO ()
setVolume i =
         withVolumeDo
          (\(VolumeInfo minVol maxVol oldVol playbackVol) ->
              case oldVol of 
              Just x -> do
                  if i >= minVol && i <= maxVol then do 
                      setChannel FrontLeft (value playbackVol) (-x)
                      setChannel FrontLeft (value playbackVol) i 
                      putStrLn $ show i
                  else
                      putStrLn "volume reach bound"
              _ -> putStrLn "failed")


adjustVolumeStr :: (Integer -> IO ()) -> String -> IO ()
adjustVolumeStr f vol =
    case (readMaybe vol) :: Maybe Integer of
        Just x -> f x
        Nothing -> putStrLn "argument doesnt valid"

adjustVolumeStrWithSign :: String -> IO ()
adjustVolumeStrWithSign arg
    | List.isInfixOf "+" arg = adjustVolumeStr adjustVolume $ last (Split.splitOn "+" arg)
    | List.isInfixOf "-" arg = adjustVolumeStr adjustVolume arg
    | otherwise = adjustVolumeStr setVolume arg
   
main :: IO ()
main = do
    args <- Environment.getArgs
    if length args == 0 then
        withVolumeDo
            (\(VolumeInfo _ _ oldVol _) ->
                case oldVol of
                    Just x -> putStrLn $ show x
                    Nothing -> putStrLn "something wrong")
    else do
        let arg = (head args)
        case arg of
            "--min" -> withVolumeDo
                           (\(VolumeInfo minVol _ _ _) -> (putStrLn . show) minVol)
            "--max" -> withVolumeDo
                           (\(VolumeInfo _ maxVol _ _) -> (putStrLn . show) maxVol)
            _ -> adjustVolumeStrWithSign arg


