module Main where
import Sound.ALSA.Mixer
import qualified System.Environment as Environment
import Text.Read
import qualified Data.List as List
import qualified Data.List.Split as Split

data VolumeInfo = VolumeInfo {min::Integer,
                              max::Integer,
                              vol::Maybe Integer,
                              playbackVol::Volume}

instance Show VolumeInfo where
  show (VolumeInfo min max (Just vol) _) =
    "Min : " ++ (show min) ++ ", Max : " ++ (show max) ++ ", Vol : " ++ (show vol)
  show (VolumeInfo _ _ Nothing _) = "Something wrong"

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


validateArg :: (Integer -> IO ()) -> String -> IO ()
validateArg f vol =
    case (readMaybe vol) :: Maybe Integer of
        Just x -> f x
        Nothing -> putStrLn "argument doesnt valid"

adjustVolumeStr :: String -> IO ()
adjustVolumeStr arg
    | List.isInfixOf "+" arg = validateArg adjustVolume $ last (Split.splitOn "+" arg)
    | List.isInfixOf "-" arg = validateArg adjustVolume arg
    | otherwise = validateArg setVolume arg
   
main :: IO ()
main = do
    args <- Environment.getArgs
    if length args == 0 then
        putStrLn "please provide argument"
    else do
        let arg = (head args)
        case arg of
            "--min" -> withVolumeDo
                           (\(VolumeInfo minVol _ _ _) -> (putStrLn . show) minVol)
            "--max" -> withVolumeDo
                           (\(VolumeInfo _ maxVol _ _) -> (putStrLn . show) maxVol)
            "--all" -> withVolumeDo $ putStrLn . show
            "--current" -> withVolumeDo
                               (\(VolumeInfo _ _ oldVol _) ->
                               case oldVol of
                                   Just x -> putStrLn $ show x
                                   Nothing -> putStrLn "something wrong")
            _ -> adjustVolumeStr arg


