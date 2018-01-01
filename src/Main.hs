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

setVolumeIfValidVolume
  :: (Integer -> Integer)
     -> (Integer -> Integer -> Volume -> IO ())
     -> IO ()
setVolumeIfValidVolume fVolume fResult =
  withVolumeDo
      (\(VolumeInfo minVol maxVol oldVol playbackVol) -> 
         let setIfValid oldVol newVol
                 | newVol > maxVol = putStrLn "value doesnt valid, > max vol"
                 | newVol < minVol = putStrLn "value doesnt valid, < min vol"
                 | otherwise = fResult oldVol newVol playbackVol
         in case oldVol of 
              Just x -> setIfValid x $ fVolume x 
              _ -> putStrLn "failed")

setPlaybackFrontLeftChannel :: Volume -> Integer -> IO ()
setPlaybackFrontLeftChannel x =
    setChannel FrontLeft (value x)

adjustVolume :: Integer -> IO ()
adjustVolume i =
    setVolumeIfValidVolume
        (+ i)
        (\_ newVol playbackVol -> do
            setPlaybackFrontLeftChannel playbackVol newVol
            putStrLn $ show newVol)
                 
setVolume :: Integer -> IO ()
setVolume i =
  setVolumeIfValidVolume
      (\_ -> i)
      (\oldVol _ playbackVol ->
          let setFrontLeftChannel = 
                  setPlaybackFrontLeftChannel playbackVol 
          in do
            setFrontLeftChannel (-oldVol)
            setFrontLeftChannel i 
            putStrLn $ show i)

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


