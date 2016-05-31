module Main where

import           Control.Arrow (first, second)
import           Control.Concurrent
import           Control.Monad
import qualified Data.EventList.Relative.TimeBody as EventList
import           Numeric.NonNegative.Wrapper (toNumber)
import           Options.Applicative ((<>))
import qualified Options.Applicative as Opt
import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as MidiFile.Event
import qualified Sound.MIDI.File.Load as MidiFile.Load
import qualified Sound.MIDI.Message as Msg
import qualified Sound.PortMidi as PM
import           System.Exit

import           Sound.MIDI.PortMidi (fromMessage)

data CliOptions = CliOptions
  { filePath :: String }

cliOptions :: Opt.Parser CliOptions
cliOptions = CliOptions
  <$> Opt.strOption (  Opt.long "file"
                    <> Opt.metavar "FILE.MID"
                    <> Opt.help "Midifile to reproduce" )

fooBar :: MidiFile.T -> [(Rational, Maybe PM.PMMsg)]
fooBar (MidiFile.Cons midiType division tracks) =
  let midiEvents = EventList.toPairList $ MidiFile.secondsFromTicks division $ MidiFile.mergeTracks midiType tracks
      -- fromETimePair = first (fromIntegral . MidiFile.fromElapsedTime)
      fromETimePair = first toNumber
      timedEvents = fmap fromETimePair midiEvents
      f x = case x of
        MidiFile.Event.MIDIEvent y -> fromMessage $ Msg.Channel y
        _ -> Nothing
  in fmap (second f) timedEvents

main :: IO ()
main = do
  options <- Opt.execParser (Opt.info cliOptions mempty)
  _ <- PM.initialize
  deviceCount <- PM.countDevices
  putStrLn "Output devices:"
  forM_ [0..deviceCount - 1] $ \deviceId -> do
    info <- PM.getDeviceInfo deviceId
    when (PM.output info) $
      putStrLn $ "  " ++ show deviceId ++ ". " ++ PM.name info
  putStr "Select: "
  selectedId <- readLn :: IO Int
  eStream <- PM.openOutput selectedId 0
  case eStream of
    Left stream -> do
      f <- MidiFile.Load.fromFile (filePath options)
      let (MidiFile.Cons midiType division tracks) = f
          tracks' = fmap ( EventList.toPairList
                         . MidiFile.secondsFromTicks division) tracks
      putStrLn $ "Midi Type = " ++ show midiType
      putStrLn $ "Time Division = " ++ show division
      putStrLn $ "N° of tracks: " ++ show (length tracks)
      semStream <- newQSem 1
      semStop <- newQSem 0
      countVar <- newMVar (length tracks')
      forM_ tracks' $ \pairs ->
        forkIO $ do
          forM_ pairs $ \(t, c) -> do
            threadDelay $ round $ toNumber t * 10^6
            mMsg <- case c of
              MidiFile.Event.MIDIEvent y -> pure $ fromMessage $ Msg.Channel y
              x -> do
                putStrLn $ "Non channel event: " ++ show x
                pure Nothing
            case mMsg of
              Just msg -> do
                waitQSem semStream
                err <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
                signalQSem semStream
                when (err /= PM.NoError) $
                  putStrLn $ "err = " ++ show err
              Nothing ->
                pure ()
          count' <- takeMVar countVar
          if count' > 1
            then putMVar countVar (count' - 1)
            else signalQSem semStop
      waitQSem semStop
      _ <- PM.close stream
      _ <- PM.terminate
      exitSuccess
    Right pmError -> do
      _ <- error $ show pmError
      exitFailure
