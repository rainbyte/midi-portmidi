module Main where

import           Control.Concurrent
import           Control.Monad
import           Options.Applicative ((<>))
import qualified Options.Applicative as Opt
import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Load as MidiFile.Load
import qualified Sound.PortMidi as PM
import           System.Exit

import           Sound.MIDI.PortMidi (fromTrack)

data CliOptions = CliOptions
  { filePath :: String }

cliOptions :: Opt.Parser CliOptions
cliOptions = CliOptions
  <$> Opt.strOption (  Opt.long "file"
                    <> Opt.metavar "FILE.MID"
                    <> Opt.help "Midifile to reproduce" )

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
          tracks' = fmap (fromTrack division) tracks
      putStrLn $ "Midi Type = " ++ show midiType
      putStrLn $ "Time Division = " ++ show division
      putStrLn $ "N° of tracks: " ++ show (length tracks)
      semStream <- newQSem 1
      semStop <- newQSem 0
      countVar <- newMVar (length tracks')
      forM_ tracks' $ \pairs ->
        forkIO $ do
          forM_ pairs $ \(t, c) -> do
            threadDelay $ round $ t * 10^6
            case c of
              Right msg -> do
                waitQSem semStream
                err <- PM.writeShort stream (PM.PMEvent (PM.encodeMsg msg) 0)
                signalQSem semStream
                when (err /= PM.NoError) $
                  putStrLn $ "err = " ++ show err
              Left str ->
                putStrLn str
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
