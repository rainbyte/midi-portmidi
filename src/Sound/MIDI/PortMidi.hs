module Sound.MIDI.PortMidi
  ( fromMessage
  , fromMidiFileRelative
  , fromTrack
  ) where

import           Control.Arrow (first, second)

import           Data.Bits (Bits, shiftR, shiftL, (.|.), (.&.))
import qualified Data.EventList.Relative.TimeBody as EventList
import           Numeric.NonNegative.Wrapper (toNumber)

import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as MidiFileEvent
import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.Message.System as System
import qualified Sound.MIDI.Message.System.Exclusive as SystemExclusive
import qualified Sound.MIDI.Message.System.Common as SystemCommon
import qualified Sound.MIDI.Message.System.RealTime as SystemRealTime
import qualified Sound.PortMidi as PM

hi7bit :: (Num a, Bits a) => a -> a
hi7bit n = (n `shiftR` 7) .&. 0x7F

lo7bit :: (Num a, Bits a) => a -> a
lo7bit n = n .&. 0x7F

fromMidiFileRelative :: MidiFile.T -> [(Rational, Either String PM.PMMsg)]
fromMidiFileRelative (MidiFile.Cons midiType division tracks) =
  let midiEvents = EventList.toPairList $
        MidiFile.secondsFromTicks division $
          MidiFile.mergeTracks midiType tracks
      fromETimePair = first toNumber
      timedEvents = fmap fromETimePair midiEvents
  in fmap (second fromMidiFileEvent) timedEvents

fromTrack :: MidiFile.Division
          -> MidiFile.Track
          -> [(Rational, Either String PM.PMMsg)]
fromTrack division = fmap (first toNumber . second fromMidiFileEvent)
                   . EventList.toPairList
                   . MidiFile.secondsFromTicks division

fromMidiFileEvent :: MidiFileEvent.T -> Either String PM.PMMsg
fromMidiFileEvent x = case x of
  MidiFileEvent.MIDIEvent y -> case fromMessage $ Msg.Channel y of
    Just msg -> Right msg
    Nothing -> Left $ "Could not convert channel event: " ++ show x
  _ -> Left $ "Non channel event: " ++ show x

-- toMessage :: PM.PMMsg -> Msg.T
-- toMessage pmMsg = _

fromMessage :: Msg.T -> Maybe PM.PMMsg
fromMessage midiMsg =
  case midiMsg of
    Msg.Channel x -> pure $ fromMessageChannel x
    Msg.System x -> fromMessageSystem x

fromMessageChannel :: Channel.T -> PM.PMMsg
fromMessageChannel x = do
  let channel = Channel.fromChannel $ Channel.messageChannel x
      (status, data1, data2) = case Channel.messageBody x of
        Channel.Voice y -> case y of
          Voice.NoteOff pitch velocity ->
            (0x80 :: Int, Voice.fromPitch pitch, Voice.fromVelocity velocity)
          Voice.NoteOn pitch velocity ->
            (0x90, Voice.fromPitch pitch, Voice.fromVelocity velocity)
          Voice.PolyAftertouch pitch pressure ->
            (0xA0, Voice.fromPitch pitch, pressure)
          Voice.Control controller controllerValue ->
            (0xB0, Voice.fromController controller, controllerValue)
          Voice.ProgramChange program ->
            (0xC0, Voice.fromProgram program, 0)
          Voice.MonoAftertouch pressure ->
            (0xD0, pressure, 0)
          Voice.PitchBend pitchBendRange ->
            (0xE0, lo7bit pitchBendRange, hi7bit pitchBendRange)
        Channel.Mode y -> case y of
          Mode.AllSoundOff -> (0xB0, 0x78, 0)
          Mode.ResetAllControllers -> (0xB0, 0x79, 0x7F)
          Mode.LocalControl b -> (0xB0, 0x7A, if b then 0x7F else 0)
          Mode.AllNotesOff -> (0xB0, 0x7B, 0)
          Mode.OmniMode b -> (0xB0, if b then 0x7D else 0x7C, 0)
          Mode.MonoMode n -> (0xB0, 0x7E, n)
          Mode.PolyMode -> (0xB0, 0x7F, 0)
  PM.PMMsg (fromIntegral (status .|. (fromIntegral channel .&. 0xF)))
           (fromIntegral data1 .&. 0x7F)
           (fromIntegral data2 .&. 0x7F)

fromMessageSystem :: System.T -> Maybe PM.PMMsg
fromMessageSystem x = do
  (status, data1, data2) <- case x of
    System.Exclusive _ -> Nothing
--          SystemExclusive._ -> (0xF0, _, _)
--          SystemExclusive._ -> (0xF7, 0, 0)
    System.Common y -> case y of
      SystemCommon.TimeCodeQuarterFrame timeType time ->
        let encType = fromEnum timeType
        --  encType = case timeType of
        --    FrameLS -> 0x0
        --    FrameMS -> 0x1
        --    SecondsLS -> 0x2
        --    SecondsMS -> 0x3
        --    MinutesLS -> 0x4
        --    MinutesMS -> 0x5
        --    HoursLS -> 0x6
        --    HoursMS -> 0x7
            encTime = fromIntegral time .&. 0xF
        in Just (0xF1, (encType `shiftL` 4) .|. encTime, 0)
      SystemCommon.SongPositionPointer n -> Just (0xF2, lo7bit n, hi7bit n)
      SystemCommon.SongSelect songNumber -> Just (0xF3, songNumber, 0)
      SystemCommon.TuneRequest -> Just (0xF6, 0, 0)
    System.RealTime y -> case y of
      SystemRealTime.TimingClock -> Just (0xF8, 0, 0)
      SystemRealTime.Start -> Just (0xFA, 0, 0)
      SystemRealTime.Continue -> Just (0xFB, 0, 0)
      SystemRealTime.Stop -> Just (0xFC, 0, 0)
      SystemRealTime.ActiveSensing -> Just (0xFE, 0, 0)
      SystemRealTime.Reset -> Just (0xFF, 0, 0)
  pure $ PM.PMMsg (fromIntegral status)
                  (fromIntegral data1 .&. 0x7F)
                  (fromIntegral data2 .&. 0x7F)
