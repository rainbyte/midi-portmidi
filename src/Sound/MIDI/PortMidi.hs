module Sound.MIDI.PortMidi
  ( fromMessage
  ) where

import           Data.Bits (shiftR, shiftL, (.|.), (.&.))

import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Mode as Mode
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.Message.System as System
import qualified Sound.MIDI.Message.System.Exclusive as SystemExclusive
import qualified Sound.MIDI.Message.System.Common as SystemCommon
import qualified Sound.MIDI.Message.System.RealTime as SystemRealTime
import qualified Sound.PortMidi as PM

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
            (0xE0, loByte pitchBendRange, hiByte pitchBendRange)
        Channel.Mode y -> case y of
          Mode.AllSoundOff -> (0xB0, 0x78, 0)
          Mode.ResetAllControllers -> (0xB0, 0x79, 0x7F)
          Mode.LocalControl b -> (0xB0, 0x7A, if b then 0x7F else 0)
          Mode.AllNotesOff -> (0xB0, 0x7B, 0)
          Mode.OmniMode b -> (0xB0, if b then 0x7D else 0x7C, 0)
          Mode.MonoMode n -> (0xB0, 0x7E, n)
          Mode.PolyMode -> (0xB0, 0x7F, 0)
  PM.PMMsg (fromIntegral (status .|. (fromIntegral channel .&. 0xF)))
           (fromIntegral data1)
           (fromIntegral data2)

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
      SystemCommon.SongPositionPointer n -> Just (0xF2, loByte n, hiByte n)
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
                  (fromIntegral data1)
                  (fromIntegral data2)

hiByte n = n `shiftR` 8
loByte n = n .&. 0xFF
