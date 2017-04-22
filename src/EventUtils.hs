module EventUtils where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Event as Event

-- Test if a controller event is an "all notes off" message.

allNotesOff Event.Controller 
            (Event.Ctrl {Event.ctrlParam = Event.Parameter {Event.unParameter = 123}}) = True
allNotesOff _ _ = False

-- Test if this is a custom message we send for pinging

pingEvent (Event.CustomEv _ _) = True
pingEvent _ = False

-- Extract channel number from a note or control event.

eventChannel :: Event.T -> Maybe Event.Channel

eventChannel e = case Event.body e of
  Event.CtrlEv _ ctrl@Event.Ctrl {} -> Just $ Event.ctrlChannel ctrl
  Event.NoteEv _ note@Event.Note {} -> Just $ Event.noteChannel note
  _ -> Nothing


