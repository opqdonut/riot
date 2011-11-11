--
-- riot/Riot/UI.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--


-- This file is an ugly hack for many of the types riot uses.
-- It is needed because GHC can't decently handle cyclic module
-- dependencies.


module Riot.Riot (
    KeyMap, Config(..), Status(..), Mode(..), UndoInfo(..), ELCont(..), 
    Encoding(..), EditableEntry(..), Action(..), RIState(..), 
    MsgLineStatus(..),
    RI, run_ri, get_settings, get_status, set_status, 
    mod_status, mod_status_io, mod_status_io2,
    with_status, with_status_io,
    ri_catch, ri_bracket,
    liftIO, gets,
    (<@), (@>),
    cont, cont_clr, cont_noclr, nocont,
    -- Query stuff
    QResult(..), QKeyMap, QAct,
) where

import Prelude hiding (catch)
import Time(getClockTime, toCalendarTime, CalendarTime)
import Control.Exception
import Control.Monad.State
import qualified Curses.Curses as Curses

import Riot.Style (StyleSpec, UIAttr)
import Riot.Entry(Entry(..), EntryTree(..))

-- Configuration

data MsgLineStatus = Untouched     -- message line not modified
                   | ClearNextKey  -- message line modified, should be cleared
                                   -- after the next key has been read
                   | DontClear     -- message line modified, but do not
                                   -- clear it
data ELCont = ELCont MsgLineStatus
            | ELQuit 

type KeyMap = [(Curses.Key, Action)]

data Action = Action String (EditableEntry a => RI a ELCont)
            | TopAction String String 
                (EditableEntry a => RI a ELCont) {- action shown in toplevel
                                                    info -}
            | Submap KeyMap

data Encoding = Enc String 
              | Default

data Config = Config {
    cfg_key_map :: KeyMap,
    cfg_help_key_map :: KeyMap,
    cfg_query_key_map :: QKeyMap,
    cfg_topinfo_text :: String,
    cfg_styles :: [StyleSpec],
    cfg_save_always :: Bool,
    cfg_encoding :: Encoding,
    cfg_backup_numbers :: Int
}


class Entry a => EditableEntry a where
    entry_set_text :: Entry e => a -> String -> CalendarTime -> RI e a

instance EditableEntry a => EditableEntry (EntryTree a) where
    entry_set_text e txt tm = do
        nent <- entry_set_text (entrytree_thisentry e) txt tm
        return e{entrytree_thisentry = nent}

data Mode = DefaultMode | HelpMode String
          deriving (Eq, Show)

data Entry a => Status a = Status {
    attr :: UIAttr,
    topinfo_text :: String,
    textarea_hscroll,
    textarea_vscroll,
    entryarea_vscroll,
    helparea_vscroll :: Int,
    screen_width, 
    screen_height :: Int,
    entries :: [EntryTree a],
    entries_fname :: Maybe String,
    selected_entry :: Int,
    active_entry :: Maybe Int,
    undo_buffer,
    redo_buffer :: [UndoInfo a],
    save_callback :: String -> [EntryTree a] -> RI a (),
    new_callback :: String -> CalendarTime -> RI a a,
    unsaved_changes :: Bool,
    mode :: Mode,
    active_key_map :: KeyMap,
    previous_search :: Maybe String
}

data Entry a => UndoInfo a = UndoInfo {
    undo_selected_entry :: Int,
    undo_active_entry :: Maybe Int,
    undo_unsaved_changes :: Bool,
    undo_entries :: [EntryTree a]
}


-- The Riot Monad

data Entry e => RIState e = RIState {
    ri_status :: Status e,
    ri_settings :: Config
}

type RI e a = StateT (RIState e) IO a

run_ri :: Entry e => Status e -> Config -> RI e a -> IO a
run_ri status cfg ri = evalStateT ri init_state
    where init_state = RIState { ri_status = status, ri_settings = cfg }

get_settings :: Entry e => RI e Config
get_settings = gets ri_settings

get_status :: Entry e => RI e (Status e)
get_status = gets ri_status

set_status :: Entry e => Status e -> RI e ()
set_status status = mod_status (\_ -> status)

mod_status :: Entry e => (Status e -> Status e) -> RI e ()
mod_status f = modify (\state -> let old = ri_status state
                                    in state{ri_status = f old})

mod_status_io2 :: Entry e => (Status e -> IO (Status e, a)) -> RI e a
mod_status_io2 f = do
    s <- get_status
    (news, x) <- liftIO (f s)
    set_status news
    return x

mod_status_io :: Entry e => (Status e -> IO (Status e)) -> RI e ()
mod_status_io f = mod_status_io2 (\s -> do x <- f s
                                           return (x, ()))

with_status_io :: Entry e => (Status e -> IO a) -> RI e a
with_status_io io = do
    s <- get_status
    liftIO $ io s

with_status :: Entry e => (Status e -> a) -> RI e a
with_status f = do
    s <- get_status
    return (f s)

-- It's a bit ugly that we have to re-implement the exception stuff here
-- There was a patch in the haskell mailing list which generalized
-- the usage of the IO monad in the functions dealing with exceptions
-- to MonadIO m => m, however, this patch seems not be included in
-- recent GHC releases.

ri_catch :: Entry e => RI e a     -- The computation to run
        -> (Exception -> RI e a) -- Handler to invoke if an exception is raised
        -> RI e a
ri_catch run handler = do
    oldState <- get
    (result, newState) <- liftIO $ runStateT run oldState
                                   `catch`
                                   (\e -> runStateT (handler e) oldState)
    put newState
    return result

ri_bracket :: Entry e => RI e a -> (a -> RI e b) -> (a -> RI e c) -> RI e c
ri_bracket acq rel run = do
    resource <- acq
    ri_catch (run resource >>= \x -> rel resource >> return x)
             (\e -> rel resource >> liftIO (throwIO e))


-- Some nice operators

(<@) f v = liftM f v
(@>) v f = liftM f v


-- ELCont helpers

cont :: Entry a => RI a ELCont
cont = return $ ELCont Untouched

cont_clr :: Entry a => RI a ELCont
cont_clr = return $ ELCont ClearNextKey

cont_noclr :: Entry a => RI a ELCont
cont_noclr = return $ ELCont DontClear

nocont :: Entry a => RI a ELCont
nocont = return ELQuit


-- Query types {{{

data QResult = QDone String
             | QCancel
             | QModified String Int
             | QResize

type QAct = String -> Int -> QResult

type QKeyMap = [(Curses.Key, QAct)]

-- }}}
