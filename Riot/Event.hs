--
-- riot/Riot/Event.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2006.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module Riot.Event(
    event_loop,
    -- Event handlers
    e_vscroll, e_hscroll, e_pgdn, e_pgup, e_eob, e_bob,
    e_selectentry, e_nextentry, e_preventry, 
    e_pgdnentry, e_pgupentry, e_firstentry, e_lastentry,
    e_actentry, e_delentry, e_editentry, e_collapse,
    e_quit, e_refresh, e_none, e_submap, e_fallback,
    e_newentry, e_newunder, e_undo, e_redo,
    e_tag, e_clear_tags, e_delete_tagged,
    e_move_tagged_after, e_move_tagged_before, e_move_tagged_under, 
    e_save, e_help, e_quit_help, e_help_up, e_help_down,
    e_clear_message
) where

import qualified Curses.Curses as Curses
--import qualified Curses.CursesHelper as CursesH

import qualified Control.Exception
import Time(getClockTime, toCalendarTime, CalendarTime)
import System.Posix.IO (setFdOption, FdOption(..))
import Control.Monad(liftM, when)
import Maybe
import List(init, intersperse)
import Char

import Riot.Style(UIAttr(..), init_uiattr, default_uiattr, StyleSpec)
import Riot.UI
import Riot.Entry
import Riot.Riot
import Riot.Query
import Riot.Locale

import qualified Riot.Editor as Editor
import qualified Riot.KeyMap as KeyMap


-- Edit {{{

unmodified_or_empty :: String -> String -> Bool
unmodified_or_empty n o =
    n == o || (dropWhile isSpace n == [])


do_modify :: EditableEntry a => 
             EntryTree a -> Loc -> (String, CalendarTime) -> RI a ()
do_modify ent loc (newtext, tm) = do
    newent <- entry_set_text ent newtext tm
    mod_status (\s -> let ss = snapshot s
                          et = entrytree_replace (entries s) loc newent
                          pos = find_viewlist_pos et loc
                      in check_everything $ ss{entries = et, 
                                               active_entry = Just pos,
                                               selected_entry = pos})


do_edit :: Entry e => 
    String 
    -> ((String, CalendarTime) -> RI e ELCont)
    -> RI e ELCont
do_edit text cont = do
    (m, v) <- liftIO $ Curses.withCursor Curses.CursorVisible $ do
        Curses.endWin
        text_ <- to_locale text
        newtext_ <- Editor.edittext text_
        newtext <- from_locale newtext_
        t <- toCalendarTime =<< getClockTime
        Curses.clearOk True
        Curses.refresh
        setFdOption 0 NonBlockingRead False
        return (unmodified_or_empty newtext_ text_, (newtext, t))
    mod_status_io get_size -- Update size information
    if m then
        do_except (Control.Exception.ErrorCall 
	           "Aborted unmodified or empty entry.")
      else
        cont v

do_insert :: Entry a => Status a -> InsertWhere -> EntryTree a -> Status a
do_insert s insw ent =
    check_everything $ ss{entries = et, 
                          active_entry = Just pos,
                          selected_entry=pos}
    where
        ss = snapshot s
        (et_, loc) = entrytree_insert (entries s) insw [ent]
        et = case loc of
                 Loc (_:[]) -> et_
                 Loc ll -> fromMaybe et_ $ entrytree_expand et_ $ Loc (List.init ll)
        pos = find_viewlist_pos et loc

do_editentry :: EditableEntry a => Int -> RI a ELCont
do_editentry e = do
    (loc, ent) <- with_status (\s -> fromJust $ lnth e (entries_viewlist s))
    do_edit (entry_text ent) $ \new -> do
        do_modify ent loc new
	cont_refresh

do_newentry :: Entry a => InsertWhere -> RI a ELCont
do_newentry insw =
    do_edit "" $ \(text, tm) -> do
        status <- get_status
        e <- new_callback status text tm
        mod_status (\s -> do_insert s insw $ new_entrytree e)
        cont_refresh


-- }}}


-- Undo/redo/save {{{


use_snapshot :: Entry a => 
    Status a -> UndoInfo a -> [UndoInfo a] -> [UndoInfo a] -> Status a
use_snapshot s (UndoInfo sel act unsaved et) ub rb =
    check_entryarea $ s{
        undo_buffer = ub,
        redo_buffer = rb,
        entries = et,
        selected_entry = sel,
        active_entry = act,
        textarea_vscroll = 0,
        textarea_hscroll = 0,
        unsaved_changes = unsaved
    }

mark_saved :: Entry a => Status a -> Status a
mark_saved s =
    s{
        unsaved_changes = False,
        undo_buffer = map set_unsaved (undo_buffer s),
        redo_buffer = map set_unsaved (redo_buffer s)
    }
    where
        set_unsaved u = u{undo_unsaved_changes = True}

undo :: Entry a => Status a -> Status a
undo s =
    case undo_buffer s of 
        [] -> s
        (su:ss) -> use_snapshot s su ss ((mk_snapshot s):(redo_buffer s))


redo :: Entry a => Status a -> Status a
redo s =
    case redo_buffer s of 
        [] -> s
        (sr:ss) -> use_snapshot s sr ((mk_snapshot s):(undo_buffer s)) ss

do_save :: Entry a => String -> RI a ()
do_save fname = do 
    status <- get_status
    (save_callback status) fname (entries status)
    mod_status mark_saved

save :: Entry a => RI a ()
save = do
    status <- get_status
    case entries_fname status of
        Nothing -> error "No file name set."
        Just fname -> do_save fname

-- }}}


-- Event handling {{{

-- Generic stuff

cont_refresh :: Entry a => RI a ELCont
cont_refresh = do
    refresh 
    cont


e_quit :: Entry a => RI a ELCont
e_quit = do
    b <- with_status unsaved_changes
    if b 
         then yesno "Save changes before quitting?" (save >> nocont) nocont
         else nocont

e_refresh :: Entry a => RI a ELCont
e_refresh = cont_refresh

e_none :: Entry a => RI a ELCont
e_none = cont

e_submap :: EditableEntry a => [Curses.Key] -> KeyMap -> RI a ELCont
e_submap keys submap = do
    with_status_io draw_submap
    k <- get_key refresh_fn       
    if KeyMap.isAbort k
       then do with_status_io clear_message
               cont
       else do c <- get_key_handler2 keys submap k 
               case c of
                 ELCont Untouched -> with_status_io clear_message
                 _ -> return ()
               return c
    where
        draw_submap s = do
            message s ("[" ++ KeyMap.keylist2str keys ++ "-]")
        refresh_fn = do redraw 
                        with_status_io draw_submap 


e_fallback :: EditableEntry a => [Curses.Key] -> KeyMap 
           -> Curses.Key -> RI a ELCont
-- Check if control is set and call handle_key again with the plain key.
e_fallback keys keyMap (Curses.KeyChar k)
     | ord '\^A' <= ord k && ord k <= ord '\^Z' =
         get_key_handler2 keys keyMap 
           (Curses.KeyChar $ chr $ ord k - ord '\^A' + ord 'a')
e_fallback keys _  _ = do
    with_status_io (\s -> message s ("Key is not bound: " ++ 
                                     KeyMap.keylist2str keys))
    cont_clr

goto_next_or_redraw :: Entry a => RI a ELCont
goto_next_or_redraw = do
    status <- get_status
    if (selected_entry status) + 1 < (n_entries_viewed status)
        then e_nextentry
        else do with_status_io draw_entries
                cont

-- Text view manipulation

update_scroll :: Entry a => RI a ELCont
update_scroll = do
    with_status_io draw_textarea
    with_status_io draw_botinfo
    cont 

e_vscroll :: Entry a => Int -> RI a ELCont
e_vscroll amount = do
    mod_status $ do_vscroll amount
    update_scroll

e_hscroll :: Entry a => Int -> RI a ELCont
e_hscroll amount = do
    mod_status $ do_hscroll amount
    update_scroll

e_pgdn :: Entry a => RI a ELCont
e_pgdn = do
    n <- with_status (\s -> ((textarea_height s) `div` 2))
    e_vscroll n

e_pgup :: Entry a => RI a ELCont
e_pgup = do
    n <- with_status (\s -> (-(textarea_height s) `div` 2))
    e_vscroll n

e_eob :: Entry a => RI a ELCont
e_eob = do
    mod_status (\s -> s{textarea_vscroll = max_vscroll s})
    update_scroll 

e_bob :: Entry a => RI a ELCont
e_bob = do
    mod_status (\s -> s{textarea_vscroll = 0})
    update_scroll

-- Entry tree manipulation

e_selectentry :: Entry a => Int -> RI a ELCont
e_selectentry n = do
    mod_status (do_selectentry n)
    with_status_io draw_entries
    with_status_io draw_midinfo 
    cont

e_nextentry :: Entry a => RI a ELCont
e_nextentry = do
    n <- with_status (\s -> selected_entry s + 1) 
    e_selectentry n

e_preventry :: Entry a => RI a ELCont
e_preventry = do
    n <- with_status (\s -> selected_entry s - 1)
    e_selectentry n

e_pgdnentry :: Entry a => RI a ELCont
e_pgdnentry = do
    n <- with_status f
    e_selectentry n
    where
        f s = min (n_entries_viewed s - 1) 
                  $ selected_entry s + (entryarea_height s `div` 2)

e_pgupentry :: Entry a => RI a ELCont
e_pgupentry = do
    n <- with_status f
    e_selectentry n
    where
        f s = max 0 $ selected_entry s - (entryarea_height s `div` 2)

e_lastentry :: Entry a => RI a ELCont
e_lastentry = do
    n <- with_status (\s -> n_entries_viewed s - 1)
    e_selectentry n

e_firstentry :: Entry a => RI a ELCont
e_firstentry = e_selectentry 0

e_actentry :: Entry a => RI a ELCont
e_actentry = do
    sel <- with_status selected_entry
    mod_status (do_actentry sel)
    mod_status (\s -> do_expand s sel)
    refresh
    cont

e_delentry :: Entry a => RI a ELCont
e_delentry = do
     n <- with_status selected_entry
     mod_status (\s -> do_delentry s n)
     cont_refresh

e_collapse :: Entry a => RI a ELCont
e_collapse = do
    mod_status (\s -> do_collapse_p s $ selected_entry s)
    cont_refresh

e_editentry :: EditableEntry a => RI a ELCont
e_editentry = do
    sel <- with_status selected_entry
    do_editentry sel

e_newentry :: Entry a => RI a ELCont
e_newentry = do
    loc <- with_status (\s -> case null (entries s) of
                                  True -> First
                                  False -> (After $ selected_entry_loc s))
    do_newentry loc
            
e_newunder :: Entry a => RI a ELCont
e_newunder = do
    loc <- with_status (\s -> case null (entries s) of
                                  True -> First
                                  False -> (FirstUnder (selected_entry_loc s)))
    do_newentry loc

e_undo :: Entry a => RI a ELCont
e_undo = do
    mod_status undo
    cont_refresh
 
e_redo :: Entry a => RI a ELCont
e_redo = do
    mod_status redo
    cont_refresh
    

-- Tags

e_tag :: Entry a => TagAction -> RI a ELCont
e_tag act = do
    mod_status (\s -> do_tag s act (selected_entry s))
    goto_next_or_redraw

e_clear_tags :: Entry a => RI a ELCont
e_clear_tags = do
    mod_status do_clear_tags
    with_status_io draw_entries
    cont

e_move_tagged_after :: Entry a => RI a ELCont
e_move_tagged_after = do
    mod_status (\s -> do_move_tagged s (After $ selected_entry_loc s))
    cont_refresh 

e_move_tagged_before :: Entry a => RI a ELCont
e_move_tagged_before = do
    mod_status (\s -> do_move_tagged s (Before $ selected_entry_loc s))
    cont_refresh 

e_move_tagged_under :: Entry a => RI a ELCont
e_move_tagged_under = do
    mod_status (\s -> do_move_tagged s (FirstUnder $ selected_entry_loc s))
    mod_status (\s -> do_expand s (selected_entry s))
    cont_refresh

e_delete_tagged :: Entry a => RI a ELCont
e_delete_tagged = do
    mod_status (\s -> do_delete s $ entrytree_get_tagged $ entries s)
    cont_refresh

-- Misc.

e_save :: Entry a => RI a ELCont
e_save = do
    save
    with_status_io draw_midinfo
    with_status_io (\s -> message s ("Saved " ++ 
                                     (fromMaybe "???" $  entries_fname s)))
    cont_clr

e_clear_message :: Entry a => RI a ELCont
e_clear_message = do
    with_status_io clear_message
    cont

e_help :: Entry a => RI a ELCont
e_help = do
    km <- gets (cfg_key_map . ri_settings)
    let help_text = concat (intersperse "\n" (KeyMap.gen_help_text km))
    help_km <- gets (cfg_help_key_map . ri_settings)
    mod_status (\s -> s {mode = HelpMode help_text, 
                         active_key_map = help_km})
    cont_refresh
       
e_quit_help :: Entry a => RI a ELCont
e_quit_help = do
    km <- gets (cfg_key_map . ri_settings)
    mod_status (\s -> s {mode = DefaultMode, 
                         active_key_map = km})
    cont_refresh

update_scroll_help :: Entry a => RI a ELCont
update_scroll_help = do
    mode <- gets (mode . ri_status)
    let text = case mode of
                   HelpMode t -> t
                   _ -> ""
    with_status_io (\s -> draw_helparea s text)
    with_status_io (\s -> draw_botinfo_help s text)
    cont 

e_vscroll_help :: Entry a => Int -> RI a ELCont
e_vscroll_help amount = do
    mod_status $ do_vscroll_help amount
    update_scroll_help

e_help_up :: Entry a => RI a ELCont
e_help_up = do
    n <- with_status (\s -> (-(helparea_height s) `div` 2))
    e_vscroll_help n

e_help_down :: Entry a => RI a ELCont
e_help_down = do
    n <- with_status (\s -> ((helparea_height s) `div` 2))
    e_vscroll_help n

get_key_handler ::  EditableEntry a => KeyMap -> Curses.Key -> RI a ELCont
get_key_handler km k = get_key_handler2 [] km k

-- The key list parameter is needed for submaps to be able to display the
-- keys entered so for
get_key_handler2 ::  EditableEntry a => [Curses.Key] -> KeyMap 
                 -> Curses.Key -> RI a ELCont
get_key_handler2 keys  km k = 
    case lookup k km of
      Just (Action _ f) -> f
      Just (TopAction _ _ f) -> f
      Just (Submap km') -> e_submap (keys ++ [k]) km' 
      Nothing           -> e_fallback (keys ++ [k]) km k 

-- }}}


-- Main event loop {{{

getCh :: IO (Maybe Curses.Key)
getCh = do
    v <- Curses.getch
    return $ case v of
        (-1) -> Nothing
        k_ -> Just $ Curses.decodeKey k_

de_elcont :: ELCont -> (Bool, Bool)
de_elcont ELQuit = (False, False)
de_elcont (ELCont ClearNextKey) = (True, True)
de_elcont _ = (True, False)

save_changes :: Entry a => RI a ()
save_changes = do
    uns <- unsaved_changes <@ get_status
    when uns $ do
        ri_catch e_save do_except
	return ()

do_except :: Entry a => Control.Exception.Exception -> RI a ELCont
do_except e = do
    s <- get_status
    liftIO $ Curses.beep
    liftIO $ do_message s attr_error (show e)
    return (ELCont ClearNextKey)

event_loop_ :: EditableEntry a => Bool -> RI a ()
event_loop_ clr = do
    k <- get_key refresh
    with_status_io $ \s -> if clr then clear_message s else return ()
    km <- gets (active_key_map . ri_status)
    (cont, clr) <- liftM de_elcont $
        ri_catch (get_key_handler km k) do_except
    so <- cfg_save_always <@ get_settings
    when so save_changes
    when cont (event_loop_ clr)

event_loop :: EditableEntry a => RI a ()
event_loop = 
    ri_bracket (liftIO $ Curses.cursSet Curses.CursorInvisible)
	       (\_ -> liftIO $ Curses.cursSet Curses.CursorVisible)
               (\_ -> event_loop_ False)

-- }}}

