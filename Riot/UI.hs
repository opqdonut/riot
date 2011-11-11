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


-- Module info {{{

module Riot.UI(
    Riot.UI.init,
    deinit,
    new_status,
    set_topinfo,
    set_entries,
    set_callbacks,
    change_entry_type,
    refresh,
    redraw,
    snapshot,
    -- Low-level drawing
    draw_textarea,
    draw_topinfo,
    draw_midinfo,
    draw_botinfo,
    draw_entries,
    draw_helparea,
    draw_botinfo_help,
    clear_message,
    -- Low-level
    message_line,
    getCh,
    get_key,
    get_key_,
    get_size,
    find_viewlist_pos,
    entries_viewlist,
    n_entries_viewed,
    do_vscroll,
    do_hscroll,
    max_vscroll,
    textarea_height,
    entryarea_height,
    helparea_height,
    do_selectentry,
    do_actentry,
    do_delentry,
    check_everything,
    check_entryarea,
    mk_snapshot,
    do_tag,
    do_clear_tags,
    do_expand,
    do_collapse_p,
    selected_entry_loc,
    do_move_tagged,
    do_delete,
    do_vscroll_help,
    -- Very low-level
    fill_to_eol,
    cset_attr,
    creset_attr,
    waddstr,
    -- Misc, to be moved elsewhere
    lnth
)where

-- }}}


-- Imports {{{

import qualified Curses.Curses as Curses
import qualified Control.Exception
import Time(CalendarTime)
import IO(stdin)
import Control.Monad(liftM, when)
import Maybe
import Char
import List(find,init,intersperse)
import Control.Monad.Trans(MonadIO, liftIO)
import System.Posix.Signals
import System.Posix.IO (setFdOption, FdOption(..))

import Riot.Style(UIAttr(..), init_uiattr, default_uiattr, StyleSpec)
import Riot.Entry
import Riot.Riot
import qualified Riot.Editor as Editor
import qualified Riot.KeyMap as KeyMap

-- }}}


-- Helper functions {{{

text_n_lines :: String -> Int
text_n_lines text =
    length (lines text)

text_n_columns :: String -> Int
text_n_columns text =
    foldl max 0 (map length (lines text))

safetail [] = []
safetail (x:xs) = xs

maybehead [] = Nothing
maybehead (x:xs) = Just x

left_align w t =
    take w (t ++ repeat ' ')

lnth n l = lookup n $ zip [0..] l

mpass :: (a -> b) -> Maybe a -> Maybe b
mpass f Nothing = Nothing
mpass f (Just x) = Just $ f x

-- }}}



-- Status {{{

undo_count = 100

new_status :: Entry a => KeyMap -> Status a
new_status km = Status {
    attr = default_uiattr,
    topinfo_text = "--",
    textarea_hscroll = 0,
    textarea_vscroll = 0,
    entryarea_vscroll = 0,
    helparea_vscroll = 0,
    screen_width = 80,
    screen_height = 24,
    entries = [],
    entries_fname = Nothing,
    selected_entry = 0,
    active_entry = Nothing,
    undo_buffer = [],
    redo_buffer = [],
    save_callback = save_disabled,
    new_callback = new_disabled,
    unsaved_changes = False,
    mode = DefaultMode,
    active_key_map = km,
    previous_search = Nothing
}

new_disabled _ _ = error "No handler to create new entries set."
save_disabled _ _ = error "No save handler set."

set_topinfo :: Entry a => Status a -> String -> Status a
set_topinfo s v = s{topinfo_text = v}

set_entries :: Entry a => Status a -> [EntryTree a] -> (Maybe String) -> Status a
set_entries s e fname =
    s{
        entries = e,
        entries_fname = fname,
        selected_entry = 0,
        active_entry = Nothing,
        entryarea_vscroll = 0,
        textarea_vscroll = 0,
        textarea_hscroll = 0,
        unsaved_changes = False,
        undo_buffer = [],
        redo_buffer = []
    }

change_entry_type :: (Entry a, Entry b) => Status a -> Status b
change_entry_type s =
    (new_status (active_key_map s)) {
        attr = attr s,
        topinfo_text = topinfo_text s,
        screen_width = screen_width s,
        screen_height = screen_height s
    } 

set_callbacks :: Entry a =>
    Status a
    -> (String -> [EntryTree a] -> RI a ())
    -> (String -> CalendarTime -> RI a a)
    -> Status a
set_callbacks s save_cb new_cb =
    s{save_callback = save_cb, new_callback = new_cb}

-- }}}


-- Sizes {{{

entryarea_height s = max 1 $ (screen_height s - 3) `div` 2
topinfo_line s = 0
midinfo_line s = (entryarea_height s) + 1
botinfo_line s = (max (screen_height s - 1) (midinfo_line s + 2)) - 1
message_line s = (botinfo_line s) + 1
entryarea_startline s = 1
entryarea_endline s = midinfo_line s - 1
textarea_startline s = midinfo_line s + 1
textarea_endline s = botinfo_line s - 1
textarea_height s = (textarea_endline s) - (textarea_startline s) + 1
helparea_startline s = 1
helparea_endline s = botinfo_line s - 1
helparea_height s = (helparea_endline s) - (helparea_startline s) + 1
-- }}}


-- Viewlist indexing {{{

valid_entry s e = 0<=e && e<n_entries_viewed s

entrytree_viewlist :: Entry a => [EntryTree a] -> [(Loc, EntryTree a)]
entrytree_viewlist et =
    entrytree_map f et
    where
       f e chview loc =
           if entrytree_expanded e then
               (loc, e):chview
           else
               [(loc, e)]

entries_viewlist s = entrytree_viewlist (entries s)
entries_viewlist_plain s = snd $ unzip $ entries_viewlist s
entries_viewlist_loc s = fst $ unzip $ entries_viewlist s

viewlist_entry s e = lnth e (entries_viewlist_plain s)
viewlist_entry_loc s e = fromJust $ lnth e (entries_viewlist_loc s)
selected_entry_loc s = viewlist_entry_loc s (selected_entry s)
active_entry_loc s = mpass (viewlist_entry_loc s) (active_entry s)

n_entries_viewed s = length $ entries_viewlist s

find_viewlist_pos et loc = 
    viewlist_pos 0 et loc
    where
        viewlist_pos cpos (e:_) (Loc (0:[])) = cpos
        viewlist_pos cpos (e:_) (Loc (0:ll)) =
            case entrytree_expanded e of
                False -> cpos -- Go to parent
                True -> viewlist_pos (cpos+1) (entrytree_children e) (Loc ll)
        viewlist_pos cpos (e:et) (Loc (l:ll)) =
            viewlist_pos (cpos+1+nchv) et $ Loc ((l-1):ll)
            where
                nchv = case entrytree_expanded e of
                           False -> 0
                           True -> length $ entrytree_viewlist $ entrytree_children e
        viewlist_pos _ _ _ = error "Invalid location"

-- }}}


-- Initialisation {{{

get_size :: Entry a => Status a -> IO (Status a)
get_size s = do
    (h, w) <- Curses.scrSize
    return s{screen_width = w, screen_height = h}

init :: Entry a => Status a -> [StyleSpec] -> IO (Status a)
init status styles = do
    Curses.initCurses
    Curses.resetParams
    setFdOption 0 NonBlockingRead False
    a <- init_uiattr styles
    get_size status{attr = a}

deinit :: IO ()
deinit = do
    Curses.endWin
    return ()
 
-- }}}


-- Drawing {{{

waddstr w s = Control.Exception.try (Curses.wAddStr w s) >> return ()

cset_attr (a, p) = do
    Curses.wAttrSet Curses.stdScr (a, p)

creset_attr = do
    cset_attr (Curses.attr0, Curses.Pair 0)

fill_to_eol = do
    (h, w) <- Curses.scrSize
    (y, x) <- Curses.getYX Curses.stdScr
    waddstr Curses.stdScr (replicate (max 0 (w-x)) ' ')

draw_lines s d l_first l_last skip f = do
    Curses.wMove Curses.stdScr l_first 0
    do_draw_lines s (drop skip d) l_first l_last skip f
    where
        do_draw_lines s d l l_last nr f
            | l>l_last = return ()
            | otherwise = do
                f s (maybehead d) nr
                do_draw_lines s (safetail d) (l+1) l_last (nr+1) f


-- Help area

draw_helparea :: Entry a => Status a -> String -> IO ()
draw_helparea s help_text = 
    do_draw_text s help_text hs vs sl el
    where hs = 0
          vs = helparea_vscroll s
          sl = helparea_startline s
          el = helparea_endline s

-- Text area

textarea_text :: Entry a => Status a -> String
textarea_text s =
    case active_entry s of
        Nothing -> []
        Just e -> maybe "" entry_text $ viewlist_entry s e

next_tab_stop pos = ((pos `div` 8) + 1) * 8

do_tab_stops pos [] = []
do_tab_stops pos ('\t':ss) = 
    replicate (nxt - pos) ' ' ++ (do_tab_stops nxt ss)
    where
        nxt = next_tab_stop pos
do_tab_stops pos (s:ss) = s:(do_tab_stops (pos + 1) ss)

do_draw_text s text hs vs sl el = do
    cset_attr (attr_text $ attr s)
    draw_lines s (map (do_tab_stops 0) $ lines text) sl el vs drawl
    creset_attr
    where
        w = screen_width s
        drawl s Nothing _ = 
            fill_to_eol
        drawl s (Just l) _ = 
            waddstr Curses.stdScr (take w $ drop hs $ l ++ repeat ' ')

do_draw_textarea s text = do
    do_draw_text s text hs vs sl el
    where
        hs = textarea_hscroll s
        vs = textarea_vscroll s
        sl = textarea_startline s
        el = textarea_endline s

draw_textarea :: Entry a => Status a -> IO ()
draw_textarea status =
    do_draw_textarea status (textarea_text status)

-- Info lines

botinfo_text :: Entry a => Status a -> String
botinfo_text s =
    case active_entry s of
        Nothing -> "--"
        Just e -> maybe "--" entry_title $ viewlist_entry s e


midinfo_text :: Entry a => Status a -> String
midinfo_text s = 
    fromMaybe "(no file)" (entries_fname s)
    ++ if unsaved_changes s then " [modified]" else ""


do_draw_infoline_align status line left right = do
    Curses.wMove Curses.stdScr line 0
    cset_attr (attr_infoline $ attr status)
    waddstr Curses.stdScr ((take n_l left_)++(take n_r right_))
    creset_attr
    where
        left_ = left ++ (repeat ' ')
        right_ = ' ':right
        w = screen_width status
        n_r = min w (length right_)
        n_l = max 0 (w-n_r)

do_draw_infoline status line text = 
    do_draw_infoline_align status line text ""

mk_n_of_m n m = 
    "("++(show n)++"/"++(show m)++") "

draw_topinfo :: Entry a => Status a -> IO ()
draw_topinfo s =
    let help_list = KeyMap.gen_tophelp_text (active_key_map s)
        help_text = concat $ intersperse ", " help_list
    in do_draw_infoline s (topinfo_line s) ((topinfo_text s) ++ help_text)

draw_topinfo_help s = draw_topinfo s -- FIXME

draw_midinfo :: Entry a => Status a -> IO ()
draw_midinfo s =
    do_draw_infoline_align s l t (mk_n_of_m n m)
    where
        l = midinfo_line s
        t = midinfo_text s
        m = length (entries_viewlist_plain s)
        n = 1 + selected_entry s
       
draw_botinfo :: Entry a => Status a -> IO ()
draw_botinfo s =
    do_draw_infoline_align s l t (mk_n_of_m n m)
    where
        l = botinfo_line s
        t = botinfo_text s
        m = text_n_lines (textarea_text s)
        n = min m $ (textarea_vscroll s)+(textarea_height s)

draw_botinfo_help :: Entry a => Status a -> String -> IO ()
draw_botinfo_help s help_text = 
    do_draw_infoline_align s l "Help" (mk_n_of_m n m)
    where
        l = botinfo_line s
        m = text_n_lines help_text
        n = min m $ (helparea_vscroll s)+(helparea_height s)
    
-- Entries

entry_attr s nr =
    case (nr == selected_entry s, Just nr == active_entry s) of
        (False, False) -> attr_entry $ attr s
        (True, False)  -> attr_entry_sel $ attr s
        (False, True)  -> attr_entry_act $ attr s
        (True, True)   -> attr_entry_act_sel $ attr s

do_draw_entry s Nothing _ = do
    cset_attr (attr_entry $ attr s)
    fill_to_eol
    creset_attr

do_draw_entry s (Just (Loc loc, e)) nr = do
    cset_attr (entry_attr s nr)
    waddstr Curses.stdScr (left_align w l)
    creset_attr
    where
        w = screen_width s
        bullet = case (entrytree_children e) of
                     [] -> " - " --[' ', Curses.bullet, ' ']
                     otherwise -> " + "
        indent = replicate (3 * (length loc - 1)) ' '
        tg = case entrytree_tagged e of
                 True -> "*"
                 False -> " "
        flags = left_align 4 (entry_flags e)
        l = concat [" ", flags, tg, " ", indent, bullet, entry_title e]

draw_entries :: Entry a => Status a -> IO ()
draw_entries s = 
    draw_lines s (entries_viewlist s) sl el vs do_draw_entry
    where
        vs = entryarea_vscroll s
        sl = entryarea_startline s
        el = entryarea_endline s

-- Refresh

redraw :: Entry a => RI a ()
redraw = do
    status <- get_status
    case mode status of
        DefaultMode -> liftIO $ do 
             --Curses.wclear Curses.stdScr
             draw_topinfo status
             draw_entries status
             draw_midinfo status
             draw_textarea status
             draw_botinfo status
             clear_message status
        HelpMode text -> liftIO $ do
             draw_topinfo_help status
             draw_helparea status text
             draw_botinfo_help status text
             clear_message status


clear_message :: Entry a => Status a -> IO ()
clear_message s = do
    cset_attr (attr_message $ attr s)
    Curses.wMove Curses.stdScr (message_line s) 0
    fill_to_eol
    Curses.refresh


refresh :: Entry a => RI a ()
refresh = do 
    redraw 
    liftIO $ Curses.refresh

-- }}}


-- Text scrolling {{{

max_scroll_ :: Int -> Int -> Int
max_scroll_ item_s view_s =
    max 0 (item_s - view_s)

text_max_hscroll :: String -> (Int, Int) -> Int
text_max_hscroll text (_, view_w) =
    max_scroll_ (text_n_columns text) view_w

text_max_vscroll :: String -> (Int, Int) -> Int
text_max_vscroll text (view_h, _) =
    max_scroll_ (text_n_lines text) view_h

textarea_size :: Entry a => Status a -> (Int, Int)
textarea_size s =
    (textarea_height s, screen_width s)

helparea_size :: Entry a => Status a -> (Int, Int)
helparea_size s =
    (helparea_height s, screen_width s)

calc_scroll s asc csc_fn msc_fn text size = 
    max 0 (min msc (csc + asc))
    where
        msc = msc_fn text size
        csc = csc_fn s

do_hscroll :: Entry a => Int -> Status a -> Status a
do_hscroll amount s = 
    s{textarea_hscroll = sc}
    where
        sc = calc_scroll s amount textarea_hscroll text_max_hscroll
               (textarea_text s) (textarea_size s)

do_vscroll :: Entry a => Int -> Status a -> Status a
do_vscroll amount s = 
    s{textarea_vscroll = sc}
    where
        sc = calc_scroll s amount textarea_vscroll text_max_vscroll
               (textarea_text s) (textarea_size s)

do_vscroll_help :: Entry a => Int -> Status a -> Status a
do_vscroll_help amount s = 
    s{helparea_vscroll = sc}
    where
        sc = calc_scroll s amount helparea_vscroll text_max_vscroll
               text (helparea_size s)
        text = case mode s of
                 HelpMode t -> t
                 _ -> ""

max_vscroll :: Entry a => Status a -> Int
max_vscroll s =
    text_max_vscroll (textarea_text s) (textarea_height s, screen_width s)

max_hscroll :: Entry a => Status a -> Int
max_hscroll s =
    text_max_hscroll (textarea_text s) (textarea_height s, screen_width s)

check_vscroll :: Entry a => Status a -> Status a
check_vscroll s =
    case textarea_vscroll s > max_vscroll s of
        True -> s{textarea_vscroll = max_vscroll s}
        False -> s

check_hscroll :: Entry a => Status a -> Status a
check_hscroll s =
    case textarea_hscroll s > max_hscroll s of
        True -> s{textarea_hscroll = max_hscroll s}
        False -> s

check_textarea :: Entry a => Status a -> Status a
check_textarea = check_vscroll . check_hscroll

-- }}}


-- Entry selection {{{

check_active :: Entry a => Status a -> Status a
check_active s =
    case active_entry s of
        Just e | e >= n_entries_viewed s -> s{active_entry = Nothing}
        otherwise -> s
        
check_selected :: Entry a => Status a -> Status a
check_selected s =
    case selected_entry s >= n_entries_viewed s of
        True -> s{selected_entry = max 0 $ (n_entries_viewed s) - 1}
        false -> s

check_e_vscroll :: Entry a => Status a -> Status a
check_e_vscroll s =
    do_selectentry (selected_entry s) s

check_entryarea :: Entry a => Status a -> Status a
check_entryarea s = check_e_vscroll $ check_selected $ check_active s

check_everything :: Entry a => Status a -> Status a
check_everything = check_textarea . check_entryarea

do_selectentry n s | valid_entry s n =
    if n > l_e then
        s{selected_entry = n, entryarea_vscroll = n - h + 1}
    else if n < f_e then
        s{selected_entry = n, entryarea_vscroll = n}
    else
        s{selected_entry = n}
    where
        n_entries = length (entries_viewlist s)
        h = entryarea_height s
        f_e = entryarea_vscroll s
        l_e = f_e + h - 1
do_selectentry 0 s | n_entries_viewed s == 0 =
    s{selected_entry = 0, entryarea_vscroll = 0}
do_selectentry _ _ = error "Invalid entry"

do_actentry e s | valid_entry s e =
    s{active_entry = Just e, textarea_vscroll = 0, textarea_hscroll = 0}

-- }}}


-- Expand/collapse {{{

update_entries s et = 
    s{entries = et, active_entry = ae, selected_entry = se}
    where
        ae = mpass (find_viewlist_pos et) (active_entry_loc s)
        se = find_viewlist_pos et (selected_entry_loc s)

do_colexp_loc :: Entry a =>
    ([EntryTree a] -> Loc -> Maybe [EntryTree a]) -> Status a -> Loc
    -> Status a
do_colexp_loc fn s loc =
    case fn (entries s) loc of
        Nothing -> s
        Just et -> check_everything $ update_entries s et

do_colexp :: Entry a =>
    ([EntryTree a] -> Loc -> Maybe [EntryTree a]) -> Status a -> Int
    -> Status a
do_colexp fn s e = do_colexp_loc fn s $ viewlist_entry_loc s e

do_expand :: Entry a => Status a -> Int -> Status a
do_expand s e | valid_entry s e =
    do_colexp entrytree_expand s e


do_collapse :: Entry a => Status a -> Int -> Status a
do_collapse s e | valid_entry s e =
    do_colexp entrytree_collapse s e


do_collapse_p :: Entry a => Status a -> Int -> Status a
do_collapse_p s e | valid_entry s e =
    do_colexp entrytree_collapse_p s e

-- }}}


-- Snapshotting {{{

mk_snapshot :: Entry a => Status a -> UndoInfo a
mk_snapshot s =
    UndoInfo (selected_entry s) (active_entry s)
             (unsaved_changes s) (entries s)
    
snapshot :: Entry a => Status a -> Status a
snapshot s =
    s{undo_buffer = nub, redo_buffer=[], unsaved_changes=True}
    where
        (rb, ub) = (redo_buffer s, undo_buffer s)
        nub = take undo_count $ (mk_snapshot s):(rb ++ reverse rb ++ ub)

-- }}}


-- Entry and entry tree manipulation {{{

rm_new_loc_vl [] rmlocv = Nothing
rm_new_loc_vl ((loc, _):vv) rmlocv =
    case loc_rm_effect loc rmlocv of
        Nothing -> rm_new_loc_vl vv rmlocv
        just_loc -> just_loc
    
rm_new_loc :: Entry a => Status a -> [Loc] -> Int -> Maybe Loc
rm_new_loc s rmlocv e =
    case loc_rm_effect loc rmlocv of
        Nothing -> 
            case rm_new_loc_vl vl_after rmlocv of
                Nothing -> rm_new_loc_vl vl_before rmlocv
                just_loc -> just_loc
        just_loc -> just_loc
    where
        loc@(Loc loc_) = viewlist_entry_loc s e
        vl_after = drop (e+1) $ entries_viewlist s
        vl_before = reverse $ take e $ entries_viewlist s

rm_get_new_entry :: Entry a =>
    Status a -> [Loc] -> [EntryTree a] -> Int -> Maybe Int
rm_get_new_entry s rmlocv etn e =
    mpass (find_viewlist_pos etn) (rm_new_loc s rmlocv e)

do_delete :: Entry a => Status a -> [Loc] -> Status a
do_delete s rmlocv =
    check_everything (snapshot s){entries = etn, 
                                  selected_entry = nsel,
                                  active_entry = nact}
    where
        et = entries s
        etn = entrytree_remove et rmlocv
        gn = rm_get_new_entry s rmlocv etn
        nsel = fromMaybe 0 $ gn (selected_entry s)
        nact = maybe Nothing gn (active_entry s)


mv_get_new_entry :: Entry a =>
    Status a -> InsertWhere -> [Loc] -> [EntryTree a] -> Int -> Maybe Int
mv_get_new_entry s insw mvlocv etn e =
    mpass (find_viewlist_pos etn) 
          $ mpass (\l -> loc_ins_effect l insw (length mvlocv))
                  (rm_new_loc s mvlocv e)

do_move :: Entry a => 
    Status a -> InsertWhere -> [Loc] -> Status a
do_move s insw mvlocv =
    check_everything (snapshot s){entries = etn, 
                                  selected_entry = nsel,
                                  active_entry = nact}
    where
        et = entries s
        etn = fst $ entrytree_move et insw mvlocv
        gn = mv_get_new_entry s insw mvlocv etn
        nsel = fromMaybe 0 $ gn (selected_entry s)
        nact = maybe Nothing gn (active_entry s)

do_delentry s e =
    do_delete s [viewlist_entry_loc s e]

do_move_tagged :: Entry a => Status a -> InsertWhere -> Status a
do_move_tagged s insw =
    do_clear_tags $ do_move s insw tagged
    where
        tagged = entrytree_get_tagged (entries s)

do_tag s act e =
    maybe s (\nent_ -> s{entries = nent_}) etn
    where
        etn = entrytree_tag (entries s) (viewlist_entry_loc s e) act

do_clear_tags s =
    maybe s (\nent_ -> s{entries = nent_}) etn
    where
        etn = entrytree_clear_tags (entries s)


-- }}}


-- getCh {{{

getCh :: IO (Maybe Curses.Key)
getCh = do
    v <- Curses.getch
    return $ case v of
        (-1) -> Nothing
        k_ -> Just $ Curses.decodeKey k_

get_key_ :: IO (Maybe Curses.Key)
get_key_ = do
    Control.Exception.catch (Curses.cBreak True) (\_ -> return ())
    getCh

get_key :: Entry a => RI a () -> RI a Curses.Key
get_key refresh_fn = do
    k <- liftIO $ get_key_
    case k of
      Nothing -> get_key refresh_fn
      Just Curses.KeyResize -> do
          mod_status_io get_size
          refresh_fn 
          get_key refresh_fn
      Just k -> return k

-- }}}
