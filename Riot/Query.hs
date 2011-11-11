--
-- riot/Riot/Query.hs
-- 
-- Copyright (c) Tuomo Valkonen 2006.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module Riot.Query(
    message,
    do_message,
    clear_message,
    yesno,
    query,
    e_search,
    e_write,
    -- Query mode actions
    QAct, QKeyMap,
    q_delnext, q_delprev, q_deleol, q_delbol, q_delline,
    q_next, q_prev, q_eol, q_bol,
    q_cancel, q_done, q_insert
) where

import qualified Curses.Curses as Curses
import qualified Control.Exception
import Control.Monad(liftM)
import Data.Char(isPrint, toLower)
import Data.List(tails)
import Data.Maybe(fromJust)

import Riot.Style(UIAttr(..), init_uiattr, default_uiattr, StyleSpec)
import Riot.UI
import Riot.Entry
import Riot.Riot
import Riot.Locale

import qualified Riot.KeyMap as KeyMap


-- Message {{{

do_message :: Entry a =>
    Status a -> (UIAttr -> (Curses.Attr, Curses.Pair)) -> String -> IO ()
do_message s attr_fn msg = do
    Curses.wMove Curses.stdScr (message_line s) 0
    cset_attr (attr_fn $ attr s)
    waddstr Curses.stdScr $ take (screen_width s) $ msg ++ repeat ' '
    creset_attr
    Curses.refresh

message :: Entry a => Status a -> String -> IO ()
message s msg = do_message s attr_message msg

msg m = with_status_io (\s -> message s m)

-- }}}


-- yes/no {{{

get_yn yes no cancel refresh_fn = do
    k <- get_key refresh_fn
    case k of
        (Curses.KeyChar 'y') -> return yes
        (Curses.KeyChar 'Y') -> return yes
        (Curses.KeyChar 'n') -> return no
        (Curses.KeyChar 'N') -> return no
        _ -> if KeyMap.isAbort k
                 then return cancel
                 else do liftIO $ Curses.beep 
                         get_yn yes no cancel refresh_fn
yesno :: Entry a => 
    String
    -> RI a ELCont
    -> RI a ELCont
    -> RI a ELCont
yesno msg yes no = do
    status <- get_status
    liftIO $ draw_yesno msg status
    action <- get_yn yes no cont refresh_fn
    with_status_io clear_message
    action
    where
        draw_yesno msg s = do
            message s (msg++" [y/n/^G]")
        refresh_fn = do 
             redraw 
             with_status_io $ draw_yesno msg

-- }}}


-- query routine {{{


get_query :: Entry a => String -> Int -> RI a QResult
get_query str point = do
    km <- liftM cfg_query_key_map get_settings
    k_ <- liftIO $ get_key_
    return $ case k_ of
        Nothing -> unmodified
        Just Curses.KeyResize -> QResize
        Just k -> case lookup k km of
            Just act -> act str point
            Nothing -> case k of
                Curses.KeyChar k | isPrint k -> q_insert k str point
                _ -> unmodified
    where
        unmodified = QModified str point


query :: Entry a =>
    String                      -- Prompt
    -> Maybe String             -- Default
    -> (String -> RI a ELCont)  -- Handler
    -> RI a ELCont              -- Cancel handler
    -> RI a ELCont
query prompt_ dflt ok_cont cancel_cont = 
    ri_bracket (liftIO $ Curses.cursSet Curses.CursorVisible)
	       (\_ -> liftIO $ Curses.cursSet Curses.CursorInvisible)
               (\_ -> loop "" 0 0)
    where
        prompt = case dflt of
                     Nothing -> prompt_ ++ ": "
                     Just d -> prompt_ ++ " [" ++ d ++ "]:"
        pl = length prompt
    
        draw_query prompt msg vstart point s = do
            let str = prompt ++ (drop vstart msg)
                curs = pl + (point - vstart)
            Curses.wMove Curses.stdScr (message_line s) 0
            cset_attr (attr_message $ attr s)
            waddstr Curses.stdScr $ take (screen_width s) $ str ++ repeat ' '
            creset_attr
            Curses.wMove Curses.stdScr (message_line s) curs
            Curses.refresh
        
        loop str vstart point = do
            with_status_io $
                draw_query prompt str vstart point
            action <- get_query str point 
            case action of
                QDone res -> do
                    with_status_io clear_message 
                    ok_cont $ case (res, dflt) of
                                  ([], Just x) -> x
                                  _            -> res
                QCancel -> do
                    with_status_io clear_message 
                    cancel_cont 
                QModified nstr npoint -> do
                    nvstart <- update_vstart nstr vstart npoint
                    loop nstr nvstart npoint
                QResize -> do
                    nvstart <- update_vstart str vstart point
                    redraw
                    loop str nvstart point
                    
        update_vstart str vstart npoint = do
            s <- get_status
            let spc = (screen_width s) - pl - 1
                nvstart = max (npoint - spc) (min vstart npoint)
            return $ nvstart

-- }}}


-- Query actions {{{

-- TODO: These things and more should really be in a general-purpose
-- library.

q_delprev :: QAct
q_delprev str 0 = QModified str 0
q_delprev str pt = QModified (a ++ drop 1 b) (pt - 1)
    where
        (a, b) = splitAt (pt - 1) str

q_delnext :: QAct
q_delnext str pt = QModified (a ++ drop 1 b) pt
    where
        (a, b) = splitAt pt str

q_delline :: QAct
q_delline str pt = QModified "" 0

q_deleol :: QAct
q_deleol str pt = QModified (take pt str) pt

q_delbol :: QAct
q_delbol str pt = QModified (drop pt str) 0


q_bol :: QAct
q_bol str pt = QModified str 0

q_eol :: QAct
q_eol str pt = QModified str (length str)

q_prev :: QAct
q_prev str pt = QModified str $ max 0 (pt - 1)

q_next :: QAct
q_next str pt = QModified str $ min (length str) (pt + 1)

q_cancel :: QAct
q_cancel str pt = QCancel

q_done :: QAct
q_done str pt = QDone str

q_insert :: Char -> QAct
q_insert ch str pt = QModified (a ++ ch:b) (pt + 1)
    where
        (a, b) = splitAt pt str

-- }}}


-- Search query {{{

maybe_do f []  = cont
maybe_do f str = f str


e_search :: Entry a => RI a ELCont
e_search = do
    ps <- liftM previous_search get_status
    query "Search for" ps (maybe_do do_search) cont


do_search str = do
    mod_status (\s -> s{previous_search = Just str})
    et <- liftM entries get_status
    sloc <- liftM selected_entry_loc get_status
    let scan_entry l res e | l <= sloc =
            case (res, is_match e) of
                ((Nothing, x), True) -> (Just l, x)
                _                    -> res
                
        scan_entry l res e | l > sloc =
            case (res, is_match e) of
                ((x, Nothing), True) -> (x, Just l)
                _                    -> res
        
        xform = map toLower        
        fnd = has_substring (==)
        
        xstr = xform str
        
        is_match e = (fnd (xform $ entry_title e) xstr) 
                   || (fnd (xform $ entry_text e) xstr)

        loc = case entrytree_fold_loc scan_entry (Nothing, Nothing) et of
                  (_, l@(Just _)) -> l
                  (l@(Just _), _) -> l
                  _               -> Nothing

    case loc of
        Nothing -> msg "Search string not found." >> cont
        Just loc -> do
            let etn = entrytree_expand_ et (loc_above loc)
                pos = find_viewlist_pos etn loc
            mod_status (\ss -> check_everything $
                               do_actentry pos $
                               do_selectentry pos ss{entries = etn})
            msg "Found."
            refresh
            cont

has_substring eq str sub = any (eq sub) subs
    where subs = map (take $ length sub) (tails str)

-- }}}


-- Write out query {{{

e_write :: Entry a => RI a ELCont
e_write = do
    query "Write in file" Nothing (maybe_do do_write) cont

do_write str = do
    (_, ent) <- with_status (\s -> fromJust $ lnth (selected_entry s) (entries_viewlist s))
    liftIO $ do
        text <- to_locale $ entry_text ent
        writeFile str text
    msg ("Wrote entry in " ++ str)
    refresh
    cont

-- }}}
