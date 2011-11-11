--
-- riot/Riot/ConfigAPI.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module Riot.ConfigAPI (
    module Curses.Curses,
    module Riot.UI,
    module Riot.Version,
    module Riot.Style,
    module Riot.Entry,
    dflt_settings,
) where

import Curses.Curses ( Key(..) )
import Riot.UI
import Riot.Version
import Riot.Style
import Riot.Entry
import Riot.Riot
import Riot.Query
import qualified Riot.Actions as A

--
-- provide a default implementation for the settings
--
dflt_settings :: Config
dflt_settings = Config {
        cfg_key_map = dflt_key_map,
        cfg_help_key_map = dflt_help_key_map,
        cfg_query_key_map = dflt_query_key_map,
        cfg_topinfo_text = dflt_topinfo_text,
        cfg_styles = dflt_styles,
        cfg_save_always = dflt_save_always,
        cfg_encoding = dflt_encoding,
        cfg_backup_numbers = dflt_backup_numbers
    }

-- defaults

dflt_key_map = [ (KeyChar 'q',     A.quit)
               , (KeyChar '\^R',   A.refresh)
               , (KeyChar '\^G',   A.clear_message)
               , (KeyChar '?',     A.help)
               , (KeyChar '.',     A.scroll_text_right)
               , (KeyChar ',',     A.scroll_text_left)
               , (KeyChar ' ',     A.scroll_text_down)
               , (KeyChar 'b',     A.scroll_text_up)
               , (KeyChar 'u',     A.scroll_entry_up)
               , (KeyChar 'v',     A.scroll_entry_down)
               , (KeyPPage,        A.scroll_entry_up)
               , (KeyNPage,        A.scroll_entry_down)
               , (KeyHome,         A.goto_first_entry)
               , (KeyEnd,          A.goto_last_entry)
               , (KeyChar 'n',     A.goto_next_entry)
               , (KeyChar 'p',     A.goto_prev_entry)
               , (KeyUp,           A.goto_prev_entry)
               , (KeyDown,         A.goto_next_entry)
               , (KeyEnter,        A.show_current_entry)
               , (KeyChar '\^M',   A.show_current_entry)
               , (KeyChar 'd',     A.delete_current_entry)
               , (KeyChar 'e',     A.edit_current_entry)
               , (KeyBackspace,    A.collapse_tree)
               , (KeyChar 'a',     A.create_new_entry)
               , (KeyChar 'r',     A.create_new_child_entry)
               , (KeyChar '\^_',   A.undo)
               , (KeyChar '\^^',   A.redo)
               , (KeyChar 't',     A.toggle_tag)
               , (KeyChar 'k',     Submap dflt_k_map)
               , (KeyChar ';',     Submap dflt_tag_map)
               , (KeyChar '/',     A.search)
               , (KeyChar 'w',     A.write)
               ]

dflt_k_map = [ (KeyChar 'u',   A.goto_first_entry)
             , (KeyChar 'v',   A.goto_last_entry)
             , (KeyChar 'd',   A.save)
             , (KeyChar 'j',   Submap dflt_tag_map)
             ]

dflt_tag_map = [ (KeyChar 't',     A.clear_tags)
               , (KeyChar 'a',     A.move_tagged_after)
               , (KeyChar 'b',     A.move_tagged_before)
               , (KeyChar 'r',     A.move_tagged_under)
               , (KeyChar 'd',     A.delete_tagged)
               ]

dflt_help_key_map = [ (KeyChar 'q',     A.quit_help)
                    , (KeyChar ' ',     A.scroll_help_down)
                    , (KeyChar 'b',     A.scroll_help_up)
                    ]

--
-- Queries
--


dflt_query_key_map = [ (KeyChar '\^F',  q_next)
                     , (KeyChar '\^B',  q_prev)
                     , (KeyChar '\^A',  q_bol)
                     , (KeyChar '\^E',  q_eol)
                     , (KeyChar '\^J',  q_deleol)
                     , (KeyBackspace,   q_delprev)
                     , (KeyChar '\^D',  q_delnext)
                     , (KeyChar '\^Y',  q_delline)
                     , (KeyEnter,       q_done)
                     , (KeyChar '\^M',  q_done)
                     , (KeyChar '\^G',  q_cancel)
                     , (KeyChar '\^C',  q_cancel)
                     ]


--
-- Help
--

dflt_topinfo_text = package++" "++version
               ++" ** "

--
-- Styles
--

dflt_styles = [
     StyleSpec ("attr_infoline", (a_bold, c_green, c_blue)),
     StyleSpec ("attr_text",     (a_none, c_white, c_black)),
     StyleSpec ("attr_entry",    (a_none, c_white, c_black)),
     StyleSpec ("attr_entry_act",(a_bold, c_white, c_black)),
     StyleSpec ("attr_entry_sel",(a_reverse, c_cyan, c_black)),
     StyleSpec ("attr_entry_act_sel",(a_reverse, c_cyan, c_black)),
     StyleSpec ("attr_message",  (a_bold, c_white, c_black)),
     StyleSpec ("attr_error",    (a_bold, c_red, c_black))
     ]

--
-- Options
---

dflt_save_always = False

dflt_encoding = Enc "utf-8"

dflt_backup_numbers = 0
