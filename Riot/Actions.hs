--
-- riot/Riot/Actions.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module Riot.Actions where

import Riot.Entry (TagAction(..))
import Riot.Riot(Action(..))
import Riot.Event
import Riot.Query

quit                   = TopAction "quit" "quit riot" e_quit
refresh                = Action "clear and redraw the screen" e_refresh
help                   = TopAction "help" "display the help screen" e_help
scroll_text_right      = Action "scroll text right" (e_hscroll 2)
scroll_text_left       = Action "scroll text left" (e_hscroll (-2))
scroll_text_down       = Action "scroll text down" e_pgdn
scroll_text_up         = Action "scroll text up" e_pgup
scroll_entry_up        = Action "scroll entry up" e_pgupentry
scroll_entry_down      = Action "scroll entry down" e_pgdnentry
goto_first_entry       = Action "goto first entry" e_firstentry
goto_last_entry        = Action "goto last entry" e_lastentry
goto_next_entry        = Action "goto next entry" e_nextentry
goto_prev_entry        = Action "goto previous entry" e_preventry
show_current_entry     = Action "show current entry" e_actentry
delete_current_entry   = Action "delete current entry" e_delentry
edit_current_entry     = Action "edit current entry" e_editentry
collapse_tree          = Action "collapse tree" e_collapse
create_new_entry       = TopAction "new" "create new entry" e_newentry
create_new_child_entry = TopAction "new child" "create new child entry" 
                           e_newunder
undo                   = Action "undo" e_undo
redo                   = Action "redo" e_redo
toggle_tag             = Action "toggle tag of current entry" (e_tag TagToggle)
save                   = TopAction "save" "save" e_save
clear_tags             = Action "clear tags" e_clear_tags
move_tagged_after      = Action "move tagged entries after current entry" 
                           e_move_tagged_after
move_tagged_before     = Action "move tagged entries before current entry" 
                           e_move_tagged_before
move_tagged_under      = Action "move tagged entries under current entry" 
                           e_move_tagged_under
delete_tagged          = Action "delete tagged entries" e_delete_tagged
quit_help              = TopAction "exit help" "exit help screen" e_quit_help
scroll_help_down       = TopAction "down" "scroll help down" e_help_down
scroll_help_up         = TopAction "up" "scroll help up" e_help_up
clear_message          = Action "clear message line" e_clear_message

search                 = Action "search for text" e_search
write                  = Action "write a copy of entry to file" e_write
