--
-- riot/Riot/KeyMap.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module Riot.KeyMap where

import Riot.Riot
import Curses.Curses (Key(..))
import Char

key2str :: Key -> String
key2str (KeyChar ' ') = "<Space>"
key2str (KeyChar '\t') = "<Tab>"
key2str (KeyChar c) | isPrint c = [c]
key2str (KeyChar c) = show c
key2str KeyDown = "<Down>"
key2str KeyUp = "<Up>"
key2str KeyLeft = "<Left>"
key2str KeyRight = "<Right>"
key2str KeyHome = "<Home>"
key2str KeyBackspace = "<BS>"
key2str (KeyF i) = 'F' : show i
key2str KeyNPage = "<NPage>"
key2str KeyPPage = "<PPage>"
key2str KeyEnter = "<Return>"
key2str KeyEnd = "<End>"
key2str k = show k

isAbort (KeyChar '\a') = True
isAbort _ = False

type FlatKeyMap = [([Key], Action)]

flatten_keymap :: [Key] -> KeyMap -> FlatKeyMap
flatten_keymap _ [] = []
flatten_keymap keys ((key, Submap sm) : rest) =
    flatten_keymap (keys ++ [key]) sm ++ flatten_keymap keys rest
flatten_keymap keys ((key, a) : rest) =
    (keys ++ [key], a) : flatten_keymap keys rest

keylist2str :: [Key] -> String
keylist2str [] = []
keylist2str (k:ks) = key2str k ++ keylist2str ks

gen_help_text :: KeyMap -> [String]
gen_help_text km =
    let flat_map = flatten_keymap [] km
        str_map = map (\ (ks, a) -> (keylist2str ks, action2str a)) flat_map
        maxlen = maximum (map (length . fst) str_map)
        spaces = 4
        f (k, s) = (take (maxlen+spaces) (k ++ repeat ' ')) ++ s
    in map f str_map
    where action2str :: Action -> String
          action2str (Action s _) = s
          action2str (TopAction _ s _) = s
          action2str _ = ""

gen_tophelp_text :: KeyMap -> [String]
gen_tophelp_text km =
    let flat_map = flatten_keymap [] km
        top_map = filter (\(_, a) -> case a of
                                       TopAction _ _ _ -> True
                                       _ -> False) flat_map
        str_map = map (\ (ks, a) -> (keylist2str ks, action2str a)) top_map
    in map (\(k,s) -> k++":"++s) str_map
    where action2str (TopAction s _ _) = s
          action2str _ = ""
