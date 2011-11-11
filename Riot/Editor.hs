--
-- riot/Riot/Editor.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--


module Riot.Editor(
    edittext,
    editfile,
    get_editor
)where

import System(getEnv)
import System.Posix.Temp(mkstemp)
import System.Posix.Files(removeLink)
import System.Posix.Unistd
import System.Cmd(system)
import Riot.Version(package)
import IO


shell_safe_string s =
    concat ["'", escape s, "'"]
    where
        escape [] = []
        escape ('\'':s) = "\\'"++escape s
        escape (x:s) = x:escape s

my_system cmd args =
    system $ concat (cmd:map (\s -> " "++shell_safe_string s) args)


--import Config(preferred_editor, fallback_editor)
preferred_editor = Nothing -- Use environment
fallback_editor = "vi" -- If environment fails

get_editor = 
    case preferred_editor of
        Just e -> return e
        Nothing -> catch (getEnv "VISUAL")
                         (\_ -> catch (getEnv "EDITOR")
                                      (\_ -> return fallback_editor))


editfile fname = do
    editor <- get_editor
    my_system editor [fname]

make_temp = do
    mkstemp $ "/tmp/"++package++"-XXXXXX"

edittext text = do
    (fname, h) <- make_temp
    catch (do_edittext fname h text)
          (\e -> finish fname h >> ioError e)
    where
        finish fname h = hClose h >> removeLink fname
        do_edittext fname h text = do
            hPutStr h text
            hClose h
            editfile fname
            txt <- readFile fname
            removeLink fname
            return txt
