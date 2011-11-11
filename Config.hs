--
-- riot/Config.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--


module Config (riot) where

import Riot.ConfigAPI
import Riot.Riot

-- To use the default settings, set riot to `dflt_settings'.
-- To use your customized settings, set riot to `custom_settings'.
riot = dflt_settings
-- riot = custom_settings

custom_settings = 
    dflt_settings
    { 
      -- Place your settings here
      cfg_save_always = True,
      cfg_encoding = Enc "iso-8859-1",
      -- Setting the number of backups to n will overwrite
      -- the files x.0, ..., x.(n-1) where x is riot's
      -- mbox file.
      cfg_backup_numbers = 5
    }
