--
-- riot/Riot/Locale.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2006.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module Riot.Locale(
    to_locale,
    from_locale
) where


import Foreign.C.String(withCString, peekCString)
import Ginsu.CWString(withLCString, peekLCString)


#ifdef CF_CHARSET_SUPPORT

to_locale str = withLCString str (\s -> peekCString s)
from_locale str = withCString str (\s -> peekLCString s)

#else

to_locale str = return str
from_locale str = return str

#endif
