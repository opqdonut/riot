--
-- riot/Riot/MBox.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--


module Riot.MBox(
    MBoxEntry,
    read_mbox,
    read_mbox_entrytree,
    show_mbox,
    show_mbox_entrytree,
    show_mbox_entrytree_each,
    new_mboxentry,
    mboxentry_msgid,
    mboxentry_inreplyto,
    thread_mbox
)where

-- Imports {{{

import Char
import Maybe
import Time
import System.Locale(defaultTimeLocale)
import List(sortBy)
import Data.Map(Map, fromList, lookup)
import Data.Unique
import Control.Monad(liftM)

import Riot.Version(version)
import Riot.Entry
import Riot.Riot

import Control.Exception
import List

#ifdef CF_CHARSET_SUPPORT
import IConv
#endif

#if 1
import Data.PackedString(PackedString, packString, nilPS, unpackPS)
#else
type PackedString = String
packString = id
unpackPS = id
nilPS = []
#endif

-- }}}


-- {{{ MBoxEntry


data MBoxEntry = MBoxEntry {
    headers_p, body_p :: !PackedString
}

headers = unpackPS . headers_p
body = unpackPS . body_p


instance Entry MBoxEntry where
    entry_title m =
        strip $ first_nonempty_line $ entry_text m
    entry_text m =  s ++ b
        where
            s = case (use_subject m, get_header "Subject:" $ headers m) of
                    (True, Just s) -> s ++ "\n\n"
                    _              -> ""
            b = decode (charset m) (body m)
    entry_flags m = ""

instance EditableEntry MBoxEntry where
    entry_set_text m txt tm = do
        enc <- liftM cfg_encoding get_settings
        let stm              = calendarTimeToString tm
            subject          = strip $ first_nonempty_line txt
       	    (body_enc, ct)   = encode enc (ensure_not_empty txt)
       	    h = set_headers (headers m) [("Subject:", subject),
                                         ("Content-Type:", ct),
                                         ("X-Riot-Version:", version),
                                         ("X-Riot-Edited:", stm)]
        return m{headers_p = packString h, body_p = packString body_enc}

instance Show MBoxEntry where
    show m = (headers m) ++ "\n\n" ++ (fix_body . body $ m)

instance Read MBoxEntry where
    readsPrec _ s = case do_read_mboxentry ('\n':s) of
                        Nothing -> []
                        Just p -> [p]

mk_msgid :: CalendarTime -> Unique -> String
mk_msgid tm u =
    -- append @riot to the message id so that some mail clients (namely
    -- mutt) recognizes it.
    "<riot."++stm++"."++(show $ hashUnique u)++"@riot.invalid>"
    where
        stm = formatCalendarTime defaultTimeLocale "%Y%m%d%H%M%S" tm

new_mboxentry :: Entry a => String -> CalendarTime -> RI a MBoxEntry
new_mboxentry txt tm = do
    u <- liftIO newUnique
    entry_set_text (mboxentry_tmpl tm u) txt tm
    
mboxentry_tmpl tm u = 
    MBoxEntry{ 
        headers_p = packString h, 
        body_p = nilPS 
    }
    where
        stm = calendarTimeToString tm
        h = "From background-static " ++ stm ++
            "\nMessage-Id: " ++ (mk_msgid tm u) ++
            "\nDate: " ++ stm ++
            "\nStatus: RO" ++
            "\nX-Riot-Version: " ++ version ++
            "\nFrom: Riot"

-- }}}


-- Misc. {{{

-- Strips leading and trailing whitespace 
-- stripped
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

first_nonempty_line s =
    reverse $ dropWhile isSpace $ reverse
            $ takeWhile (\c -> c /= '\n') $ dropWhile isSpace s
                
                
fix_body [] = []
fix_body s@('\n':'\n':[]) = s
fix_body s@('\n':[]) = '\n':s
fix_body (c:[]) = c:"\n\n"
fix_body ('\n':'F':'r':'o':'m':' ':s) = '\n':'>':'F':'r':'o':'m':' ':(fix_body s)
fix_body (s:ss)= s:(fix_body ss)

ensure_not_empty [] = "\n"
ensure_not_empty s  = s

use_subject m = isNothing $ get_header "X-Riot-Version:" (headers m)

-- }}}


-- Read {{{

-- Find 'From'
do_read_mboxentry (' ':s) = do_read_mboxentry s
do_read_mboxentry ('\t':s) = do_read_mboxentry s
do_read_mboxentry ('\n':s@('F':'r':'o':'m':' ':_)) =
    case do_read_headers_r "" s of
        Nothing -> Nothing
        Just (h_r, s_) -> Just (MBoxEntry { 
                                    headers_p = packString (reverse h_r), 
                                    body_p = packString (reverse b_r) 
                                }, s__)
                          where
                              (b_r, s__) = do_read_body_r "" s_

do_read_mboxentry _ = Nothing

do_read_headers_r got ('\n':'\n':left) = Just (got, left)
do_read_headers_r got (s:ss) = do_read_headers_r (s:got) ss
do_read_headers_r _ _ = Nothing

do_read_body_r got ('\n':ss@('F':'r':'o':'m':' ':_)) = ('\n':got, ss)
do_read_body_r got ('\n':'>':'F':'r':'o':'m':' ':ss) = 
    do_read_body_r (" morF\n"++got) ss
do_read_body_r got [] = (got, [])
do_read_body_r got (s:ss) = do_read_body_r (s:got) ss

read_mbox :: String -> [MBoxEntry]
read_mbox s =
    case do_read_mboxentry ('\n':s) of
        Nothing -> case null s of
                       True -> []
                       False -> error $ "MBox parse error."
        Just (e, s_) -> e:(read_mbox s_)
        

read_mbox_entrytree :: String -> [EntryTree MBoxEntry]
read_mbox_entrytree s =
    thread_mbox $ read_mbox s

is_header f s = (map toLower $ take (length f) s) == (map toLower f)

get_header f l = do
    h <- get_header_ f l
    return $ strip h

-- Variant of get_header where leading and trailing whitespace is not
-- stripped from the header's content.
get_header_ f [] = Nothing
get_header_ f ('\n':s) | is_header f s =
    let content = drop (length f) $ reverse $ fst $ get_header_content [] s
        in case content of
              (' ':xs) -> return xs
              s        -> return s
get_header_ f (s:ss) = get_header_ f ss    

get_header_content acc ('\n':'\t':ss) = get_header_content ('\t':'\n':acc) ss
get_header_content acc sss@('\n':ss) = (acc, sss)
get_header_content acc (s:ss) = get_header_content (s:acc) ss
get_header_content acc [] = (acc, [])

set_header f [] fnew = '\n':f++" "++fnew
set_header f ('\n':s) fnew | is_header f s =
    '\n':f++" "++fnew++(snd $ get_header_content [] s)
set_header f (s:ss) fnew = s:(set_header f ss fnew)

set_headers hdrs key_val = 
    foldr (\(k, v) h -> set_header k h v) hdrs key_val

-- }}}


-- Show {{{

set_inreplyto m id =
    m{headers_p = packString $ set_header "In-Reply-To:" (headers m) id}

show_mbox :: [MBoxEntry] -> String
show_mbox mm = concat $ map show mm

show_mbox_entrytree :: [EntryTree MBoxEntry] -> String
show_mbox_entrytree = concat . show_mbox_entrytree_each

show_mbox_entrytree_each :: [EntryTree MBoxEntry] -> [String]
show_mbox_entrytree_each et =
    concatMap (do_show_mbox_entrytree_each Nothing) et

do_show_mbox_entrytree_each :: Maybe String -> EntryTree MBoxEntry -> [String]
do_show_mbox_entrytree_each parid e =
    (show m):concatMap (do_show_mbox_entrytree_each id) ch
    where
        m_ = entrytree_thisentry e
        m = case (parid, mboxentry_inreplyto m_ == parid) of
		(Just p, False) -> set_inreplyto m_ p
		otherwise -> m_
	id = mboxentry_msgid m
        ch = entrytree_children e


-- }}}


-- Threading {{

parse_msgid ('<':ss) =
    case rest of
        ('>':_) -> Just ('<':id++">")
        otherwise -> Nothing
    where
        (id, rest) = span (\c -> c/='>') ss
parse_msgid _ = Nothing

mboxentry_msgid :: MBoxEntry -> Maybe String
mboxentry_msgid m = 
    maybe Nothing parse_msgid 
          $ get_header "Message-Id:" (headers m)

mboxentry_inreplyto :: MBoxEntry -> Maybe String
mboxentry_inreplyto m = 
    maybe Nothing parse_msgid 
          $ get_header "In-Reply-To:" (headers m)


make_fm :: [(Int, Maybe String, Maybe String)] 
        -> Map String (Int, Maybe String, Maybe String)
make_fm l = 
    (\ xs -> assert (let is = map fst xs in is == nub is) (fromList xs)) $ catMaybes $ map f l
    where
        f s@(_, Just id, _) = Just (id, s)
	f _ = Nothing

get_ir _ Nothing = Nothing
get_ir fm (Just ir) = Data.Map.lookup ir fm

idpath fm (i, _, ir) acc =
   case get_ir fm ir of
       Nothing -> (i:acc)
       Just ird -> idpath fm ird (i:acc)

compare_idp fm (_, s1) (_, s2) =
    compare (idpath fm s1 []) (idpath fm s2 [])

thread_mbox :: [MBoxEntry] -> [EntryTree MBoxEntry]
thread_mbox mm =
    list_to_entrytree $ map cdepth mss
    where
        ms = zip3 [1..] (map mboxentry_msgid mm) (map mboxentry_inreplyto mm)
        fm = make_fm ms
        mss = sortBy (compare_idp fm) (zip mm ms)
        cdepth im@(m, p) =
            (False, length idp - 1, m)
            where
                idp = idpath fm p []
    

-- }}}


-- Character set conversions {{{

get_charset [] = Default
get_charset ('c':'h':'a':'r':'s':'e':'t':'=':ss) = 
    Enc $ takeWhile (not . isSpace) ss
get_charset (s:ss) = get_charset ss

charset m = 
    case get_header "Content-Type:" (headers m) of
        Nothing -> Default
	Just h -> get_charset h

#ifdef CF_CHARSET_SUPPORT

fail_notice a e = "[Failed to "++a++": "++(show e)++"]\n\n"

encode Default txt = (txt, "text/plain")
encode (Enc charset) txt =
    case from_unicode charset txt of
        Right t -> (t, "text/plain; charset=" ++ charset)
	Left e -> ((fail_notice "encode" e) ++ txt, "text/plain")

decode Default txt = txt
decode (Enc charset) txt =
    case to_unicode charset txt of
        Right t -> t
	Left e -> (fail_notice "decode" e) ++ txt

#else

encode _ txt = (txt, "text/plain")
decode _ txt = txt

#endif


-- }}}
