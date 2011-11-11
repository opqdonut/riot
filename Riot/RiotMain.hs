{-# OPTIONS -cpp -fglasgow-exts #-}
--
-- riot/Riot/RiotMain.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

module Riot.RiotMain (
    static_main,
    dynamic_main
) where

import Riot.Entry
import Riot.MBox
import Riot.Version           (package, version)
import Riot.Riot
import Riot.Event             (event_loop)
import qualified Riot.UI as UI
import qualified Riot.BootAPI (ConfigData(..), RiotMainType)

#ifdef NO_HS_PLUGINS
import qualified Config
#else
import qualified Riot.ConfigAPI
#endif

import Ginsu.Locale           (setupLocale)

import Control.Exception      (catchJust, ioErrors, throw, bracket)
import Control.Monad          (liftM, when)
import Data.IORef
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment     (getArgs, getEnv)
import System.Exit
import System.IO--              (readFile, writeFile)
import System.IO.Error        (isDoesNotExistError)
import System.IO.Unsafe       (unsafePerformIO)
import System.Posix.User      (getUserEntryForID, getRealUserID, homeDirectory)
import System.Posix.Signals as PosixSig

import GHC.Base (unsafeCoerce#)

#ifdef NO_HS_PLUGINS
settings = Config.riot
#else
settings = Riot.ConfigAPI.dflt_settings
#endif

default_file = ".riot"

get_home :: IO (String)
get_home =
    catch (getRealUserID >>= getUserEntryForID >>= (return . homeDirectory))
          (\_ -> getEnv "HOME")

get_default_file :: IO (String)
get_default_file = do
    home <- get_home
    return $ home ++ "/" ++ default_file

do_load :: String -> IO [EntryTree MBoxEntry]
do_load fname = 
     liftM read_mbox_entrytree $ readFile fname

do_save :: String -> [EntryTree MBoxEntry] -> RI MBoxEntry ()
do_save fname et = 
    do cfg <- get_settings
       let n = cfg_backup_numbers cfg
       liftIO $ do 
         make_backups (abs n)
         h <- openFile fname WriteMode
	 mapM_ (hPutStr h) (show_mbox_entrytree_each et)
	 hClose h
    where make_backups 0 = return ()
          make_backups n = 
              let to = backup_name (n - 1)
                  from = if n == 1 then fname else backup_name (n - 2)
              in do b <- doesFileExist from
                    if b then renameFile from to else return ()
                    make_backups (n - 1)
          backup_name n = fname ++ ('.' : show n)

load_initial :: String -> IO ([EntryTree MBoxEntry], String)
load_initial fname = do
    et <- catchJust ioErrors (do_load fname) exh
    return (et, fname)
    where
        exh e = if isDoesNotExistError e then
                    return []
                else
                    ioError e

load_default :: IO ([EntryTree MBoxEntry], String)
load_default =  get_default_file >>= load_initial

init_sighandlers = do 
#ifndef BROKEN_NOCLDSTOP
    --PosixSig.setStoppedChildFlag True
#endif
    PosixSig.installHandler PosixSig.sigCHLD PosixSig.Default Nothing
    PosixSig.installHandler PosixSig.sigINT PosixSig.Ignore Nothing
    PosixSig.installHandler PosixSig.sigPIPE PosixSig.Ignore Nothing
    return ()

release_sighandlers = do 
    PosixSig.installHandler PosixSig.sigINT PosixSig.Default Nothing
    PosixSig.installHandler PosixSig.sigPIPE PosixSig.Default Nothing
    return ()


data Opts = Help | Version | Libdir String

options :: [OptDescr Opts]
options = [
    Option ['V']  ["version"] (NoArg Version) "Show version information",
    Option ['B']  ["libdir"]  (ReqArg Libdir "libdir") "Path to runtime libraries",
    Option ['h']  ["help"]    (NoArg Help)    "Show this help"
    ]

usage = putStr $ usageInfo "Usage: riot [option...] [file]" options
versinfo = putStr $ package++" "++version++"\n"

do_opts :: [Opts] -> IO ()
do_opts [] = return ()
do_opts (o:oo) =
    case o of
        Help     -> usage    >> exitWith(ExitSuccess)
        Version  -> versinfo >> exitWith(ExitSuccess)
        Libdir s -> do_opts oo

do_args :: [String] -> IO ([EntryTree MBoxEntry], String)
do_args args =
    case (getOpt Permute options args) of
        (o, n, []) -> do
            do_opts o
            case n of
                [] -> load_default
                (f:[]) -> load_initial f
                otherwise -> error "Too many file parameters."
        (_, _, errs) -> error (concat errs)

initui :: [EntryTree MBoxEntry] -> String -> RI MBoxEntry ()
initui tt fname = do
    sets <- get_settings
    let styles = cfg_styles sets
        topinfo = cfg_topinfo_text sets
    status <- get_status
    s <- liftIO $ UI.init status styles
    s <- return $ UI.set_topinfo s topinfo
    s <- return $ UI.set_entries s tt (Just fname)
    s <- return $ UI.set_callbacks s do_save new_mboxentry
    set_status s

ri_main :: RI MBoxEntry ()
ri_main = do
    liftIO setupLocale
    args <- liftIO getArgs
    (tt, fname) <- liftIO $ do_args args
    settings <- get_settings
    tt <- return $! tt -- Force load before initialising UI
    ri_bracket (do initui tt fname
                   liftIO init_sighandlers)
               (\_ -> do liftIO UI.deinit
                         liftIO release_sighandlers)
               (\_ -> do UI.refresh
                         event_loop)

init_status :: Entry a => Config -> Status a
init_status cfg = UI.new_status (cfg_key_map cfg)

--
-- static main. no plugins
--
static_main = run_ri (init_status settings) settings ri_main

--
-- dynamic main. called from Boot.hs
--

dynamic_main :: Riot.BootAPI.RiotMainType
dynamic_main (Just (Riot.BootAPI.CD cfg)) = do 
    putStrLn "done."
    case unsafeCoerce# cfg of -- MAGIC: to unwrap the existentially-passed config value
        (cfg_ :: Config) -> 
            do run_ri (init_status cfg_) cfg_ ri_main

dynamic_main Nothing = putStrLn "done." >> static_main

