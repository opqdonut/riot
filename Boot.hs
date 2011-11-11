{-# OPTIONS -cpp -fglasgow-exts #-}
--
-- riot/Boot.hs
-- 
-- Copyright (c) Tuomo Valkonen 2004-2005.
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--

--
-- Boot loader for riot-hsplugins.
-- This is a small stub that loads the runtime riot library, then jumps
-- to it. It solves the problem of unnecessary code linking.
-- As such, we only want to do plugin-related stuff here. So we don't
-- even mess with the locale stuff yet
--
-- We should have no static dependencies on any other part of riot, or
-- ginsu
--
-- One of our jobs is to find value :: ConfigAPI.Config to pass to
-- Main.main (a dynamically loaded module in the riot library). To do
-- this we need to check if the -B flag was on the command line.
--
-- Once we find our Config file we have to:
--      * load Config.o if it exists
--      * load -package riot, and any dependencies
--      * jump to Main.main, with Config as an argument
--
-- This is the only module that depends on -package plugins
-- NB: ncurses must be statically linked in. It can't be dynamically loaded
-- NB: we now need to distribute Main.o outside of HSriot.o
--

module Boot ( main ) where

import Riot.BootAPI           ( ConfigData(..), RiotMainType )
import System.Plugins
import System.Plugins.Utils   ( (</>), (<.>) )

import Data.Maybe
import Data.IORef
import Control.Monad          (when)
import System.IO              (hFlush, stdout)
import System.Directory
import System.IO.Unsafe       (unsafePerformIO)
import System.Console.GetOpt
import System.Environment     (getArgs, getEnv)
import System.Exit            (exitFailure)
import System.Posix.User      (getUserEntryForID, getRealUserID, homeDirectory)

-- ---------------------------------------------------------------------
-- riot + hs-plugins uses config scripts stored in ~/.riotrc.
--

config_dir, config_file, riot_main_obj :: FilePath
config_dir    = ".riotrc"                -- ~/.riotrc/ stores Config.hs
config_file   = "Config.hs"              -- name of user-defineable Config file

riot_main_obj = "Riot/RiotMain.o"

config_sym, riot_main_sym :: Symbol
config_sym    = "riot"                   -- symbol to retrieve from Config.hs
riot_main_sym = "dynamic_main"

-- ---------------------------------------------------------------------
-- Where do the libraries live?
-- This value can be overridden on the command line with the -B flag
--

libdir :: IORef FilePath
libdir = unsafePerformIO $ newIORef (LIBDIR :: FilePath)
{-# NOINLINE libdir #-}

-- ---------------------------------------------------------------------
-- Finding config files
--
get_home :: IO String
get_home =
    catch (getRealUserID >>= getUserEntryForID >>= (return . homeDirectory))
          (\_ -> getEnv "HOME")

-- ~/.riotrc/
get_config_dir  :: IO String
get_config_dir = do
    home <- get_home
    return $ home</>config_dir

-- ~/.riotrc/Config.hs
get_config_file :: IO (String)
get_config_file = do
    home <- get_home
    return $ home</>config_dir</>config_file

-- ---------------------------------------------------------------------
-- Do some argv parsing to strip off any -B args needed to find our
-- runtime libraries. Any other args are ignored and passed through to
-- Main.main. Think like GHCs +RTS -RTS options
--

data Opts = LibDir FilePath

options :: [OptDescr Opts]
options = [
        Option ['B']  ["libdir"]  (ReqArg LibDir "libdir") "Path to runtime libraries"
    ]

--
-- Determine if there is a libdir -B flag provided
--
doArgs :: [String] -> (Maybe FilePath)
doArgs argv = case getOpt Permute options argv of
        (o, _, _) -> case reverse o of
                        []           -> Nothing
                        (LibDir d:_) -> Just d

-- ---------------------------------------------------------------------
--
-- Given a source file, compile it to a (.o, .hi) pair
--
compile :: FilePath -> IO (Maybe FilePath)
compile src = do
    flags  <- get_make_flags
    status <- make src $ flags ++ ["-i"] -- to stop it seeing random .o files
    case status of
        MakeSuccess _ obj -> return $ Just obj
        MakeFailure errs  -> do 
            putStrLn "Errors in config file, using defaults"
            mapM_ putStrLn errs
            return Nothing

--
-- What packages do we need -- in dependency order
--
packages :: [String]
packages = [ "riot" ]

--
-- Flags to find the runtime libraries, to help ghc out
--
get_make_flags :: IO [String]
get_make_flags = do libpath <- readIORef libdir
                    let pkgs = concatMap (f libpath) packages
                    return $ ("-i" ++ libpath) : pkgs
    where f l p = ["-package-conf", l </> p <.> "conf", "-package", p]

get_load_flags :: IO [String]
get_load_flags = do libpath <- readIORef libdir
                    return $ map (\p -> libpath </> p <.> "conf") packages


-- ---------------------------------------------------------------------
-- Let's go. Find and load a config file, and pass argv, a handle to the
-- module and the symbol to retrieve from it to Main.main
--

main :: IO ()
main = do
    -- look for -B libdir flag
    argv <- getArgs              
    let mlib = doArgs argv 
    when (isJust mlib) $ writeIORef libdir (fromJust mlib)

    -- check if ~/.riotrc/ exists
    d        <- get_config_dir          
    d_exists <- doesDirectoryExist d
    when (not d_exists) $ createDirectory d

    putStr "Starting up dynamic Haskell ... " >> hFlush stdout

    -- look for ~/.riotrc/Config.hs
    c        <- get_config_file 
    c_exists <- doesFileExist c
    m_obj    <- if c_exists then compile c else return Nothing
 
    -- now load user's Config.o if we have it
    paths   <- get_load_flags
    libpath <- readIORef libdir
    cfghdl  <- case m_obj of
        Nothing  -> return Nothing
        Just obj -> do status <- load obj [libpath] paths config_sym
                       case status of
                            LoadSuccess _ v -> return $ Just (CD v)
                            LoadFailure e   -> do
                                putStrLn "Unable to config file, using defaults"
                                mapM_ putStrLn e ; return Nothing

    -- now, get a handle to Main.dynamic_main, and jump to it
    status    <- load (libpath </> riot_main_obj) [] paths riot_main_sym
    riot_main <- case status of
        LoadSuccess _ v -> return (v :: RiotMainType)
        LoadFailure e -> do putStrLn "Unable to load riot.Main, exiting"
                            mapM_ putStrLn e ; exitFailure

    riot_main cfghdl   -- jump to dynamic code

-- vim: sw=4 ts=4
