#!/usr/bin/env stack
-- stack --resolver lts-15.4 script 

import System.Directory (listDirectory, canonicalizePath)
import System.FilePath (takeFileName)
import System.IO.Error (isPermissionError)

import Data.List (isPrefixOf, stripPrefix, sortOn)
import Data.Maybe (mapMaybe)

import Numeric (readHex)

import Control.Monad (forM_)
import Control.Exception (throw, try)


data Process = Process {
    port :: Int,
    pid :: Int,
    cmdLine :: String
  } deriving (Show)


procDirs :: IO [FilePath]
procDirs =
  do
    dirNames <- listDirectory "/proc"
    let procNames = filter (all (`elem` "0123456789")) dirNames
    return $ map ("/proc/" ++) procNames


-- TODO: parse socket string instead of drop + take
extractInode :: String -> String
extractInode fd =
  let rst = drop 8 fd
  in take (length rst - 1) rst

inodes :: FilePath -> IO [String]
inodes procDir =
  do
    fds <- listDirectory (procDir ++ "/fd")
    let fullFds = map ((procDir ++ "/fd/") ++) fds
    canonicalFds <- sequence $ map canonicalizePath fullFds
    let socketFiles = filter ((procDir ++ "/fd/socket:") `isPrefixOf`) canonicalFds
    return $ map (extractInode . takeFileName) socketFiles

listeningInodePort :: String -> Maybe (String, String)
listeningInodePort line =
  let ws = words line
      localAddr = ws !! 1
      -- TODO: oh lord mother o mary
      state = fst . (!! 0) . readHex $ ws !! 3
      port = show . fst . (!! 0) . readHex $ drop 9 localAddr
      inode = ws !! 9
   in if state == 10
         then Just (inode, port)
         else Nothing

inodePorts :: FilePath -> IO [(String, String)]
inodePorts procDir =
  do
    ls <- fmap (drop 1 . lines) $ readFile (procDir ++ "/net/tcp")
    return $ mapMaybe listeningInodePort ls

ports :: FilePath -> IO [Int]
ports procDir =
  do
    is <- inodes procDir
    ips <- inodePorts procDir
    return $ mapMaybe (\i -> fmap readInt $ lookup i ips) is
  where
    readInt = read :: String -> Int

cmd :: FilePath -> IO String
cmd procDir =
  do 
    line <- readFile $ procDir ++ "/cmdline"
    return $ map repl line
  where repl '\0' = ' '
        repl c = c

readProc :: FilePath -> IO [Process]
readProc procDir = do
  cmdLine' <- cmd procDir
  let pid' = readInt $ takeFileName procDir
  ports' <- ports procDir
  let procs = map (\port' -> Process {
      port=port',
      pid=pid',
      cmdLine=cmdLine'
    }) ports'
  return $ procs
  where
    readInt = read :: String -> Int

procRow :: Process -> String
procRow proc =
  concat [
    show (port proc),
    "\t",
    show (pid proc),
    "\t",
    cmdLine proc
  ]

main :: IO ()
main = 
  do
    -- Get the results from /proc.
    dirs <- procDirs
    procs <- fmap concat . sequence $ map readProc' dirs
    let sortedProcs = sortOn port procs
    -- Print the results.
    putStrLn "PORT\tPID\tCOMMAND"
    forM_ sortedProcs (putStrLn . procRow)
  where
    -- Skip directories not owned by the current user.
    readProc' dir =
      do
        readOrError <- try $ readProc dir
        case readOrError of
          Left e -> if isPermissionError e then (return []) else throw e
          Right r -> return r

