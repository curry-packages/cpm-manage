------------------------------------------------------------------------------
--- This module implements tools to manage the central repository:
---
--- > cpm-manage add package.json: add this package to the central repository
--- > cpm-manage testall: test all packages of the central repository
---
------------------------------------------------------------------------------

module Manage(main) where

import CSV       ( readCSVFile )
import Directory ( copyFile, doesFileExist, doesDirectoryExist
                 , createDirectoryIfMissing, getCurrentDirectory )
import FilePath  ( (</>) )
import List      ( sum )
import System    ( getArgs, exitWith, system )

import CPM.Config   ( repositoryDir, packageInstallDir, readConfiguration )
import CPM.FileUtil ( inTempDir, recreateDirectory )
import CPM.Package

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["testall"]     -> testAllPackages
    ["add",pkgfile] -> addNewPackage pkgfile
    _               -> do putStrLn $ "Wrong arguments!\n\n" ++ helpText
                          exitWith 1

helpText :: String
helpText = unlines $
  [ "Options:", ""
  , "add package.json : add this package to the central repository"
  , "testall          : test all packages of the central repository"]

------------------------------------------------------------------------------
-- Run `cpm test` on all packages of the central repository
testAllPackages :: IO ()
testAllPackages = do
  --system ("cpm list --all --csv > allpkgs.csv")
  system ("cpm list --csv > allpkgs.csv")
  allinfos <- readCSVFile "allpkgs.csv" >>= return . tail
  runAllTests allinfos
  system "rm -f allpkgs.csv" >> done
 where
  runAllTests allinfos = do
    -- create installation bin dir:
    curdir <- getCurrentDirectory
    let bindir = curdir </> "pkgbin"
    recreateDirectory bindir
    results <- mapIO (testPackage bindir) allinfos
    if sum (map fst results) == 0
      then putStrLn "PACKAGES SUCCESSFULLY TESTED!"
      else do putStrLn $ "ERRORS OCCURRED IN PACKAGES: " ++
                         unwords (map snd (filter ((> 0) . fst) results))
              exitWith 1

  testPackage bindir pkginfo = case pkginfo of
    [name,_,version] -> do
      let pkgname = name ++ "-" ++ version
      putStrLn $ unlines [dline, "Testing: " ++ pkgname, dline]
      let cmd = unwords
                  [ "rm -rf", name, "&&"
                  , "cpm","checkout", name, version, "&&"
                  , "cd", name, "&&"
                  -- install possible binaries in bindir:
                  , "cpm", "-d bin_install_path="++bindir, "install", "&&"
                  , "export PATH="++bindir++":$PATH", "&&"
                  , "cpm", "test", "&&"
                  , "cd ..", "&&"
                  , "rm -rf", name
                  ]
      putStrLn $ "CMD: " ++ cmd
      ecode <- system cmd
      when (ecode>0) $ putStrLn $ "ERROR OCCURED IN PACKAGE '"++pkgname++ "'!"
      return (ecode,pkgname)
    _ -> error $ "Illegal package info: " ++ show pkginfo

  dline = take 78 (repeat '=')

------------------------------------------------------------------------------
-- Add a new package where the name of the package description file
-- is given as a parameter.
addNewPackage :: String -> IO ()
addNewPackage pkgfile = do
  config <- readConfiguration >>= \c -> case c of
    Left err -> do
      putStrLn $ "Error reading .cpmrc file: " ++ err
      exitWith 1
    Right c' -> return c'
  expkgfile <- doesFileExist pkgfile
  unless expkgfile (error $ "Package file '" ++ pkgfile ++ "' does not exist!")
  pkgtxt <- readFile pkgfile
  let pkg = case readPackageSpec pkgtxt of
              Left err -> error err
              Right p  -> p
  let pkgName          = name pkg
      pkgVersion       = version pkg
      pkgIndexDir      = pkgName </> showVersion pkgVersion
      pkgCheckoutDir   = name pkg
      pkgRepositoryDir = repositoryDir config </> pkgIndexDir
  expkgdir <- doesDirectoryExist pkgRepositoryDir
  when expkgdir (error $ "Package repository directory '" ++ pkgRepositoryDir ++
                         "' already exists!")
  putStrLn $ "Create directory: " ++ pkgRepositoryDir
  createDirectoryIfMissing True pkgRepositoryDir
  copyFile pkgfile (pkgRepositoryDir </> "package.json")
  putStrLn $ "Package repository directory '" ++ pkgRepositoryDir ++ "' added."
  let cmd = unwords [ "cpm", "checkout", pkgName, showVersion pkgVersion, "&&"
                    , "cd", pkgCheckoutDir, "&&"
                    , "cpm", "install", "&&"
                    , "cpm", "test", "&&"
                    , "cd ..", "&&", "rm -rf", pkgCheckoutDir]
  putStrLn $ "\nChecking new package with command:\n" ++ cmd
  ecode <- inTempDir $ system cmd
  when (ecode>0) $ do
    inTempDir (system $ "rm -rf " ++ pkgCheckoutDir)
    system $ "rm -rf " ++ pkgRepositoryDir
    system $ "rm -rf " ++ packageInstallDir config </> packageId pkg
    putStrLn "Unable to checkout, package deleted in repository directory!"
    exitWith 1
  putStrLn $ "\nEverything looks fine..."
  putStrLn $ "\nTo publish the new repository directory, run command:\n"
  putStrLn $ "cd " ++ repositoryDir config ++
             " && git add " ++ pkgIndexDir </> "package.json" ++
             " && git commit -m\"" ++ pkgIndexDir ++ " added\" " ++
             " && git push origin master"

------------------------------------------------------------------------------
