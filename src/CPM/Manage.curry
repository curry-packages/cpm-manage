------------------------------------------------------------------------------
--- This module implements tools to manage the central repository:
---
--- > cpm-manage add package.json: add this package to the central repository
--- > cpm-manage testall: test all packages of the central repository
---
------------------------------------------------------------------------------

module CPM.Manage ( main )
  where

import CSV       ( readCSVFile )
import Directory ( copyFile, doesFileExist, doesDirectoryExist
                 , createDirectoryIfMissing, getCurrentDirectory )
import FilePath  ( (</>) )
import HTML
import IOExts    ( evalCmd )
import List      ( sum )
import Maybe     ( isJust )
import System    ( getArgs, exitWith, system )

import CPM.Config   ( repositoryDir, packageInstallDir, readConfiguration )
import CPM.ErrorLogger
import CPM.FileUtil ( inTempDir, recreateDirectory )
import CPM.Package

--- Base URL of CPM documentations
cpmBaseURL :: String
cpmBaseURL = "http://www-ps.informatik.uni-kiel.de/~mh/curry/cpm/"

--- Directory of CPM documentations
cpmHtmlDir :: String
cpmHtmlDir = "/net/medoc/home/mh/public_html/curry/cpm"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["genhtml"]     -> writeAllPackagesAsHTML
    ["gendocs"]     -> generateDocsOfAllPackages
    ["testall"]     -> testAllPackages
    ["add",pkgfile] -> addNewPackage pkgfile
    ["updatetag"]   -> updateTagOfPackage
    _               -> do putStrLn $ "Wrong arguments!\n\n" ++ helpText
                          exitWith 1

helpText :: String
helpText = unlines $
  [ "Options:", ""
  , "add package.json : add this package to the central repository"
  , "genhtml          : generate HTML pages of central repository (in local files)"
  , "gendocs          : generate HTML documentations of all packages (in directory"
  , "                   " ++ cpmHtmlDir ++ ")"
  , "testall          : test all packages of the central repository"
  , "updatetag        : update current tag in locale package, i.e., delete it"
  , "                   and add it in the git repository"
  ]

------------------------------------------------------------------------------
---- Get infos of all packages with a valid version.
allValidPackageInfos :: IO [[String]]
allValidPackageInfos = do
  system ("cpm list --csv > allpkgs.csv")
  allinfos <- readCSVFile "allpkgs.csv" >>= return . tail
  return $ filter isValidVersion allinfos
 where
  isValidVersion pkginfo = case pkginfo of
    [_,_,version] -> isJust (readVersion version)
    _             -> False

------------------------------------------------------------------------------
-- Generate web pages of the central repository
writeAllPackagesAsHTML :: IO ()
writeAllPackagesAsHTML = do
  allinfos <- allValidPackageInfos
  let indexfile = "index.html"
  putStrLn $ "Writing '" ++ indexfile ++ "'..."
  writeVisibleFile indexfile $ showHtmlPage $
    standardPage "Curry Packages in the CPM Repository"
                 [packageInfosAsHtmlTable allinfos]
  mapIO_ writePackageAsHTML allinfos
  system "rm -f allpkgs.csv" >> done
 where
  writePackageAsHTML pkginfo = case pkginfo of
    [name,_,version] -> do
      let htmlfile = name ++ ".html"
      putStrLn $ "Writing '" ++ htmlfile ++ "'..."
      (_,out,_) <- evalCmd "cpm" ["info","-a","-p",name,version] ""
      let apiref = cpmBaseURL ++ "DOC_" ++ name
      writeVisibleFile htmlfile $ showHtmlPage $
        standardPage ("Curry Package '"++name++"'")
                     [h2 [href apiref [htxt "API documentation"]],
                      verbatim out]
    _ -> error $ "Illegal package info: " ++ show pkginfo

  writeVisibleFile f s = writeFile f s >> system ("chmod 644 " ++ f) >> done


-- Format a list of package infos (name, synopsi, version) as an HTML table
packageInfosAsHtmlTable :: [[String]] -> HtmlExp
packageInfosAsHtmlTable pkginfos =
  headedTable $ [map ((:[]) . htxt)  ["Name", "Synopsis", "Version"] ] ++
                map formatPkg pkginfos
 where
  formatPkg pkginfo = case pkginfo of
    [pname,psyn,pversion] ->  [ [href (pname++".html") [htxt pname]]
                              , [htxt psyn], [htxt pversion] ]
    _ -> error $ "Illegal package info: " ++ show pkginfo


------------------------------------------------------------------------------
-- Generate HTML documentation of all packages in the central repository
generateDocsOfAllPackages :: IO ()
generateDocsOfAllPackages = do
  allinfos <- allValidPackageInfos
  mapIO_ genDocOfPackage allinfos
  system "rm -f allpkgs.csv" >> done
 where
  genDocOfPackage pkginfo = case pkginfo of
    [name,_,version] -> do
      putStrLn $ unlines [dline, "Documenting: " ++ name, dline]
      let docdir = cpmHtmlDir </> "DOC_" ++ name
          cmd = unwords [ "rm -rf", name, "&&"
                        , "cpm","checkout", name, version, "&&"
                        , "cd", name, "&&"
                        , "cpm", "install", "--noexec", "&&"
                        , "cpm", "doc", "--docdir", docdir, "&&"
                        , "cd ..", "&&"
                        , "rm -rf", name
                        ]
      putStrLn $ "CMD: " ++ cmd
      system cmd
    _ -> error $ "Illegal package info: " ++ show pkginfo

------------------------------------------------------------------------------
-- Run `cpm test` on all packages of the central repository
testAllPackages :: IO ()
testAllPackages = do
  allinfos <- allValidPackageInfos
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
      then putStrLn $ show (length allinfos) ++ " PACKAGES SUCCESSFULLY TESTED!"
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

dline :: String
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
  putStrLn $ "pushd " ++ repositoryDir config ++
             " && git add " ++ pkgIndexDir </> "package.json" ++
             " && git commit -m\"" ++ pkgIndexDir ++ " added\" " ++
             " && git push origin master && popd"

------------------------------------------------------------------------------
-- Re-tag the current git version with the current package version
-- and copy the package spec file to the cpm index
updateTagOfPackage :: IO ()
updateTagOfPackage = do
  loadPackageSpec "." |>= \pkg ->
   updateTagInGit ('v' : showVersion (version pkg)) |>
   updateCpmIndex pkg
  done
 where
  updateTagInGit t = do
    let cmd = unwords ["git tag -d",t,"&&","git tag -a",t,"-m",t,"&&",
                       "git push --tags -f"]
    putStrLn $ "Execute: " ++ cmd
    system cmd
    succeedIO ()

  updateCpmIndex pkg = do
    config <- readConfiguration >>= \c -> case c of
      Left err -> do
        putStrLn $ "Error reading .cpmrc file: " ++ err
        exitWith 1
      Right c' -> return c'
    let pkgFile          = "package.json"
        pkgIndexDir      = name pkg </> showVersion (version pkg)
        pkgRepositoryDir = repositoryDir config </> pkgIndexDir
        cmd = unwords ["cp -f", pkgFile, pkgRepositoryDir </> pkgFile]
    putStrLn $ "Execute: " ++ cmd
    system cmd
    succeedIO ()

------------------------------------------------------------------------------
