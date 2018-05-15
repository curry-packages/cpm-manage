------------------------------------------------------------------------------
--- This module implements tools to manage the central repository:
---
--- Run "cpm-manage -h" to see all options.
---
------------------------------------------------------------------------------

module CPM.Manage ( main )
  where

import Directory ( getCurrentDirectory )
import FilePath  ( (</>), replaceExtension )
import IOExts    ( evalCmd )
import List      ( nub, sortBy, sum )
import System    ( getArgs, exitWith, system )
import Time      ( getLocalTime, toDayString )

import HTML.Base
import ShowDotGraph

import CPM.Config          ( Config, repositoryDir, packageInstallDir
                           , readConfigurationWith )
import CPM.ErrorLogger
import CPM.FileUtil        ( inDirectory, inTempDir, recreateDirectory
                           , removeDirectoryComplete )
import CPM.Package
import CPM.PackageCache.Global ( acquireAndInstallPackage, checkoutPackage )
import CPM.Package.Helpers     ( renderPackageInfo )
import CPM.Repository          ( allPackages, listPackages
                               , readPackageFromRepository )
import CPM.Repository.Update   ( addPackageToRepository, updateRepository )
import CPM.Repository.Select   ( getBaseRepository )
import CPM.Resolution          ( isCompatibleToCompiler )

------------------------------------------------------------------------------
-- Some global settings:

--- Base URL of CPM documentations
cpmBaseURL :: String
cpmBaseURL = "http://www.informatik.uni-kiel.de/~curry/cpm/DOC/"

--- Directory of CPM documentations
cpmHtmlDir :: String
cpmHtmlDir = "/net/medoc/home/mh/public_html/curry/cpm"

------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["genhtml"]     -> writeAllPackagesAsHTML
    ["gendocs"]     -> generateDocsOfAllPackages
    ["gentar"]      -> genTarOfAllPackages
    ["testall"]     -> testAllPackages
    ["add"]         -> addNewPackage
    ["update"]      -> updatePackage
    ["showgraph"]   -> showAllPackageDependencies
    ["--help"]      -> putStrLn helpText
    ["-h"]          -> putStrLn helpText
    _               -> do putStrLn $ "Wrong arguments!\n\n" ++ helpText
                          exitWith 1

helpText :: String
helpText = unlines $
  [ "Options:", ""
  , "add        : add this package version to the central repository"
  , "update     : tag git repository of local package with current version"
  , "             and update central index with current package specification"
  , "genhtml    : generate HTML pages of central repository (in directory"
  , "             '" ++ cpmHtmlDir ++ "')"
  , "gendocs    : generate HTML documentations of all packages (in directory"
  , "             '" ++ cpmHtmlDir </> "DOC" ++ "')"
  , "gentar     : generate tar.gz files of all packages (in current directory)"
  , "testall    : test all packages of the central repository"
  , "showgraph  : visualize all package dependencies as dot graph"
  ]

------------------------------------------------------------------------------
--- Get all packages from the repository.
--- For each package, get the newest version compatible
--- to the current compiler. If there is no compatible version and the
--- first argument is False, get the newest version, otherwise the package
--- is ignored.
--- In addition to this package list (third component),
--- the first component contains the current configuration and the
--- second component the list of all packages grouped by versions
--- (independent of the compiler compatbility).
getAllPackageSpecs :: Bool -> IO (Config,[[Package]],[Package])
getAllPackageSpecs compat = do
  config <- readConfiguration
  putStrLn "Reading base repository..."
  repo <- getBaseRepository config
  let allpkgversions = listPackages repo
      allcompatpkgs  = sortBy (\ps1 ps2 -> name ps1 <= name ps2)
                              (concatMap (filterCompatPkgs config)
                                         allpkgversions)
  return (config,allpkgversions,allcompatpkgs)
 where
  -- Returns the first package compatible to the current compiler.
  -- If compat is False and there are no compatible packages,
  -- return the first package.
  filterCompatPkgs cfg pkgs =
    let comppkgs = filter (isCompatibleToCompiler cfg) pkgs
    in if null comppkgs
         then if compat then [] else take 1 pkgs
         else [head comppkgs]

------------------------------------------------------------------------------
-- Generate web pages of the central repository
writeAllPackagesAsHTML :: IO ()
writeAllPackagesAsHTML = inDirectory cpmHtmlDir $ do
  (config,allpkgversions,newestpkgs) <- getAllPackageSpecs False
  putStrLn "Reading all package specifications..."
  allpkgs <- mapIO (fromErrorLogger . readPackageFromRepository config)
                   newestpkgs
  let indexfile = "index.html"
  ltime <- getLocalTime
  putStrLn $ "Writing '" ++ indexfile ++ "'..."
  writeReadableFile indexfile $ showHtmlPage $
    cpmHtmlPage "Curry Packages in the CPM Repository" $
      [h1 [htxt "Curry Packages in the ",
           href "http://www.curry-language.org/tools/cpm" [htxt "CPM"]
             `addAttr` ("target","_blank"),
           htxt $ " Repository (" ++ toDayString ltime ++ ")"],
       packageInfosAsHtmlTable allpkgs] ++
       pkgStatistics allpkgversions newestpkgs
  mapIO_ writePackageAsHTML allpkgs
 where
  pkgStatistics allpkgversions newestpkgs =
    [h4 [htxt "Statistics:"],
     par [htxt $ show (length newestpkgs) ++ " packages", breakline,
          htxt $ show (length (concat allpkgversions)) ++ " package versions"]]
         
  writePackageAsHTML pkg = do
    let pname    = name pkg
        htmlfile = pname ++ ".html"
    putStrLn $ "Writing '" ++ htmlfile ++ "'..."
    let pkginfo = renderPackageInfo True True True pkg
        manref  = manualRef pkg False
    writeReadableFile htmlfile $ showHtmlPage $
      cpmTitledHtmlPage ("Curry Package '"++pname++"'") $
        [blockstyle "reference" $ apiRef pkg False] ++
        (if null manref then [] else [blockstyle "reference" manref]) ++
        [blockstyle "metadata"
           [h3 [htxt "Package metadata:"],
            verbatim pkginfo]]

--- Writes a file readable for all:
writeReadableFile :: String -> String -> IO ()
writeReadableFile f s = writeFile f s >> system ("chmod 644 " ++ f) >> done

--- API reference of a package:
apiRef :: Package -> Bool -> [HtmlExp]
apiRef pkg small =
 let title       = if small then "API" else "API documentation"
     addArrow he = if small then he else addClass he "arrow"
 in [addArrow $ href (cpmBaseURL ++ packageId pkg) [htxt title]]

--- Manual reference of a package:
manualRef :: Package -> Bool -> [HtmlExp]
manualRef pkg small =
 let title       = if small then "PDF" else "Manual (PDF)"
     addArrow he = if small then he else addClass he "arrow"
 in case documentation pkg of
      Nothing -> []
      Just (PackageDocumentation _ docmain _) ->
        [addArrow $ href (cpmBaseURL ++ packageId pkg </>
                          replaceExtension docmain ".pdf")
                         [htxt title]]

-- Format a list of packages as an HTML table
packageInfosAsHtmlTable :: [Package] -> HtmlExp
packageInfosAsHtmlTable pkgs =
  headedTable $
    [map ((:[]) . htxt)  ["Name", "API", "Doc","Executable","Synopsis", "Version"] ] ++
    map formatPkg pkgs
 where
  formatPkg pkg =
    [ [href (name pkg ++ ".html") [htxt $ name pkg]]
    , apiRef pkg True
    , let manref = manualRef pkg True
      in if null manref then [nbsp] else manref
    , [htxt $ maybe ""
                    (\ (PackageExecutable n _ _) -> n)
                    (executableSpec pkg)]
    , [htxt $ synopsis pkg]
    , [htxt $ showVersion (version pkg)] ]

--- Standard HTML page with a title for CPM generated docs.
cpmHtmlPage :: String -> [HtmlExp] -> HtmlPage
cpmHtmlPage title hexps =
  page title hexps `addPageParam` pageCSS "css/cpm.css"

--- Standard HTML page with a title (included in the body)
--- for CPM generated docs:
cpmTitledHtmlPage :: String -> [HtmlExp] -> HtmlPage
cpmTitledHtmlPage title hexps = cpmHtmlPage title (h1 [htxt title] : hexps)

------------------------------------------------------------------------------
-- Generate HTML documentation of all packages in the central repository
generateDocsOfAllPackages :: IO ()
generateDocsOfAllPackages = do
  (_,_,allpkgs) <- getAllPackageSpecs True
  mapIO_ genDocOfPackage allpkgs
 where
  genDocOfPackage pkg = inTempDir $ do
    let pname = name pkg
        pversion = showVersion (version pkg)
    putStrLn $ unlines [dline, "Documenting: " ++ pname, dline]
    let cmd = unwords [ "rm -rf", pname, "&&"
                      , "cypm","checkout", pname, pversion, "&&"
                      , "cd", pname, "&&"
                      , "cypm", "install", "--noexec", "&&"
                      , "cypm", "doc", "--docdir", cpmHtmlDir </> "DOC"
                              , "--url", cpmBaseURL, "&&"
                      , "cd ..", "&&"
                      , "rm -rf", pname
                      ]
    putStrLn $ "CMD: " ++ cmd
    system cmd

------------------------------------------------------------------------------
-- Run `cypm test` on all packages of the central repository
testAllPackages :: IO ()
testAllPackages = do
  (_,_,allpkgs) <- getAllPackageSpecs True
  results <- mapIO checkoutAndTestPackage allpkgs
  if sum (map fst results) == 0
    then putStrLn $ show (length allpkgs) ++ " PACKAGES SUCCESSFULLY TESTED!"
    else do putStrLn $ "ERRORS OCCURRED IN PACKAGES: " ++
                       unwords (map snd (filter ((> 0) . fst) results))
            exitWith 1

dline :: String
dline = take 78 (repeat '=')

------------------------------------------------------------------------------
-- Generate tar.gz files of all packages (in the current directory)
genTarOfAllPackages :: IO ()
genTarOfAllPackages = do
  putStrLn "Generating tar.gz of all package versions..."
  (cfg,allpkgversions,_) <- getAllPackageSpecs False
  allpkgs <- mapIO (fromErrorLogger . readPackageFromRepository cfg)
                   (sortBy (\ps1 ps2 -> packageId ps1 <= packageId ps2)
                           (concat allpkgversions))
  mapIO_ (writePackageAsTar cfg) allpkgs --(take 3 allpkgs)
 where
  writePackageAsTar cfg pkg = do
    let pkgname    = name pkg
        pkgid      = packageId pkg
    putStrLn $ "Checking out '" ++ pkgid ++ "'..."
    let checkoutdir = pkgname
    system $ unwords [ "rm -rf", checkoutdir ]
    fromErrorLogger
      (acquireAndInstallPackage cfg pkg |> checkoutPackage cfg pkg)
    let cmd = unwords [ "mv", checkoutdir, pkgid, "&&"
                      , "tar", "cvzf", pkgid ++ ".tar.gz", pkgid, "&&"
                      , "rm", "-rf", pkgid
                      ]
    putStrLn $ "...with command:\n" ++ cmd
    ecode <- system cmd
    when (ecode>0) $ error $ "ERROR OCCURED IN PACKAGE '" ++ pkgid ++ "'!"


------------------------------------------------------------------------------
-- Add a new package (already committed and pushed into its git repo)
-- where the package specification is stored in the current directory.
addNewPackage :: IO ()
addNewPackage = do
  config <- readConfiguration
  pkg <- fromErrorLogger (loadPackageSpec ".")
  setTagInGit pkg
  let pkgIndexDir      = name pkg </> showVersion (version pkg)
      pkgRepositoryDir = repositoryDir config </> pkgIndexDir
      pkgInstallDir    = packageInstallDir config </> packageId pkg
  fromErrorLogger $ addPackageToRepository config "." False False
  putStrLn $ "Package repository directory '" ++ pkgRepositoryDir ++ "' added."
  (ecode,_) <- checkoutAndTestPackage pkg
  when (ecode>0) $ do
    removeDirectoryComplete pkgRepositoryDir
    removeDirectoryComplete pkgInstallDir
    putStrLn "Checkout/test failure, package deleted in repository directory!"
    updateRepository config
    exitWith 1
  putStrLn $ "\nEverything looks fine..."
  putStrLn $ "\nTo publish the new repository directory, run command:\n"
  putStrLn $ "pushd " ++ repositoryDir config ++
             " && git add " ++ pkgIndexDir </> packageSpecFile ++
             " && git commit -m\"" ++ pkgIndexDir ++ " added\" " ++
             " && git push origin master && popd"

-- Test a specific version of a package by checking it out in temp dir,
-- install it (with a local bin dir), and run all tests.
-- Returns the exit code of the package test command and the packaged id.
checkoutAndTestPackage :: Package -> IO (Int,String)
checkoutAndTestPackage pkg = do
  -- create installation bin dir:
  curdir <- inTempDir getCurrentDirectory
  let bindir = curdir </> "pkgbin"
  recreateDirectory bindir
  let pkgname     = name pkg
      pkgversion  = version pkg
      pkgid       = packageId pkg
  putStrLn $ unlines [dline, "Testing package: " ++ pkgid, dline]
  let checkoutdir = pkgname
      cmd = unwords
              [ "rm -rf", checkoutdir, "&&"
              , "cypm", "checkout", pkgname, showVersion pkgversion, "&&"
              , "cd", checkoutdir, "&&"
              -- install possible binaries in bindir:
              , "cypm", "-d bin_install_path="++bindir, "install", "&&"
              , "export PATH="++bindir++":$PATH", "&&"
              , "cypm", "test", "&&"
              , "cypm", "-d bin_install_path="++bindir, "uninstall"
              ]
  putStrLn $ "...with command:\n" ++ cmd
  ecode <- inTempDir $ system cmd
  inTempDir (system $ unwords ["rm -rf ", checkoutdir, bindir])
  when (ecode>0) $ putStrLn $ "ERROR OCCURED IN PACKAGE '"++pkgid++ "'!"
  return (ecode,pkgid)

-- Set the package version as a tag in the git repository.
setTagInGit :: Package -> IO ()
setTagInGit pkg = do
  let ts = 'v' : showVersion (version pkg)
  (_,gittag,_) <- evalCmd "git" ["tag","-l",ts] ""
  let deltag = if null gittag then [] else ["git tag -d",ts,"&&"]
      cmd    = unwords $ deltag ++ ["git tag -a",ts,"-m",ts,"&&",
                                    "git push --tags -f"]
  putStrLn $ "Execute: " ++ cmd
  ecode <- system cmd
  when (ecode > 0) $ error "ERROR in setting the git tag"

------------------------------------------------------------------------------
-- Re-tag the current git version with the current package version
-- and copy the package spec file to the cpm index
updatePackage :: IO ()
updatePackage = do
  config <- readConfiguration
  pkg <- fromErrorLogger (loadPackageSpec ".")
  let pkgInstallDir    = packageInstallDir config </> packageId pkg
  setTagInGit pkg
  putStrLn $ "Deleting old repo copy '" ++ pkgInstallDir ++ "'..."
  removeDirectoryComplete pkgInstallDir
  (ecode,_) <- checkoutAndTestPackage pkg
  when (ecode > 0) $ do removeDirectoryComplete pkgInstallDir
                        putStrLn $ "ERROR in package, CPM index not updated!"
                        exitWith 1
  fromErrorLogger $ addPackageToRepository config "." True False

------------------------------------------------------------------------------
-- Show package dependencies as graph
showAllPackageDependencies :: IO ()
showAllPackageDependencies = do
  config <- readConfiguration
  pkgs <- getBaseRepository config >>= return . allPackages
  let alldeps = map (\p -> (name p, map (\ (Dependency p' _) -> p')
                                        (dependencies p)))
                    pkgs
      dotgraph = depsToGraph alldeps
  putStrLn $ "Show dot graph..."
  viewDotGraph dotgraph

depsToGraph :: [(String, [String])] -> DotGraph
depsToGraph cpmdeps =
  Graph "CPM Dependencies"
        (map (\s -> Node s []) (nub (map fst cpmdeps ++ concatMap snd cpmdeps)))
        (map (\ (s,t) -> Edge s t [])
             (nub (concatMap (\ (p,ds) -> map (\d -> (p,d)) ds) cpmdeps)))

------------------------------------------------------------------------------
--- Reads to the .cpmrc file from the user's home directory and return
--- the configuration. Terminate in case of some errors.
readConfiguration :: IO Config
readConfiguration =
  readConfigurationWith [] >>= \c -> case c of
    Left err -> do putStrLn $ "Error reading .cpmrc file: " ++ err
                   exitWith 1
    Right c' -> return c'

------------------------------------------------------------------------------
-- The name of the package specification file.
packageSpecFile :: String
packageSpecFile = "package.json"

------------------------------------------------------------------------------
