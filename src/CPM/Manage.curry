------------------------------------------------------------------------------
--- This module implements tools to manage the central repository:
---
--- Run "cpm-manage -h" to see all options.
---
------------------------------------------------------------------------------

module CPM.Manage -- ( main )
  where

import Directory ( copyFile, doesFileExist, doesDirectoryExist
                 , createDirectoryIfMissing, getCurrentDirectory )
import FilePath  ( (</>), replaceExtension )
import HTML
import List      ( findIndex, nub, replace, sortBy, sum, union )
import System    ( getArgs, exitWith, system )

import CPM.Config     ( repositoryDir, packageInstallDir, readConfiguration )
import CPM.ErrorLogger
import CPM.FileUtil   ( inTempDir, recreateDirectory )
import CPM.Package
import CPM.PackageCopy ( renderPackageInfo )
import qualified CPM.PackageCache.Global as GC
import CPM.Repository ( allPackages, listPackages, readRepository
                      , updateRepositoryCache )
import CPM.Resolution ( isCompatibleToCompiler )

import ShowDotGraph

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
    ["showgraph"]   -> showAllPackageDependencies
    ["--help"]      -> putStrLn helpText
    ["-h"]          -> putStrLn helpText
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
  , "showgraph        : visualize all package dependencies as dot graph"
  ]

------------------------------------------------------------------------------
--- Get all packages. For each package, get the newest version compatible
--- to the current compiler. If there is no compatible version and the
--- first argument is False, get the newest version, otherwise the package
--- is ignored.
getAllPackageSpecs :: Bool -> IO [Package]
getAllPackageSpecs compat = do
  config <- readConfiguration >>= \c ->
   case c of
    Left err -> do putStrLn $ "Error reading .cpmrc settings: " ++ err
                   exitWith 1
    Right c' -> return c'
  repo <- readRepository config
  let allpkgs = sortBy (\ps1 ps2 -> name ps1 <= name ps2)
                       (concatMap (filterCompatPkgs config)
                                  (listPackages repo))
  return allpkgs
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
writeAllPackagesAsHTML = do
  allpkgs  <- getAllPackageSpecs False
  let indexfile = "index.html"
  putStrLn $ "Writing '" ++ indexfile ++ "'..."
  writeReadableFile indexfile $ showHtmlPage $
    cpmStandardPage "Curry Packages in the CPM Repository"
                    [packageInfosAsHtmlTable allpkgs]
  mapIO_ writePackageAsHTML allpkgs
  system "rm -f allpkgs.csv" >> done
 where
  writePackageAsHTML pkg = do
    let pname    = name pkg
        htmlfile = pname ++ ".html"
    putStrLn $ "Writing '" ++ htmlfile ++ "'..."
    let pkginfo = renderPackageInfo True True GC.emptyCache pkg
        manref  = manualRef pkg False
    writeReadableFile htmlfile $ showHtmlPage $
      cpmStandardPage ("Curry Package '"++pname++"'") $
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
 in [addArrow $ href (cpmBaseURL ++ "DOC_" ++ name pkg) [htxt title]]

--- Manual reference of a package:
manualRef :: Package -> Bool -> [HtmlExp]
manualRef pkg small =
 let title       = if small then "PDF" else "Manual (PDF)"
     addArrow he = if small then he else addClass he "arrow"
 in case documentation pkg of
      Nothing -> []
      Just (PackageDocumentation _ docmain _) ->
        [addArrow $ href (cpmBaseURL ++ "DOC_" ++ name pkg </>
                          replaceExtension docmain ".pdf")
                         [htxt title]]

-- Format a list of packages as an HTML table
packageInfosAsHtmlTable :: [Package] -> HtmlExp
packageInfosAsHtmlTable pkgs =
  headedTable $
    [map ((:[]) . htxt)  ["Name", "API", "Doc","Synopsis", "Version"] ] ++
    map formatPkg pkgs
 where
  formatPkg pkg =
    [ [href (name pkg ++ ".html") [htxt $ name pkg]]
    , apiRef pkg True
    , let manref = manualRef pkg True
      in if null manref then [nbsp] else manref
    , [htxt $ synopsis pkg]
    , [htxt $ showVersion (version pkg)] ]

-- Standard HTML page for CPM generated docs:
cpmStandardPage :: String -> [HtmlExp] -> HtmlPage
cpmStandardPage title hexps =
  standardPage title hexps `addPageParam` pageCSS "css/cpm.css"

------------------------------------------------------------------------------
-- Generate HTML documentation of all packages in the central repository
generateDocsOfAllPackages :: IO ()
generateDocsOfAllPackages = do
  allpkgs <- getAllPackageSpecs True
  mapIO_ genDocOfPackage allpkgs
  system "rm -f allpkgs.csv" >> done
 where
  genDocOfPackage pkg = do
    let pname = name pkg
        pversion = showVersion (version pkg)
    putStrLn $ unlines [dline, "Documenting: " ++ pname, dline]
    let docdir = cpmHtmlDir </> "DOC_" ++ pname
        cmd = unwords [ "rm -rf", pname, "&&"
                      , "cpm","checkout", pname, pversion, "&&"
                      , "cd", pname, "&&"
                      , "cpm", "install", "--noexec", "&&"
                      , "cpm", "doc", "--docdir", docdir, "&&"
                      , "cd ..", "&&"
                      , "rm -rf", pname
                      ]
    putStrLn $ "CMD: " ++ cmd
    system cmd

------------------------------------------------------------------------------
-- Run `cpm test` on all packages of the central repository
testAllPackages :: IO ()
testAllPackages = do
  allpkgs <- getAllPackageSpecs True
  runAllTests allpkgs
  system "rm -f allpkgs.csv" >> done
 where
  runAllTests allpkgs = do
    -- create installation bin dir:
    curdir <- getCurrentDirectory
    let bindir = curdir </> "pkgbin"
    recreateDirectory bindir
    results <- mapIO (testPackage bindir) allpkgs
    if sum (map fst results) == 0
      then putStrLn $ show (length allpkgs) ++ " PACKAGES SUCCESSFULLY TESTED!"
      else do putStrLn $ "ERRORS OCCURRED IN PACKAGES: " ++
                         unwords (map snd (filter ((> 0) . fst) results))
              exitWith 1

  testPackage bindir pkg = do
    let pname    = name pkg
        pversion = showVersion (version pkg)
        pkgid    = packageId pkg
    putStrLn $ unlines [dline, "Testing: " ++ pkgid, dline]
    let cmd = unwords
                [ "rm -rf", pname, "&&"
                , "cpm","checkout", pname, pversion, "&&"
                , "cd", pname, "&&"
                -- install possible binaries in bindir:
                , "cpm", "-d bin_install_path="++bindir, "install", "&&"
                , "export PATH="++bindir++":$PATH", "&&"
                , "cpm", "test", "&&"
                , "cd ..", "&&"
                , "rm -rf", pname
                ]
    putStrLn $ "CMD: " ++ cmd
    ecode <- system cmd
    when (ecode>0) $ putStrLn $ "ERROR OCCURED IN PACKAGE '"++pkgid++ "'!"
    return (ecode,pkgid)

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
  updateRepositoryCache config
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
    updateRepositoryCache config
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
    updateRepositoryCache config
    succeedIO ()

------------------------------------------------------------------------------
-- Show package dependencies as graph
showAllPackageDependencies :: IO ()
showAllPackageDependencies = do
  config <- readConfiguration >>= \c -> case c of
    Left err -> do
      putStrLn $ "Error reading .cpmrc file: " ++ err
      exitWith 1
    Right c' -> return c'
  pkgs <- readRepository config >>= return . allPackages
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
