------------------------------------------------------------------------------
--- This module implements tools to manage the central repository:
---
--- Run "cpm-manage -h" to see all options.
---
------------------------------------------------------------------------------

module CPM.Manage ( main )
  where

import Control.Monad      ( when, unless )
import Data.List          ( (\\), groupBy, isPrefixOf, intercalate, isSuffixOf
                          , nub, nubBy, partition, sort, sortBy, sum )

import Data.Time          ( CalendarTime, getLocalTime, toDayString )
import HTML.Base
import HTML.Styles.Bootstrap4
import ShowDotGraph
import System.Directory   ( createDirectoryIfMissing, doesDirectoryExist
                          , doesFileExist, getAbsolutePath, getCurrentDirectory
                          , getDirectoryContents, getTemporaryDirectory )
import System.FilePath    ( (</>), replaceExtension )
import System.IOExts      ( evalCmd, readCompleteFile )
import System.Environment ( getArgs )
import System.Process     ( getPID, exitWith, system )
import Text.CSV           ( readCSV, writeCSVFile )

import CPM.Config              ( Config, repositoryDir, packageInstallDir
                               , readConfigurationWith, showConfiguration )
import CPM.ErrorLogger
import CPM.FileUtil            ( copyDirectory, inDirectory, quote
                               , recreateDirectory
                               , removeDirectoryComplete )
import CPM.Package
import CPM.PackageCache.Global ( acquireAndInstallPackageFromSource
                               , checkoutPackage )
import CPM.Package.Helpers     ( renderPackageInfo )
import CPM.Repository          ( allPackages, listPackages
                               , readPackageFromRepository )
import CPM.Repository.Update   ( addPackageToRepository, updateRepository )
import CPM.Repository.Select   ( getBaseRepository, getPackageVersion )
import CPM.Resolution          ( isCompatibleToCompiler )

import CPM.Manage.Config
import CPM.Package.HTML

------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["genhtml"]       -> writePackageIndexAsHTML "CPM"
    ["genhtml",d]     -> writePackageIndexAsHTML d
    ["genhtml",d,p,v] -> writePackageVersionAsHTML d p v
    ["genreadme"]     -> writeReadmeFiles "CPM"
    ["genreadme",d]   -> writeReadmeFiles d
    ["gendocs"]       -> generateDocsOfAllPackages packageDocDir
    ["gendocs",d]     -> getAbsolutePath d >>= generateDocsOfAllPackages
    ["gentar"]        -> genTarOfAllPackages packageTarDir
    ["gentar",d]      -> getAbsolutePath d >>= genTarOfAllPackages
    ["testall"]       -> testAllPackages ""
    ["testall",d]     -> getAbsolutePath d >>= testAllPackages
    ["sumcsv",d]      -> do ad <- getAbsolutePath d
                            sumCSVStatsOfPkgs ad "SUM.csv"
    ["add"]           -> addNewPackage True
    ["addnotag"]      -> addNewPackage False
    ["update"]        -> updatePackage
    ["showgraph"]     -> showAllPackageDependencies
    ["writedeps"]     -> writeAllPackageDependencies
    ["copydocs"]      -> copyPackageDocumentations packageDocDir
    ["copydocs",d]    -> getAbsolutePath d >>= copyPackageDocumentations
    ["config"]        -> printConfig
    ["--help"]        -> putStrLn helpText
    ["-h"]            -> putStrLn helpText
    _                 -> do putStrLn $ "Wrong arguments!\n"
                            putStrLn helpText
                            exitWith 1

helpText :: String
helpText = banner ++ unlines
    [ "Options:", ""
    , "config         : show current configuration"
    , "add            : add this package version to the central repository"
    , "                 and tag git repository of this package with its version"
    , "addnotag       : add this package version to the central repository"
    , "                 (do not tag git repository)"
    , "update         : tag git repository of local package with current version"
    , "                 and update central index with current package specification"
    , "genhtml [<d>]  : generate HTML pages of central repository into <d>"
    , "                 (default: 'CPM')"
    , "genhtml <d> <p> <v>: generate HTML pages for package <p> / version <v>"
    , "                 into directory <d>"
    , "genreadme [<d>]: generate README.html files of central repository into <d>"
    , "                 (default: 'CPM') if they are not already present"
    , "gendocs [<d>]  : generate HTML documentations of all packages into <d>"
    , "                 (default: '" ++ packageDocDir ++ "')"
    , "gentar  [<d>]  : generate tar.gz files of all packages into <d>"
    , "                 (default: '" ++ packageTarDir ++ "')"
    , "testall [<d>]  : test all packages of the central repository"
    , "                 and write test statistics into directory <d>"
    , "sumcsv  [<d>]  : sum up all CSV package statistic files in <d>"
    , "showgraph      : visualize all package dependencies as dot graph"
    , "writedeps      : write all package dependencies as CSV file 'pkgs.csv'"
    , "copydocs [<d> ]: copy latest package documentations"
    , "                 from <d> (default: '" ++ packageDocDir ++ "')"
    , "                 to '" ++ currygleDocDir ++ "'"
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
  repo <- fromEL $ getBaseRepository config
  let allpkgversions = listPackages repo
      allcompatpkgs  = sortBy (\ps1 ps2 -> name ps1 <= name ps2)
                              (concatMap (filterCompatPkgs config)
                                         allpkgversions)
  return (config,allpkgversions,allcompatpkgs)
 where
  -- Returns the first package compatible to the current compiler.
  -- If `compat` is False and there are no compatible packages,
  -- return the first package.
  filterCompatPkgs cfg pkgs =
    let comppkgs = filter (isCompatibleToCompiler cfg) pkgs
    in if null comppkgs
         then if compat then [] else take 1 pkgs
         else [head comppkgs]

------------------------------------------------------------------------------
-- Generate README files used in the HTML index pages into the
-- documentation directories if they are not already there.
-- Thus, for each package p version v, do the following:
-- If there is a README file in directory PACKAGES/p-v but not
-- README.html in directory DOC/p-v, generate the latter by
--     pandoc -s -t html -o ....
writeReadmeFiles :: String -> IO ()
writeReadmeFiles cpmindexdir = do
  createDirectoryIfMissing True cpmindexdir
  inDirectory cpmindexdir $ do
    (_,allpkgversions,_) <- getAllPackageSpecs False
    mapM_ genReadmeForPackage
          (sortBy (\p1 p2 -> packageId p1 <= packageId p2)
                  (concat allpkgversions))

genReadmeForPackage :: Package -> IO ()
genReadmeForPackage pkg = do
  putStrLn $ "CHECKING PACKAGE: " ++ pkgid
  rmfiles  <- getReadmeFiles pkgdir
  rmexist  <- doesFileExist $ docdir </> "README.html"
  if null rmfiles || rmexist
    then unless rmexist $ putStrLn $ "No README file found"
    else do
      let readmefile = head rmfiles
          formatcmd1 = formatCmd1 (pkgdir </> readmefile)
          formatcmd2 = formatCmd2 (pkgdir </> readmefile)
      createDirectoryIfMissing True docdir
      putStrLn $ "Executing: " ++ formatcmd1
      rc1 <- system formatcmd1
      putStrLn $ "Executing: " ++ formatcmd2
      rc2 <- system formatcmd2
      if rc1 == 0 && rc2 == 0
        then do
          -- make them readable:
          system $ unwords ["chmod -f 644 ", quote outfile1, quote outfile2]
          return ()
        else error $ "Error during execution of commands:\n" ++
                     formatcmd1 ++ "\n" ++ formatcmd2
 where
  pkgid  = packageId pkg
  pkgdir = "PACKAGES" </> pkgid
  docdir = "DOC" </> pkgid
    
  getReadmeFiles dir = do
    entries <- getDirectoryContents dir
    return $ filter ("README" `isPrefixOf`) entries

  outfile1 = docdir </> "README.html"
  outfile2 = docdir </> "README_I.html"

  formatCmd1 readme = "pandoc -s -t html -o " ++ outfile1 ++ " " ++ readme
  formatCmd2 readme = "pandoc -t html -o " ++ outfile2 ++ " " ++ readme
    
------------------------------------------------------------------------------
-- Generate main HTML index pages of the CPM repository.
writePackageIndexAsHTML :: String -> IO ()
writePackageIndexAsHTML cpmindexdir = do
  createDirectoryIfMissing True cpmindexdir
  inDirectory cpmindexdir $ do
   createDirectoryIfMissing True packageHtmlDir
   system $ "chmod 755 " ++ packageHtmlDir
   (config,allpkgversions,newestpkgs) <- getAllPackageSpecs False
   let stats = pkgStatistics allpkgversions newestpkgs
   putStrLn "Reading all package specifications..."
   allnpkgs <- fromEL $ mapM (readPackageFromRepository config) newestpkgs
   writePackageIndex allnpkgs "index.html" stats 0
   allvpkgs <- fromEL $
                mapM (readPackageFromRepository config)
                 (concat
                    (map reverse
                       (sortBy (\pg1 pg2 -> name (head pg1) <= name (head pg2))
                               allpkgversions)))
   writePackageIndex allvpkgs "indexv.html" stats 1
   writeCategoryIndexAsHTML allnpkgs
   mapM_ (writePackageAsHTML allpkgversions) allvpkgs
   --mapM_ (writePackageAsHTML allpkgversions) $ take 3 allnpkgs
 where
  writePackageIndex allpkgs indexfile statistics actindex = do
    putStrLn $ "Writing '" ++ indexfile ++ "'..."
    indextable <- packageInfosAsHtmlTable allpkgs
    let ptitle   = "Curry Packages in the CPM Repository"
        pkglinks = map (\p -> hrefPrimBadge
                                (packageHtmlDir </> packageId p ++ ".html")
                                [htxt $ if actindex==0 then name p
                                                       else packageId p])
                       allpkgs
        pindex   = [h2 [htxt "Package index:"], par (hitems pkglinks)]
    pagestring <- cpmIndexPage ptitle (pindex ++ [indextable] ++ statistics)
                               actindex
    writeReadableFile indexfile pagestring

  pkgStatistics allpkgversions newestpkgs =
    [h4 [htxt "Statistics:"],
     par [htxt $ show (length newestpkgs) ++ " packages", breakline,
          htxt $ show (length (concat allpkgversions)) ++ " package versions"]]

-- Generate main category index page.
writeCategoryIndexAsHTML :: [Package] -> IO ()
writeCategoryIndexAsHTML allpkgs = do
  let allcats = sortBy (<=) . nub . concatMap category $ allpkgs
      catpkgs = map (\c -> (c, sortBy pidLeq . nubBy pidEq .
                                 filter (\p -> c `elem` category p) $ allpkgs))
                    allcats
  cattables <- mapM formatCat catpkgs
  let catlinks = map (\ (c,_) -> hrefPrimBadge ('#':c) [htxt c]) catpkgs
      hcats = concatMap (\ (c,t) -> [anchor c [htxt ""], hrule, h1 [htxt c], t])
                        cattables
      ptitle = "Curry Packages by Category"
  pagestring <- cpmIndexPage ptitle
                  (h2 [htxt "Category index:"] : par (hitems catlinks) :
                   hcats) 2
  let catindexfile = "indexc.html"
  putStrLn $ "Writing '" ++ catindexfile ++ "'..."
  writeReadableFile catindexfile pagestring
 where
  pidEq p1 p2 = packageId p1 == packageId p2

  pidLeq p1 p2 = packageId p1 <= packageId p2

  formatCat (c,ps) = do
    pstable <- packageInfosAsHtmlTable ps
    return (c, pstable)

--- Standard HTML page for generated a package index.
cpmIndexPage :: String -> [BaseHtml] -> Int -> IO String
cpmIndexPage title maindoc actindex = do
  time <- getLocalTime
  let dayversion = " (Version: " ++ toDayString time ++ ")"
      btbase     = "bt4"
  return $ showHtmlPage $
    bootstrapPage (favIcon btbase) (cssIncludes btbase) (jsIncludes btbase)
                  title homeBrand
                  (leftTopMenu False actindex)
                  rightTopMenu 0 []
                  [h1 [htxt title, smallMutedText dayversion]]
                  maindoc (curryDocFooter time)

--- Generate HTML page for a package in a given version into a directory.
writePackageVersionAsHTML :: String -> String -> String -> IO ()
writePackageVersionAsHTML cpmindexdir pname pversion = do
  case readVersion pversion of
    Nothing -> error $ "'" ++ pversion ++ "' is not a valid version"
    Just  v -> do
      (cfg,allpkgs,_) <- getAllPackageSpecs False
      mbpkg <- fromEL $ getPackageVersion cfg pname v
      case mbpkg of
        Nothing ->
          error $ "Package '" ++ pname ++ "-" ++ pversion ++ "' not found!"
        Just pkg -> do
          fullpkg <- fromEL $ readPackageFromRepository cfg pkg
          createDirectoryIfMissing True cpmindexdir
          putStrLn $ "Changing to directory '" ++ cpmindexdir ++ "'..."
          inDirectory cpmindexdir $ do
            createDirectoryIfMissing True packageHtmlDir
            system $ "chmod 755 " ++ packageHtmlDir
            writePackageAsHTML allpkgs fullpkg

--- Write HTML pages for a single package.
writePackageAsHTML :: [[Package]] -> Package -> IO ()
writePackageAsHTML allpkgversions pkg = do
  pagestring <- packageToHTML allpkgversions pkg
  inDirectory packageHtmlDir $ do
    putStrLn $ "Writing '" ++ htmlfile ++ "'..."
    writeReadableFile htmlfile pagestring
    putStrLn $ "Writing '" ++ htmlsrcfile ++ "'..."
    srcdirstring <- directoryContentsPage (".." </> "PACKAGES") pkgid
    writeReadableFile htmlsrcfile srcdirstring
    writeReadableFile metafile (renderPackageInfo True True True pkg)
    -- set symbolic link to recent package:
    system $ unwords
      ["/bin/rm", "-f", htmllink, "&&", "ln", "-s", htmlfile, htmllink]
    return ()
 where
  pkgid       = packageId pkg
  htmlfile    = pkgid ++ ".html"
  htmlsrcfile = pkgid ++ "-src.html"
  htmllink    = name pkg ++ ".html"
  metafile    = pkgid ++ ".txt"

--- Writes a file readable for all:
writeReadableFile :: String -> String -> IO ()
writeReadableFile f s = writeFile f s >> system ("chmod 644 " ++ f) >> return ()

-- Format a list of packages as an HTML table
packageInfosAsHtmlTable :: [Package] -> IO BaseHtml
packageInfosAsHtmlTable pkgs = do
  rows <- mapM formatPkgAsRow pkgs
  return $ borderedHeadedTable
    (map ((:[]) . htxt)
         ["Name", "API", "Doc","Executable","Synopsis", "Version"])
    rows
 where
  formatPkgAsRow :: Package -> IO [[BaseHtml]]
  formatPkgAsRow pkg = do
    hasapidir <- doesDirectoryExist apiDir
    hasapiidx <- doesFileExist $ apiDir </> indexhtml
    let docref = maybe [] (\r -> [hrefPrimBadge r [htxt "PDF"]]) (manualURL pkg)
    return
      [ [hrefPrimSmBlock (packageHtmlDir </> pkgid ++ ".html")
                         [htxt $ name pkg]]
      , if hasapiidx then [ehrefPrimBadge (cpmDocURL ++ pkgid </> indexhtml)
                                          [htxt "API doc"]]
                     else [nbsp]
      , if hasapidir then docref else [nbsp]
      , intercalate [nbsp]
          (map (\ (PackageExecutable n _ _) -> [kbdInput [htxt n]])
               (executableSpec pkg))
      , [htxt $ synopsis pkg]
      , [htxt $ showVersion (version pkg)] ]
   where
    pkgid     = packageId pkg
    apiDir    = "DOC" </> pkgid
    indexhtml = "index.html"

------------------------------------------------------------------------------
-- Generate HTML documentation of all packages in the central repository
generateDocsOfAllPackages :: String -> IO ()
generateDocsOfAllPackages packagedocdir = do
  (_,_,allpkgs) <- getAllPackageSpecs True
  mapM_ genDocOfPackage allpkgs
 where
  genDocOfPackage pkg = inEmptyTempDir $ do
    let pname = name pkg
        pversion = showVersion (version pkg)
    putStrLn $ unlines [dline, "Documenting: " ++ pname, dline]
    let cmd = unwords [ "rm -rf", pname, "&&"
                      , "cypm","checkout", pname, pversion, "&&"
                      , "cd", pname, "&&"
                      , "cypm", "install", "--noexec", "&&"
                      , "cypm", "doc", "--docdir", packagedocdir
                              , "--url", cpmDocURL, "&&"
                      , "cd ..", "&&"
                      , "rm -rf", pname
                      ]
    putStrLn $ "CMD: " ++ cmd
    system cmd

------------------------------------------------------------------------------
-- Run `cypm test` on all packages of the central repository
testAllPackages :: String -> IO ()
testAllPackages statdir = do
  (_,_,allpkgs) <- getAllPackageSpecs True
  results <- mapM (checkoutAndTestPackage statdir) allpkgs
  if sum (map fst results) == 0
    then putStrLn $ show (length allpkgs) ++ " PACKAGES SUCCESSFULLY TESTED!"
    else do putStrLn $ "ERRORS OCCURRED IN PACKAGES: " ++
                       unwords (map snd (filter ((> 0) . fst) results))
            exitWith 1

dline :: String
dline = take 78 (repeat '=')

------------------------------------------------------------------------------
-- Generate tar.gz files of all packages (in the current directory)
genTarOfAllPackages :: String -> IO ()
genTarOfAllPackages tardir = do
  createDirectoryIfMissing True tardir
  putStrLn $ "Generating tar.gz of all package versions in '" ++ tardir ++
             "'..."
  (cfg,allpkgversions,_) <- getAllPackageSpecs False
  allpkgs <- fromEL $ mapM (readPackageFromRepository cfg)
                        (sortBy (\ps1 ps2 -> packageId ps1 <= packageId ps2)
                                (concat allpkgversions))
  mapM_ (writePackageAsTar cfg) allpkgs --(take 3 allpkgs)
 where
  writePackageAsTar cfg pkg = do
    let pkgname  = name pkg
        pkgid    = packageId pkg
        pkgdir   = tardir </> pkgid
        tarfile  = pkgdir ++ ".tar.gz"
    putStrLn $ "Checking out '" ++ pkgid ++ "'..."
    let checkoutdir = pkgname
    system $ unwords [ "rm -rf", checkoutdir, pkgdir ]
    fromEL $ do
      acquireAndInstallPackageFromSource cfg pkg
      checkoutPackage cfg pkg
    let cmd = unwords [ "cd", checkoutdir, "&&"
                      , "tar", "cvzf", tarfile, ".", "&&"
                      , "chmod", "644", tarfile, "&&"
                      , "cd", "..", "&&", "mv", checkoutdir, pkgdir, "&&"
                      , "chmod", "-R", "go+rX", pkgdir
                      ]
    putStrLn $ "...with command:\n" ++ cmd
    ecode <- system cmd
    when (ecode>0) $ error $ "ERROR OCCURED IN PACKAGE '" ++ pkgid ++ "'!"


------------------------------------------------------------------------------
-- Add a new package (already committed and pushed into its git repo)
-- where the package specification is stored in the current directory.
addNewPackage :: Bool -> IO ()
addNewPackage withtag = do
  config <- readConfiguration
  pkg <- fromEL (loadPackageSpec ".")
  when withtag $ setTagInGit pkg
  let pkgIndexDir      = name pkg </> showVersion (version pkg)
      pkgRepositoryDir = repositoryDir config </> pkgIndexDir
      pkgInstallDir    = packageInstallDir config </> packageId pkg
  fromEL $ addPackageToRepository config "." False False
  putStrLn $ "Package repository directory '" ++ pkgRepositoryDir ++ "' added."
  (ecode,_) <- checkoutAndTestPackage "" pkg
  when (ecode>0) $ do
    removeDirectoryComplete pkgRepositoryDir
    removeDirectoryComplete pkgInstallDir
    putStrLn "Checkout/test failure, package deleted in repository directory!"
    fromEL $ updateRepository config True True True False
    exitWith 1
  putStrLn $ "\nEverything looks fine..."
  putStrLn $ "\nTo publish the new repository directory, run command:\n"
  putStrLn $ "pushd " ++ repositoryDir config ++
             " && git add " ++ pkgIndexDir </> packageSpecFile ++
             " && git commit -m\"" ++ pkgIndexDir ++ " added\" " ++
             " && git push origin master && popd"

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
-- Test a specific version of a package by checking it out in a temporary
-- directory, install it (with a local bin dir), and run all tests.
-- Returns the exit code of the package test command and the packaged id.
checkoutAndTestPackage :: String -> Package -> IO (Int,String)
checkoutAndTestPackage statdir pkg = inEmptyTempDir $ do
  putStrLn $ unlines [dline, "Testing package: " ++ pkgid, dline]
  -- create installation bin dir:
  curdir <- getCurrentDirectory
  let bindir = curdir </> "pkgbin"
  recreateDirectory bindir
  let statfile = if null statdir then "" else statdir </> pkgid ++ ".csv"
  unless (null statdir) $ createDirectoryIfMissing True statdir
  let checkoutdir = pkgname
      cmd = unwords $
              [ "rm -rf", checkoutdir, "&&"
              , "cypm", "checkout", pkgname, showVersion pkgversion, "&&"
              , "cd", checkoutdir, "&&"
              -- install possible binaries in bindir:
              , "cypm", "-d bin_install_path=" ++ bindir, "install", "&&"
              , "export PATH=" ++ bindir ++ ":$PATH", "&&"
              , "cypm", "test"] ++
              (if null statfile then [] else ["-f", statfile]) ++
              [ "&&"
              , "cypm", "-d bin_install_path=" ++ bindir, "uninstall"
              ]
  putStrLn $ "...with command:\n" ++ cmd
  ecode <- system cmd
  when (ecode>0) $ putStrLn $ "ERROR OCCURED IN PACKAGE '" ++ pkgid ++ "'!"
  return (ecode,pkgid)
 where
  pkgname     = name pkg
  pkgversion  = version pkg
  pkgid       = packageId pkg

------------------------------------------------------------------------------
-- Combine all CSV statistics files for packages (produced by
-- `cypm test -f ...`) contained in a directory into a result file
-- and sum up the results.
sumCSVStatsOfPkgs :: String -> String -> IO ()
sumCSVStatsOfPkgs dir outfile = do
  combineCSVFilesInDir readStats showResult addStats ([],[]) dir outfile
  putStrLn $ "All results written to file '" ++ outfile ++ "'."
 where
  readStats rows =
    let [pkgid,ct,rc,total,unit,prop,eqv,io,mods] = rows !! 1
    in (rows !! 0,
        [ (pkgid, ct,
           map (\s -> read s :: Int) [rc,total,unit,prop,eqv,io], mods) ])

  showResult (header,rows) =
    header :
    sortBy (<=)
           (map (\(pkgid,ct,nums,mods) -> pkgid : ct : map show nums ++ [mods])
                rows) ++
    ["TOTAL:" : "" :
      map show
          (foldr1 (\nums1 nums2 -> map (uncurry (+)) (zip nums1 nums2))
                  (map (\ (_,_,ns,_) -> ns) rows))]

  addStats (header,rows1) (_,rows2) = (header, rows1 ++ rows2)

-- Combine all CSV files contained in a directory into one result CSV file
-- according to an operation to read the contents of each CSV file,
-- an operation to write the result into CSV format,
-- an operation to combine the results, and a default value.
combineCSVFilesInDir :: ([[String]] -> a) -> (a -> [[String]]) -> (a -> a -> a)
                     -> a -> String -> String -> IO ()
combineCSVFilesInDir fromcsv tocsv combine emptycsv statdir outfile = do
  dcnts <- getDirectoryContents statdir
  let csvfiles = map (statdir </>) (filter (".csv" `isSuffixOf`) dcnts)
  stats <- mapM (\f -> readCompleteFile f >>= return . fromcsv . readCSV)
                csvfiles
  let results = foldr combine emptycsv stats
  writeCSVFile outfile (tocsv results)

------------------------------------------------------------------------------
-- Re-tag the current git version with the current package version
-- and copy the package spec file to the cpm index
updatePackage :: IO ()
updatePackage = do
  config <- readConfiguration
  pkg <- fromEL (loadPackageSpec ".")
  let pkgInstallDir    = packageInstallDir config </> packageId pkg
  setTagInGit pkg
  putStrLn $ "Deleting old repo copy '" ++ pkgInstallDir ++ "'..."
  removeDirectoryComplete pkgInstallDir
  (ecode,_) <- checkoutAndTestPackage "" pkg
  when (ecode > 0) $ do removeDirectoryComplete pkgInstallDir
                        putStrLn $ "ERROR in package, CPM index not updated!"
                        exitWith 1
  fromEL $ addPackageToRepository config "." True False

------------------------------------------------------------------------------
-- Show package dependencies as dot graph
showAllPackageDependencies :: IO ()
showAllPackageDependencies = do
  pkgs <- getAllPackages
  let alldeps = map (\p -> (name p, map (\ (Dependency p' _) -> p')
                                        (dependencies p))) pkgs
      dotgraph = depsToGraph alldeps
  putStrLn $ "Show dot graph..."
  viewDotGraph dotgraph

depsToGraph :: [(String, [String])] -> DotGraph
depsToGraph cpmdeps =
  dgraph "CPM Dependencies"
    (map (\s -> Node s []) (nub (map fst cpmdeps ++ concatMap snd cpmdeps)))
    (map (\ (s,t) -> Edge s t [])
         (nub (concatMap (\ (p,ds) -> map (\d -> (p,d)) ds) cpmdeps)))

-- Write package dependencies into CSV file 'pkgs.csv'
writeAllPackageDependencies :: IO ()
writeAllPackageDependencies = do
  (_,_,pkgs) <- getAllPackageSpecs True
  let alldeps = map (\p -> (name p, map (\ (Dependency p' _) -> p')
                                        (dependencies p)))
                    pkgs
  writeCSVFile "pkgs.csv" (map (\ (p,ds) -> p:ds) alldeps)
  putStrLn $ "Package dependencies written to 'pkgs.csv'"

------------------------------------------------------------------------------
-- Copy all package documentations from directory `packagedocdir` into
-- the directory `currygleDocDir` so that the documentations
-- can be used by Currygle to generate the documentation index
copyPackageDocumentations :: String -> IO ()
copyPackageDocumentations packagedocdir = do
  allpkgs <- getAllPackages
  let pkgs   = map sortVersions (groupBy (\a b -> name a == name b) allpkgs)
      pkgids = sortBy (\xs ys -> head xs <= head ys) (map (map packageId) pkgs)
  putStrLn $ "Number of package documentations: " ++ show (length pkgs)
  recreateDirectory currygleDocDir
  mapM_ copyPackageDoc pkgids
 where
  sortVersions ps = sortBy (\a b -> version a `vgt` version b) ps

  copyPackageDoc [] = return ()
  copyPackageDoc (pid:pids) = do
    let pdir = packagedocdir </> pid
    exdoc <- doesDirectoryExist pdir
    if exdoc
      then do putStrLn $ "Copying documentation of " ++ pid ++ "..."
              copyDirectory pdir (currygleDocDir </> pid)
      else
        if null pids
          then putStrLn $ "Documentation " ++ pid ++ " does not exist!"
          else copyPackageDoc pids

------------------------------------------------------------------------------
--- Returns all packages where in each package
--- the name, version, dependencies, and compilerCompatibility is set.
getAllPackages :: IO [Package]
getAllPackages = do
  config <- readConfiguration
  fromEL (getBaseRepository config >>= return . allPackages)

--- Reads to the .cpmrc file from the user's home directory and return
--- the configuration. Terminate in case of some errors.
readConfiguration :: IO Config
readConfiguration = do
  c <- fromEL $ readConfigurationWith []
  case c of
    Left err -> do putStrLn $ "Error reading .cpmrc file: " ++ err
                   exitWith 1
    Right c' -> return c'

--- Prints the current configuration.
printConfig :: IO ()
printConfig = do
  cfg <- readConfiguration
  putStr $ unlines [banner, "Current configuration:", "", showConfiguration cfg]
  (_,allpkgversions,allcompatpkgs) <- getAllPackageSpecs True
  putStrLn $ "\nNewest compatible packages:\n" ++
             unwords (map packageId allcompatpkgs)
  let allpkgs  = concatMap (take 1) allpkgversions
      incnames = map name allpkgs \\ map name allcompatpkgs
  putStrLn $ "\nIncompatible packages:\n" ++ unwords (sort incnames)

--- Executes an IO action with the current directory set to a new empty
--- temporary directory. After the execution, the temporary directory
--- is deleted.
inEmptyTempDir :: IO a -> IO a
inEmptyTempDir a = do
  tmp <- newTempDir
  createDirectoryIfMissing True tmp
  r  <- inDirectory tmp a
  removeDirectoryComplete tmp
  return r

--- Returns a new temporary directory.
newTempDir :: IO String
newTempDir = do
  t   <- getTemporaryDirectory
  pid <- getPID
  getNewDir (t </> "cpm" ++ show pid) 0
 where
  getNewDir base i = do
    let tmpdir = base ++ show i
    exdir <- doesDirectoryExist tmpdir
    if exdir then getNewDir base (i+1)
             else return tmpdir

------------------------------------------------------------------------------
-- The name of the package specification file.
packageSpecFile :: String
packageSpecFile = "package.json"

------------------------------------------------------------------------------
-- Generates a HTML representation of the contents of a directory.
directoryContentsPage :: String -> String -> IO String
directoryContentsPage base dir = do
  time <- getLocalTime
  maindoc <- directoryContentsAsHTML 1 base dir
  let btbase = "../bt4"
  return $ showHtmlPage $
    bootstrapPage (favIcon btbase) (cssIncludes btbase) (jsIncludes btbase)
      ("Browse " ++ dir) homeBrand
      (leftTopMenu True (-1)) rightTopMenu 0 []
      [h1 [smallMutedText "Contents of ", htxt dir]]
      maindoc (curryDocFooter time)

directoryContentsAsHTML :: Int -> String -> String -> IO [BaseHtml]
directoryContentsAsHTML d base dir = do
  exdir <- doesDirectoryExist basedir
  if exdir
    then do
      dirfiles <- getDirectoryContents basedir >>= return . filter isReal
      if null dirfiles
        then return []
        else do
          ls <- mapM dirElemAsHTML (sortBy (<=) dirfiles)
          let (files,dirs) = partition (\hes -> length hes == 1) ls
          return [ulist (files ++ dirs) `addAttr` ("style","list-style: none")]
    else return []
 where
  basedir = base </> dir

  dirElemAsHTML df = do
    isdir <- doesDirectoryExist (basedir </> df)
    if isdir && (d < 10) -- to avoid very deep (infinite) dir trees
      then do subdir <- directoryContentsAsHTML (d+1) basedir df
              return [code [htxt (df ++ "/")], block subdir]
      else return [code [href (basedir </> df)
                              [htxt $ df ++ if isdir then "/" else ""]]]

  isReal fn = not ("." `isPrefixOf` fn)

------------------------------------------------------------------------------
--- Transform an error logger action into a standard IO action.
fromEL :: ErrorLogger a -> IO a
fromEL = fromErrorLogger Info False
-- To show debug infos and timings, use: 
--fromEL = fromErrorLogger Debug True

------------------------------------------------------------------------------
