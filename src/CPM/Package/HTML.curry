------------------------------------------------------------------------------
--- This module contains operations to generate HTML representations of
--- packages.
------------------------------------------------------------------------------

module CPM.Package.HTML
  where

import Data.Char        ( isSpace )
import Data.List        ( find, intercalate, intersperse, isPrefixOf, splitOn )

import Data.Time        ( CalendarTime, calendarTimeToString, getLocalTime
                        , toUTCTime )
import HTML.Base
import HTML.Styles.Bootstrap4
import Language.Curry.Resources ( curryHomeURL, kics2URL, pakcsURL, curry2goURL
                                , cpmHomeURL, curryPackagesURL
                                , curryPackagesDocURL, masalaHomeURL )
import System.Directory ( doesDirectoryExist, doesFileExist
                        , getModificationTime )
import System.FilePath  ( (</>), replaceExtension )
import System.IOExts    ( readCompleteFile )
import Text.CSV         ( readCSV )

import CPM.Package
import CPM.Package.Helpers  ( renderPackageInfo )

------------------------------------------------------------------------------
--- The base directory of the CurryInfo HTML pages.
curryInfoHtmlBase :: String
curryInfoHtmlBase = "/var/www/webapps/curry-info/HTML"

--- The base URL of the CurryInfo HTML pages.
curryInfoHtmlURL :: String
curryInfoHtmlURL = "https://cpm.curry-lang.org/webapps/curry-info/HTML"

------------------------------------------------------------------------------
--- Generate HTML page string for a given package.
packageToHTML :: [[Package]] -> Package -> IO String
packageToHTML allpkgversions pkg = do
  hasapidir  <- doesDirectoryExist apiDir
  hasaindex  <- doesFileExist $ apiDir </> indexhtml
  hasreadme  <- doesFileExist readmefile
  hasreadmei <- doesFileExist readmeifile
  readmei    <- if hasreadmei then readFile readmeifile else return ""
  let cidir  = "packages" </> pname </> "versions" </> pversion
  hascinfo   <- doesDirectoryExist $ curryInfoHtmlBase </> cidir
  mbpkgtime  <- getUploadTime pkg
  mbtested   <- getTestResults pkgid
  let apilinks = (if hasaindex
                    then [ehref (curryPackagesDocURL ++ pkgid </> indexhtml)
                                [htxt "API documentation"]]
                    else []) ++
                 (if hasapidir
                    then maybe []
                               (\mref -> [href mref [htxt "Manual (PDF)"]])
                               (manualURL pkg)
                    else [])
      cilink   = if hascinfo then [ehref (curryInfoHtmlURL </> cidir) 
                                         [htxt "Analysis information"]]
                             else []
      infomenu = (if hasreadme
                    then [ehref ("../" ++ readmefile) [htxt "README"]]
                    else []) ++
                 [ehref (pkgid ++ ".txt") [htxt "Package specification"]] ++
                 apilinks ++
                 [href (pname ++ "-deps.html") [htxt "Package dependencies"]] ++
                 cilink
      mbdocurl = if hasapidir then Just $ curryPackagesDocURL ++ pkgid
                              else Nothing
      sidenav =
        [ulistWithClass "list-group" "list-group-item"
           (map (\ (t,c) -> (h5 [htxt t] : c))
                (packageInfoAsHTML allpkgversions pkg mbdocurl ++
                   [("Further infos:",
                     [ulistWithClass "nav flex-column" "nav-item"
                                     (map addNavLink infomenu)])]))] ++
        (maybe [] (\t -> [blockstyle "badge badge-secondary"
                            [htxt $ "Uploaded at " ++
                                    calendarTimeToString t ++ " (UTC)"]])
                  mbpkgtime) ++
        (maybe [] (\s -> [blockstyle "badge badge-success" [htxt s]])
                  mbtested)
  let pkgdesc =  (if hasreadmei then [htmlText readmei] else []) ++
                 [hrule,
                  h2 [htxt "Download"],
                  dlist (map (\ (l,hs) -> ([htxt (l++": ")],hs))
                             (pkgtarref ++ showPkgSource pkg))]
  cpmPackagePage pname sidenav (map addNavLink apilinks) pkgdesc
 where
  addNavLink h = [h `addClass` "nav-link"]

  pname       = name pkg
  pkgid       = packageId pkg
  pversion    = showVersion (version pkg)
  apiDir      = "DOC" </> pkgid
  indexhtml   = "index.html"
  readmefile  = apiDir </> "README.html"
  readmeifile = apiDir </> "README_I.html"
  pkgtar      = pkgid ++ ".tar.gz"
  pkgtarref   = [("Checkout with CPM",
                  [kbdInput
                    [htxt $ "cypm checkout " ++ pname ++ " " ++ pversion]]),
                 ("Package source", 
                  [ehref (".." </> "PACKAGES" </> pkgtar) [htxt pkgtar],
                   htxt " [", href (pkgid ++ "-src.html") [htxt "browse"],
                   htxt "]"])]

-- Get the upload time of the package (i.e., the time of the tar file).
getUploadTime :: Package -> IO (Maybe CalendarTime)
getUploadTime pkg = do
  hastar <- doesFileExist pkgtarpath
  if hastar then do ct <- getModificationTime pkgtarpath
                    return $ Just $ toUTCTime ct
            else return Nothing
 where
  pkgtar     = packageId pkg ++ ".tar.gz"
  pkgtarpath = "PACKAGES" </> pkgtar

-- Get some string describing a successful test.
getTestResults :: String -> IO (Maybe String)
getTestResults pkgid = do
  let testfile = "TEST" </> pkgid ++ ".csv"
  hastests <- doesFileExist testfile
  if hastests
    then do
      tinfos <- readCompleteFile testfile >>= return . readCSV
      case tinfos of
        [_, (_:ct:rc:_)] | rc == "0" -> return $ Just $
                                          "Succesfully tested at " ++ ct
        _                          -> return Nothing
    else return Nothing

--- Manual URL of a package (if specified in package).
manualURL :: Package -> Maybe String
manualURL pkg = case documentation pkg of
  Nothing                                 -> Nothing
  Just (PackageDocumentation _ docmain _) ->
    Just (curryPackagesDocURL ++ packageId pkg </>
          replaceExtension docmain ".pdf")

------------------------------------------------------------------------------
--- Renders information about a package as HTML description list.
packageInfoAsHTML :: [[Package]] -> Package -> Maybe String
                  -> [(String,[BaseHtml])]
packageInfoAsHTML allpkgversions pkg mbdocurl =
  [ ("Synopsis", [ htxt (synopsis pkg) ]) ] ++
  cats ++
  [ ("Versions", hitems $ map (showPkgVersion pkg) pkgversions)
  , ("Dependencies", hitems $ map dep2html $ dependencies pkg)
  , ("Author", auth) ] ++
  maintnr ++
  compilers ++
  expmods ++
  executables ++
  showParaField description "Description" ++
  showLicense ++
  showParaField copyright   "Copyright" ++
  showUrlField  homepage    "Homepage" ++
  showUrlField  repository  "Repository" ++
  showUrlField  bugReports  "Bug reports"
 where
  pkgversions = maybe []
                      (map version)
                      (find (\pg -> not (null pg) && name (head pg) == name pkg)
                            allpkgversions)

  auth = vitems $ map (htxt . strip) (concatMap (splitOn ",") $ author pkg)

  maintnr = case maintainer pkg of
    [] -> []
    xs -> [("Maintainer", vitems $ map htxt (concatMap (splitOn ",") xs))]

  cats =
    let pcats = category pkg
    in if null pcats
         then []
         else [("Categor" ++ if length pcats == 1 then "y" else "ies",
                hitems $
                   map (\c -> hrefPrimBadge ("../indexc.html#" ++ c) [htxt c])
                       pcats)]

  dep2html dep@(Dependency dp vcs) =
    maybe (htxt $ showDependency dep)
          (\_ -> hrefPrimBadge (dp ++ ".html")
                   [htxt dp, nbsp,
                    showConstraintBadge (showVersionConstraints vcs)])
          (find (\pgs -> not (null pgs) && name (head pgs) == dp)
                allpkgversions)

  compilers =
    if null (compilerCompatibility pkg)
      then []
      else [("Compiler requirements",
             hitems $ map showCompilerReq $ compilerCompatibility pkg)]

  expmods =
    if null (exportedModules pkg)
      then []
      else [("Exported modules",
             hitems $
               map (\m -> code [maybe (htxt m)
                                      (\u -> ehrefPrimBadge (u </> m ++ ".html")
                                                            [htxt m])
                                      mbdocurl])
                   (exportedModules pkg))]

  executables = case executableSpec pkg of
    []   -> []
    [ex] -> [("Executable installed by package", getExName ex)]
    exs  -> [("Executables installed by package",
              intercalate [nbsp] (map getExName  exs))]
   where
    getExName (PackageExecutable n _ _) = [kbdInput [htxt n]]

  showLicense =
   let lkind = case license pkg of
                 Nothing -> []
                 Just s  -> [htxt s]
       lfile = case licenseFile pkg of
                 Nothing -> []
                 Just f  -> [ehref (".." </> "PACKAGES" </> packageId pkg </> f)
                                   [htxt "License file"]]
   in [("License", intersperse (htxt " / ") (lkind ++ lfile))]

  showUrlField fgetter fname = case fgetter pkg of
    Nothing -> []
    Just  s -> [(fname, [showURL s])]

  showParaField fgetter fname = case fgetter pkg of
    Nothing -> []
    Just  s -> [(fname, [htxt s])]

--- Horizontal placement of HTML expressions separated by blanks.
hitems :: [BaseHtml] -> [BaseHtml]
hitems = intersperse (htxt " ")

--- Vertical placement of HTML expressions.
vitems :: [BaseHtml] -> [BaseHtml]
vitems = intersperse breakline

showPkgVersion :: Package -> Version -> BaseHtml
showPkgVersion pkg v =
  (if version pkg == v then hrefPrimBadge else hrefScndBadge)
    (name pkg ++ "-" ++ vers ++ ".html") [htxt vers]
 where
  vers = showVersion v

showPkgSource :: Package -> [(String,[BaseHtml])]
showPkgSource pkg = case source pkg of
  Just (Git url _) -> [("Source repository", [showURL url])]
  Just (Http url ) -> [("Source", [showURL url])]
  _                -> []

showCompilerReq :: CompilerCompatibility -> BaseHtml
showCompilerReq (CompilerCompatibility cc vcs)
  | cc == "pakcs"
  = ehrefSuccBadge pakcsURL
          [htxt cc, nbsp, showConstraintBadge (showVersionConstraints vcs)]
  | cc == "kics2"
  = ehrefWarnBadge kics2URL
          [htxt cc, nbsp, showConstraintBadge (showVersionConstraints vcs)]
  | cc == "curry2go"
  = ehrefInfoBadge curry2goURL
          [htxt cc, nbsp, showConstraintBadge (showVersionConstraints vcs)]
  | otherwise
  = textstyle "badge badge-secondary" (cc ++ " " ++ showVersionConstraints vcs)

showConstraintBadge :: String -> BaseHtml
showConstraintBadge = textstyle "badge badge-light" 

showURL :: String -> BaseHtml
showURL s | "http" `isPrefixOf` s = ehref s [htxt s]
          | otherwise             = htxt s

--------------------------------------------------------------------------
-- Auxiliary operations to support generated HTML pages.

--- Standard HTML page for generated package descriptions.
cpmPackagePage :: String -> [BaseHtml] -> [[BaseHtml]] -> [BaseHtml]
               -> IO String
cpmPackagePage pkgname sidenav apilinks maindoc = do
  let htmltitle = [h1 [smallMutedText "Curry Package ", htxt pkgname]]
  time <- getLocalTime
  let btbase = "../bt4"
  return $ showHtmlPage $ bootstrapPage
    (favIcon btbase) (cssIncludes btbase) (jsIncludes btbase)
    pkgname
    (pkgname ++ ".html", [htxt $ "Package", nbsp, code [htxt pkgname]])
    (apilinks ++ [[nbsp, nbsp, nbsp]] ++ leftTopMenu True (-1))
    rightTopMenu 4 sidenav
    htmltitle maindoc (curryDocFooter time)


--- A small muted text (used in the title):
smallMutedText :: String -> BaseHtml
smallMutedText s = htmlStruct "small" [("class","text-muted")] [htxt s]

-- The URL of the favicon relative to the base directory of BT4.
favIcon :: String -> String
favIcon btdir = btdir </> "img" </> "favicon.ico"

-- The CSS includes relative to the base directory of BT4.
cssIncludes :: String -> [String]
cssIncludes btdir =
  map (\n -> btdir </> "css" </> n ++ ".css") ["bootstrap.min", "cpm"]

-- The JavaScript includes relative to the base directory of BT4.
jsIncludes :: String -> [String]
jsIncludes btdir =
   ["https://code.jquery.com/jquery-3.4.1.slim.min.js",
    btdir </> "js/bootstrap.bundle.min.js"]

packagesHomeBrand :: (String,[BaseHtml])
packagesHomeBrand = (curryPackagesURL </> "index.html", [htxt "Curry Packages"])

--- The standard left top menu.
--- The first argument is true if we are inside a package documentation.
--- The second argument indicates the index of the active link
--- (negative value = no active link)
leftTopMenu :: Bool -> Int -> [[BaseHtml]]
leftTopMenu inpkg actindex =
  [ [mhref 0 "index.html"  "All Packages"]
  , [mhref 2 "indexc.html" "Categories"]
  , [mhref 1 "indexv.html" "Recent Uploads"]
  ]
 where
  mhref i url txt = (if i == actindex then hrefNavActive else hrefNav)
                       (if inpkg then ".." </> url else url) [htxt txt]

--- The standard right top menu.
rightTopMenu :: [[BaseHtml]]
rightTopMenu =
  [ [ehrefNav masalaHomeURL [htxt "Masala"]]
  , [ehrefNav cpmHomeURL    [htxt "Curry Package Manager"]]
  , [ehrefNav curryHomeURL  [htxt "Curry Homepage"]]
  ]

--------------------------------------------------------------------------
-- Standard footer information for generated web pages:
curryDocFooter :: CalendarTime -> [BaseHtml]
curryDocFooter time =
  [italic [htxt "Generated by cpm-manage at ",
           htxt (calendarTimeToString time)]]

--- A bordered table:
borderedTable :: [[[BaseHtml]]] -> BaseHtml
borderedTable rows =
  table rows `addClass` "table table-bordered table-hover"

--- A bordered headed table:
borderedHeadedTable :: [[BaseHtml]] -> [[[BaseHtml]]] -> BaseHtml
borderedHeadedTable headrow rows =
  headedTable headrow rows `addClass` "table table-bordered table-hover"

--- Headed table with a header row and a matrix of items.
--- Each item is a list of HTML expressions.
headedTable :: [[BaseHtml]] -> [[[BaseHtml]]] -> BaseHtml
headedTable headrow items =
  htmlStruct "table" []
    [htmlStruct "thead" [("class","thead-light")] [toRow "th" headrow],
     htmlStruct "tbody" [] (map (toRow "td") items)]
 where
  toRow ti row = hStruct "tr" (map (hStruct ti) row)

------------------------------------------------------------------------------
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------------------------
