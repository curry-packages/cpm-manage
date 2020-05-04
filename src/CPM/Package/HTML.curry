------------------------------------------------------------------------------
--- This module contains some operations to generate HTML representations
--- of packages.
------------------------------------------------------------------------------

module CPM.Package.HTML
  where

import Char      ( isSpace )
import Directory ( doesDirectoryExist, doesFileExist )
import FilePath  ( (</>), replaceExtension )
import IOExts    ( readCompleteFile )
import List      ( find, intersperse, isPrefixOf, splitOn )
import Time      ( CalendarTime, calendarTimeToString, getLocalTime )

import HTML.Base
import HTML.Styles.Bootstrap4
import Text.CSV                ( readCSV )

import CPM.Package
import CPM.Package.Helpers     ( renderPackageInfo )

import CPM.Manage.Config       ( cpmDocURL, kics2URL, pakcsURL )

------------------------------------------------------------------------------
--- Generate HTML page string for a given package.
packageToHTML :: [[Package]] -> [Package] -> Package -> IO String
packageToHTML allpkgversions newestpkgs pkg = do
  hasapi     <- doesDirectoryExist apiDir
  hasreadme  <- doesFileExist readmefile
  hasreadmei <- doesFileExist readmeifile
  readmei    <- if hasreadmei then readFile readmeifile else return ""
  mbtested   <- getTestResults pkgid
  let apilinks = if hasapi
                   then [ehref (cpmDocURL ++ pkgid)
                                  [htxt "API documentation"]] ++
                        maybe []
                              (\mref -> [href mref [htxt "Manual (PDF)"]])
                              (manualURL pkg)
                   else []
      infomenu = (if hasreadme
                    then [ehref ("../" ++ readmefile) [htxt "README"]]
                    else []) ++
                 [ehref (pkgid ++ ".txt") [htxt "Package specification"]] ++
                 apilinks
      mbdocurl = if hasapi then Just (cpmDocURL ++ pkgid) else Nothing
      sidenav =
        [ulistWithClass "list-group" "list-group-item"
           (map (\ (t,c) -> (h5 [htxt t] : c))
                (packageInfoAsHTML allpkgversions newestpkgs pkg mbdocurl ++
                   [("Further infos:",
                     [ulistWithClass "nav flex-column" "nav-item"
                                     (map addNavLink infomenu)])]))] ++
        (maybe [] (\s -> [blockstyle "badge badge-success" [htxt s]]) mbtested)
  let pkgdesc =  (if hasreadmei then [HtmlText readmei] else []) ++
                 [hrule,
                  h2 [htxt "Download"],
                  dlist (map (\ (l,hs) -> ([htxt (l++": ")],hs))
                             (pkgtarref ++ showPkgSource pkg))]
  cpmPackagePage pname sidenav (map addNavLink apilinks) pkgdesc
 where
  addNavLink h = [h `addClass` "nav-link"]

  pname       = name pkg
  pkgid       = packageId pkg
  apiDir      = "DOC" </> pkgid
  readmefile  = apiDir </> "README.html"
  readmeifile = apiDir </> "README_I.html"
  pkgtar      = pkgid ++ ".tar.gz"
  pkgtarref   = [("Checkout with CPM",
                  [kbdInput [htxt $ "cypm checkout " ++ pname ++ " " ++
                                    showVersion (version pkg)]]),
                 ("Package source", 
                  [ehref (".." </> "PACKAGES" </> pkgtar) [htxt pkgtar],
                   htxt " [", href (pkgid ++ "-src.html") [htxt "browse"],
                   htxt "]"])]

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
  Nothing -> Nothing
  Just (PackageDocumentation _ docmain _) ->
    Just (cpmDocURL ++ packageId pkg </> replaceExtension docmain ".pdf")

------------------------------------------------------------------------------
--- Renders information about a package as HTML description list.
packageInfoAsHTML :: [[Package]] -> [Package] -> Package -> Maybe String
                  -> [(String,[HtmlExp])]
packageInfoAsHTML allpkgversions newestpkgs pkg mbdocurl =
  [ ("Synopsis", [ htxt (synopsis pkg) ]) ] ++
  cats ++
  [ ("Versions", hitems $ map (showPkgVersion pkg) pkgversions)
  , ("Dependencies", hitems $ map dep2html $ dependencies pkg)
  , ("Author", auth) ] ++
  maintnr ++
  compilers ++
  expmods ++
  executable ++
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
    let cats = category pkg
    in if null cats
         then []
         else [("Categor" ++ if length cats == 1 then "y" else "ies",
                hitems $
                   map (\c -> hrefPrimBadge ("../indexc.html#" ++ c) [htxt c])
                       cats)]

  dep2html dep@(Dependency dp vcs) =
    maybe (htxt $ showDependency dep)
          (\np -> hrefPrimBadge (packageId np ++ ".html")
                    [htxt dp, nbsp,
                     showConstraintBadge (showVersionConstraints vcs)])
          (find (\p -> name p == dp) newestpkgs)

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

  executable =
    maybe []
          (\ (PackageExecutable n _ _) ->
             [("Executable installed by package", [kbdInput [htxt n]])])
          (executableSpec pkg)

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
hitems :: [HtmlExp] -> [HtmlExp]
hitems = intersperse (htxt " ")

--- Vertical placement of HTML expressions.
vitems :: [HtmlExp] -> [HtmlExp]
vitems = intersperse breakline

showPkgVersion :: Package -> Version -> HtmlExp
showPkgVersion pkg v =
  (if version pkg == v then hrefPrimBadge else hrefScndBadge)
    (name pkg ++ "-" ++ vers ++ ".html") [htxt vers]
 where
  vers = showVersion v

showPkgSource :: Package -> [(String,[HtmlExp])]
showPkgSource pkg = case source pkg of
  Just (Git url _) -> [("Source repository", [showURL url])]
  Just (Http url ) -> [("Source", [showURL url])]
  _                -> []

showCompilerReq :: CompilerCompatibility -> HtmlExp
showCompilerReq (CompilerCompatibility cc vcs)
  | cc == "pakcs"
  = ehrefSuccBadge pakcsURL
          [htxt cc, nbsp, showConstraintBadge (showVersionConstraints vcs)]
  | cc == "kics2"
  = ehrefWarnBadge kics2URL
          [htxt cc, nbsp, showConstraintBadge (showVersionConstraints vcs)]
  | otherwise
  = textstyle "badge badge-secondary" (cc ++ " " ++ showVersionConstraints vcs)

showConstraintBadge :: String -> HtmlExp
showConstraintBadge = textstyle "badge badge-light" 

showURL :: String -> HtmlExp
showURL s | "http" `isPrefixOf` s = ehref s [htxt s]
          | otherwise             = htxt s

--------------------------------------------------------------------------
-- Auxiliary operations to support generated HTML pages.

--- Standard HTML page for generated package descriptions.
cpmPackagePage :: String -> [HtmlExp] -> [[HtmlExp]] -> [HtmlExp] -> IO String
cpmPackagePage title sidenav apilinks maindoc = do
  let htmltitle = [h1 [smallMutedText "Curry Package ", htxt title]]
  time <- getLocalTime
  let btbase = "../bt4"
  return $ showHtmlPage $
    bootstrapPage (favIcon btbase) (cssIncludes btbase) (jsIncludes btbase)
                  title homeBrand
                  (leftTopMenu True (-1)) (apilinks ++ rightTopMenu) 4 sidenav
                  htmltitle maindoc (curryDocFooter time)


--- A small muted text (used in the title):
smallMutedText :: String -> HtmlExp
smallMutedText s = HtmlStruct "small" [("class","text-muted")] [htxt s]

--- The URL of the Curry homepage
curryHomeURL :: String
curryHomeURL = "http://www.curry-lang.org"

--- The URL of CPM
cpmHomeURL :: String
cpmHomeURL = "http://www.curry-lang.org/tools/cpm"

-- The URL of the favicon relative to the base directory of BT4.
favIcon :: String -> String
favIcon btdir = btdir </> "img" </> "favicon.ico"

-- The CSS includes relative to the base directory of BT4.
cssIncludes :: String -> [String]
cssIncludes btdir =
  map (\n -> btdir </> "css" </> n ++ ".css") ["bootstrap.min","cpm"]

-- The JavaScript includes relative to the base directory of BT4.
jsIncludes :: String -> [String]
jsIncludes btdir =
   ["https://code.jquery.com/jquery-3.4.1.slim.min.js",
    btdir </> "js/bootstrap.bundle.min.js"]

homeBrand :: (String,[HtmlExp])
homeBrand = (cpmHomeURL, [htxt "Curry Package Repository"])

--- The standard left top menu.
--- The first argument is true if we are inside a package documentation.
--- The second argument indicates the index of the active link
--- (negative value = no active link)
leftTopMenu :: Bool -> Int -> [[HtmlExp]]
leftTopMenu inpkg actindex =
  [ [mhref 0 "index.html"  "Packages"]
  , [mhref 1 "indexv.html" "Versions"]
  , [mhref 2 "indexc.html" "Categories"]
  ]
 where
  mhref i url txt = (if i == actindex then hrefNavActive else hrefNav)
                       (if inpkg then ".." </> url else url) [htxt txt]

--- The standard right top menu.
rightTopMenu :: [[HtmlExp]]
rightTopMenu =
  [ [ehrefNav curryHomeURL [htxt "Curry Homepage"]]
  ]

--------------------------------------------------------------------------
-- Standard footer information for generated web pages:
curryDocFooter :: CalendarTime -> [HtmlExp]
curryDocFooter time =
  [italic [htxt "Generated by cpm-manage at ",
           htxt (calendarTimeToString time)]]

--- A bordered table:
borderedTable :: [[[HtmlExp]]] -> HtmlExp
borderedTable rows =
  table rows `addClass` "table table-bordered table-hover"

--- A bordered headed table:
borderedHeadedTable :: [[HtmlExp]] -> [[[HtmlExp]]] -> HtmlExp
borderedHeadedTable headrow rows =
  headedTable headrow rows `addClass` "table table-bordered table-hover"

--- Headed table with a header row and a matrix of items.
--- Each item is a list of HTML expressions.
headedTable :: [[HtmlExp]] -> [[[HtmlExp]]] -> HtmlExp
headedTable headrow items =
  HtmlStruct "table" []
    [HtmlStruct "thead" [("class","thead-light")] [toRow "th" headrow],
     HtmlStruct "tbody" [] (map (toRow "td") items)]
 where
  toRow ti row = HtmlStruct "tr" [] (map (\item -> HtmlStruct ti [] item) row)

------------------------------------------------------------------------------
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------------------------
