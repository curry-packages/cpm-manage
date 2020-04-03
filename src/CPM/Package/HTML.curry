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
import HTML.Styles.Bootstrap3  ( bootstrapPage, glyphicon, homeIcon )
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
                   then [[ehref (cpmDocURL ++ pkgid)
                                [htxt "API documentation"]]] ++
                        maybe []
                              (\mref -> [[href mref [htxt "Manual (PDF)"]]])
                              (manualURL pkg)
                   else []
      infomenu = (if hasreadme
                    then [[ehref ("../" ++ readmefile) [htxt "README"]]]
                    else []) ++
                 [[ehref (pkgid ++ ".txt") [htxt "Package specification"]]] ++
                 apilinks
      mbdocurl = if hasapi then Just (cpmDocURL ++ pkgid) else Nothing
      sidenav =
        [ dlist (map (\ (t,c) -> ([htxt t], c))
                     (packageInfoAsHTML allpkgversions newestpkgs pkg mbdocurl))
        , bold [htxt "Further infos:"]
        , ulist infomenu `addClass` "nav nav-sidebar"
        ] ++
        (maybe [] (\s -> [blockstyle "label label-success" [htxt s]]) mbtested)
  let pkgdesc =  (if hasreadmei then [HtmlText readmei] else []) ++
                 [hrule,
                  h2 [htxt "Download"],
                  dlist (map (\ (l,hs) -> ([htxt (l++": ")],hs))
                             (pkgtarref ++ showPkgSource pkg))
                 ]
  cpmPackagePage ("Curry Package '" ++ pname ++ "'") sidenav apilinks pkgdesc
 where
  pname       = name pkg
  pkgid       = packageId pkg
  apiDir      = "DOC" </> pkgid
  readmefile  = apiDir </> "README.html"
  readmeifile = apiDir </> "README_I.html"
  pkgtar      = pkgid ++ ".tar.gz"
  pkgtarref   = [("CPM package source", 
                  [ehref ("../PACKAGES" </> pkgtar) [htxt pkgtar]])]

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
    if null (category pkg)
      then []
      else [("Category",
             hitems $ map (\c -> hrefPrimXs ("../indexc.html#" ++ c) [htxt c])
                          (category pkg))]

  dep2html dep@(Dependency dp vcs) =
    maybe (htxt $ showDependency dep)
          (\np -> hrefPrimXs (packageId np ++ ".html")
                    [htxt dp, nbsp,
                     textstyle "badge" (showVersionConstraints vcs)])
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
             hitems $ map (\m -> code [maybe (htxt m)
                                         (\u -> ehrefPrimXs (u </> m ++ ".html")
                                                            [htxt m])
                                         mbdocurl])
                          (exportedModules pkg))]

  executable =
    maybe []
          (\ (PackageExecutable n _ _) ->
             [("Executable installed by package",
               [nbsp, kbd [htxt n]])])
          (executableSpec pkg)

  showLicense =
   let lkind = case license pkg of
                 Nothing -> []
                 Just s  -> [htxt s]
       lfile = case licenseFile pkg of
                 Nothing -> []
                 Just f  -> [ehref ("../PACKAGES" </> packageId pkg </> f)
                                   [htxt "License file"]]
   in [("License", nbsp : intersperse (htxt " / ") (lkind ++ lfile))]

  showUrlField fgetter fname = case fgetter pkg of
    Nothing -> []
    Just  s -> [(fname, [nbsp, showURL s])]

  showParaField fgetter fname = case fgetter pkg of
    Nothing -> []
    Just  s -> [(fname, [htxt s])]

--- Horizontal placement of HTML expressions separated by blanks.
hitems :: [HtmlExp] -> [HtmlExp]
hitems = intersperse (htxt " ")

--- Vertical placement of HTML expressions.
vitems :: [HtmlExp] -> [HtmlExp]
vitems = intersperse breakline . map (\x -> inline [nbsp,x])

showPkgVersion :: Package -> Version -> HtmlExp
showPkgVersion pkg v =
  (if version pkg == v then hrefPrimXs else hrefDfltXs)
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
  = ehrefSuccXs pakcsURL
          [htxt cc, nbsp, textstyle "badge" (showVersionConstraints vcs)]
  | cc == "kics2"
  = ehrefWarnXs kics2URL
          [htxt cc, nbsp, textstyle "badge" (showVersionConstraints vcs)]
  | otherwise
  = textstyle "label label-default" (cc ++ " " ++ showVersionConstraints vcs)

showURL :: String -> HtmlExp
showURL s | "http" `isPrefixOf` s = ehref s [htxt s]
          | otherwise             = htxt s

-- Hypertext reference rendered as an extra small primary button.
hrefPrim :: String -> [HtmlExp] -> HtmlExp
hrefPrim ref hexps = href ref hexps `addClass` "btn btn-primary"

-- Hypertext reference rendered as a small primary block button.
hrefPrimSmBlock :: String -> [HtmlExp] -> HtmlExp
hrefPrimSmBlock ref hexps =
  href ref hexps `addClass` "btn btn-sm btn-primary btn-block"

-- Hypertext reference rendered as an extra small primary button.
hrefPrimXs :: String -> [HtmlExp] -> HtmlExp
hrefPrimXs ref hexps = href ref hexps `addClass` "btn btn-xs btn-primary"

-- Hypertext reference rendered as a small default button.
hrefDfltSm :: String -> [HtmlExp] -> HtmlExp
hrefDfltSm ref hexps = href ref hexps `addClass` "btn btn-sm btn-default"

-- Hypertext reference rendered as an extra small default button.
hrefDfltXs :: String -> [HtmlExp] -> HtmlExp
hrefDfltXs ref hexps = href ref hexps `addClass` "btn btn-xs btn-default"

-- External hypertext reference rendered as an extra small primary button.
ehrefPrimXs :: String -> [HtmlExp] -> HtmlExp
ehrefPrimXs ref hexps = ehref ref hexps `addClass` "btn btn-xs btn-primary"

-- External hypertext reference rendered as an extra small success button.
ehrefSuccXs :: String -> [HtmlExp] -> HtmlExp
ehrefSuccXs ref hexps = ehref ref hexps `addClass` "btn btn-xs btn-success"

-- External hypertext reference rendered as an extra small warning button.
ehrefWarnXs :: String -> [HtmlExp] -> HtmlExp
ehrefWarnXs ref hexps = ehref ref hexps `addClass` "btn btn-xs btn-warning"

-- Render as keyboard or user input.
kbd :: [HtmlExp] -> HtmlExp
kbd = HtmlStruct "kbd" []

--------------------------------------------------------------------------
-- Auxiliary operations to support generated HTML pages.

--- Standard HTML page for generated package descriptions.
cpmPackagePage :: String -> [HtmlExp] -> [[HtmlExp]] -> [HtmlExp] -> IO String
cpmPackagePage title sidenav apilinks maindoc = do
  let htmltitle = [h1 [htxt title]]
  time <- getLocalTime
  return $ showHtmlPage $
    bootstrapPage styleBaseURL cssIncludes title homeBrand
                  (leftTopMenu True) (apilinks ++ rightTopMenu) 4 sidenav
                  htmltitle maindoc (curryDocFooter time)


--- The URL of the Curry homepage
curryHomeURL :: String
curryHomeURL = "http://www.curry-lang.org"

--- The URL of CPM
cpmHomeURL :: String
cpmHomeURL = "http://www.curry-lang.org/tools/cpm"

--- The URL of the base directory containing the styles, images, etc.
styleBaseURL :: String
styleBaseURL = "../bt3"

cssIncludes :: [String]
cssIncludes = ["bootstrap.min","cpm"]

homeBrand :: (String,[HtmlExp])
homeBrand = (cpmHomeURL, [homeIcon, nbsp, htxt "Curry Package Manager"])

--- The standard left top menu.
leftTopMenu :: Bool -> [[HtmlExp]]
leftTopMenu inpkg =
  [ [href (mainurl "index.html")  [htxt "All packages"]]
  , [href (mainurl "indexv.html") [htxt "All package versions"]]
  , [href (mainurl "indexc.html") [htxt "All categories"]]
  ]
 where
  mainurl u = if inpkg then ".." </> u else u

--- The standard right top menu.
rightTopMenu :: [[HtmlExp]]
rightTopMenu =
  [ [ehref curryHomeURL [extLinkIcon, htxt " Curry Homepage"]]
  ]

--------------------------------------------------------------------------
-- Icons:

extLinkIcon :: HtmlExp
extLinkIcon = glyphicon "new-window"

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
    [HtmlStruct "thead" [] [toRow "th" headrow],
     HtmlStruct "tbody" [] (map (toRow "td") items)]
 where
  toRow ti row = HtmlStruct "tr" [] (map (\item -> HtmlStruct ti [] item) row)

--- An external reference
ehref :: String -> [HtmlExp] -> HtmlExp
ehref url desc = href url desc `addAttr` ("target","_blank")

------------------------------------------------------------------------------
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
