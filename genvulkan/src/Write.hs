{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write
  ( generateVkSource
  ) where

import           Control.Arrow                        (first, second)
import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe)
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Format
import           NeatInterpolation
import           Path
import           Path.IO
import           System.IO                            (writeFile)
import           Text.RE.TDFA.String

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Extensions
import           Write.Commands
import           Write.Extension
import           Write.Feature
import           Write.ModuleWriter
import           Write.Types
import           Write.Types.Enum


generateVkSource :: Path b Dir
                 -> Path c File
                 -> VkXml ()
                 -> IO ()
generateVkSource outputDir outCabalFile vkXml = do

  createDirIfMissing True (outputDir </> [reldir|Graphics|])
  createDirIfMissing True (outputDir </> [reldir|Graphics|] </> [reldir|Vulkan|])
  createDirIfMissing True
    (outputDir </> [reldir|Graphics|] </> [reldir|Vulkan|] </> [reldir|Ext|])


  exportedNamesCommon <- do
    (enames, mr) <- runModuleWriter vkXml "Graphics.Vulkan.Common" mempty $ do
       writePragma "Strict"
       writePragma "DataKinds"
       genApiConstants
       genBaseTypes
       getNamesInScope
    writeModule outputDir [relfile|Graphics/Vulkan/Common.hsc|] id mr
    pure enames

  exportedNamesBase <- do
    (enames, mr) <- runModuleWriter vkXml "Graphics.Vulkan.Base" exportedNamesCommon $ do
       writePragma "Strict"
       writePragma "DataKinds"
       writeFullImport "Graphics.Vulkan.Marshal.Internal"
       writeFullImport "Graphics.Vulkan.Marshal"
       writeFullImport "Graphics.Vulkan.Common"
       genBaseStructs
       genBaseCommands
       getNamesInScope
    writeModule outputDir [relfile|Graphics/Vulkan/Base.hsc|]
         ( addOptionsPragma GHC "-fno-warn-missing-methods"
         . addOptionsPragma GHC "-fno-warn-unticked-promoted-constructors"
         ) mr
    pure enames

  exportedNamesCore <- do
    (enames, mr) <- runModuleWriter vkXml "Graphics.Vulkan.Core" exportedNamesBase $ do
       writePragma "Strict"
       writePragma "DataKinds"
       writeFullImport "Graphics.Vulkan.Marshal.Internal"
       writeFullImport "Graphics.Vulkan.Marshal"
       writeFullImport "Graphics.Vulkan.Common"
       writeFullImport "Graphics.Vulkan.Base"
       genFeature
       getNamesInScope
    writeModule outputDir [relfile|Graphics/Vulkan/Core.hsc|]
         ( addOptionsPragma GHC "-fno-warn-missing-methods"
         . addOptionsPragma GHC "-fno-warn-unticked-promoted-constructors"
         ) mr
    pure enames

  eModules <- forM (extensions . unInorder . globExtensions $ vkXml) $ \ext -> do
    let eName = T.unpack . unVkExtensionName . extName $ attributes ext
        modName = "Graphics.Vulkan.Ext." <> eName
    fname <- parseRelFile (eName ++ ".hsc")
    (exProtect, mr) <- runModuleWriter vkXml modName exportedNamesCore $ do
       writePragma "Strict"
       writePragma "DataKinds"
       writeFullImport "Graphics.Vulkan.Marshal.Internal"
       writeFullImport "Graphics.Vulkan.Marshal"
       writeFullImport "Graphics.Vulkan.Common"
       writeFullImport "Graphics.Vulkan.Base"
       writeFullImport "Graphics.Vulkan.Core"
       genExtension ext
    writeModule outputDir ([reldir|Graphics/Vulkan/Ext|] </> fname)
         ( addOptionsPragma GHC "-fno-warn-missing-methods"
         . addOptionsPragma GHC "-fno-warn-unticked-promoted-constructors"
         ) mr
    pure (T.pack modName, exProtect)

  writeFile (toFilePath $ outputDir </> [relfile|Graphics/Vulkan/Ext.hs|])
      $ T.unpack $ T.unlines $
        [ "{-# LANGUAGE CPP #-}"
        , "module Graphics.Vulkan.Ext"
        , "    ("
        ]
     <> zipWith exportLine (' ' : repeat ',') eModules
     <> [ "    ) where"
        , ""
        ]
     <> map importLine eModules

  writeFile (toFilePath outCabalFile) . T.unpack $ genCabalFile eModules


importLine :: (Text, Maybe Text) -> Text
importLine (mname, Nothing) = "import " <> mname
importLine (mname, Just p)  = T.unlines
  [ "#ifdef " <> p
  , "import " <> mname
  , "#endif"
  ]


exportLine :: Char -> (Text, Maybe Text) -> Text
exportLine c (mname, Nothing) = "    " <> T.cons c (" module " <> mname)
exportLine c (mname, Just p)  = T.unlines
  [ "#ifdef " <> p
  , "    " <> T.cons c (" module " <> mname)
  , "#endif"
  ]


genCabalFile :: [(Text, Maybe Text)] -> Text
genCabalFile eModules = T.unlines $
      ( [text|
          name:                vulkan-api
          version:             0.1.0.0
          synopsis:            Low-level low-overhead vulkan api bindings
          description:         Haskell bindings for vulkan api as described in vk.xml.
          homepage:            https://github.com/achirkin/genvulkan#readme
          license:             BSD3
          license-file:        LICENSE
          author:              Artem Chirkin
          maintainer:          chirkin@arch.ethz.ch
          copyright:           Copyright: (c) 2018 Artem Chirkin
          category:            vulkan, bsd3, graphics, library, opengl
          build-type:          Simple
          cabal-version:       >=1.10

        |]
      : map mkFlagDef protectedGroups
      )
   <> ( [text|
          library
              hs-source-dirs:      src, src-gen
              exposed-modules:
                  Graphics.Vulkan
                  Graphics.Vulkan.Marshal
                  Graphics.Vulkan.Common
                  Graphics.Vulkan.Base
                  Graphics.Vulkan.Core
                  Graphics.Vulkan.Ext
        |]
      : map (spaces <>) unprotected
      )
   <> map mkModules protectedGroups
   <> ( tail $ T.lines
        [text|
          DUMMY (have to keep it here for NeatInterpolation to work properly)
              other-modules:
                  Graphics.Vulkan.Marshal.Internal
              build-depends:
                  base >= 4.7 && < 5
                , ghc-prim >= 0.4 && < 0.6
              default-language:    Haskell2010
              ghc-options:         -Wall
              extra-libraries:     vulkan
              include-dirs:        include

          source-repository head
              type:     git
              location: https://github.com/achirkin/genvulkan
        |]
      )
  where
    spaces = "        "
    mkGroup []           = []
    mkGroup xs@((_,g):_) = [(g, map fst xs)]
    (unprotected, protectedGroups)
       = splitThem
       . (>>= mkGroup)
       . L.groupBy (\(_, a) (_, b) -> a == b)
       $ L.sortOn snd eModules
    splitThem []                 = ([], [])
    splitThem ((Nothing, xs):ms) = first (xs ++)     $ splitThem ms
    splitThem ((Just g , xs):ms) = second ((g, xs):) $ splitThem ms

    mkFlagName
      = firstDown
      . T.pack
      . toCamelCase
      . T.unpack
      . T.toLower
      . removeVk
    removeVk g = fromMaybe g $ T.stripPrefix "VK_" g

    mkFlagDef (g, _)
      | f <- mkFlagName g
      = [text|
          flag $f
              description:
                Enable platform-specific extensions protected by CPP macros $g
              default: False
        |]

    mkModules (g,ms)
      | f <- mkFlagName g
      = T.unlines
      $ ("    if flag(" <> f <> ")")
      : ("      cpp-options: -D" <> g)
      :  "      exposed-modules:"
      : map (spaces <>) ms



addOptionsPragma :: Tool -> String -> Module (Maybe a) -> Module (Maybe a)
addOptionsPragma tool str (Module a h ps is ds)
  = Module a h (OptionsPragma Nothing (Just tool) str : ps) is ds
addOptionsPragma tool str (XmlPage a b ps c d e f)
  = XmlPage a b (OptionsPragma Nothing (Just tool) str : ps) c d e f
addOptionsPragma tool str (XmlHybrid a b ps c d e f g h)
  = XmlHybrid a b (OptionsPragma Nothing (Just tool) str : ps) c d e f g h

writeModule :: Path b Dir
            -> Path Rel File
            -> (Module A -> Module A)
            -> ModuleWriting
            -> IO ()
writeModule outputDir p postF mw = do
    rez `deepseq` putStrLn "Done generating; now apply hfmt to reformat code..."
    frez <- hfmt rez
    case frez of
      Left err -> do
        putStrLn $ "Could not format the code:\n" <> err
        writeFile pp
          $ fixSourceHooks isHsc rez
      Right (ss, rez') -> do
        forM_ ss $ \(Suggestion s) ->
          putStrLn $ "Formatting suggestion:\n    " <> s
        writeFile pp
          $ fixSourceHooks isHsc rez'
    putStrLn $ "Done: " <> pp
  where
    isHsc = ".hsc" `L.isSuffixOf` pp
    pp = toFilePath $ outputDir </> p
    rez = uncurry exactPrint
        . ppWithCommentsMode defaultMode
        . postF $ genModule mw

-- unfortunately, I have to disable hindent for now,
--  because it breaks haddock:
--   moves the first section declaration before the opening parenthesis
--   in the export list.
hfmt :: String -> IO (Either String ([Suggestion], String))
hfmt source = do
    sets <- autoSettings
    mfts <- sequence [hlint sets,  stylish sets]
     -- formatters sets   -- hindent sets,
    pure . fmap (first removeCamelCaseSuggestions)
         $ go mfts (Right ([], source))
  where
    go [] esr       = esr
    go _ (Left err) = Left err
    go (f:fs) (Right (suggs, txt)) = case format f (HaskellSource "" txt) of
        Left err -> Left err
        Right (Reformatted (HaskellSource _ txt') suggs')
          -> go fs (Right ( suggs ++ suggs', txt'))
    removeCamelCaseSuggestions [] = []
    removeCamelCaseSuggestions (Suggestion s:xs)
      | "Use camelCase" `L.isInfixOf` s = removeCamelCaseSuggestions xs
      | otherwise = Suggestion s : removeCamelCaseSuggestions xs

-- | Various small fixes to normalize haddock output, enable CPP, etc.
fixSourceHooks :: Bool -> String -> String
fixSourceHooks isHsc = (if isHsc then enableHSC else id)
                     . uncommentCPP . splitExportSections
  where
    stripBeginSpace = dropWhile isSpace

    -- improve haddock layout
    splitExportSections = unlines . go . lines
      where
        go [] = []
        go (x:y:zs) | "-- *" `L.isPrefixOf` stripBeginSpace y
                    && "--" `L.isPrefixOf` stripBeginSpace x
                    = x:"":y:go zs
        go (x:xs) = x : go xs

    -- enable CPP: I put all CPP code into comments starting with @-- ##@
    uncommentCPP = unlines . go . lines
      where
        go [] = []
        go (x:xs) | "-- ##" `L.isPrefixOf` stripBeginSpace x
                  = drop 5 (stripBeginSpace x) : go xs
                  | otherwise = x : go xs

    -- enable hsc expressions
    enableHSC
        = unlines
        . ("#include \"vulkan/vulkan.h\"":) . ("":)
        . byThree (*=~/ [ed|HSC2HS___[[:space:]]+"##([^"]+)"///#$1|])
        . map ( (*=~/ [edBS|{-##///{-#|])
              . (*=~/ [edBS|##-}///#-}|])
              . (*=~/ [edBS|#///##|])
              )
        . lines
      where
        byThree _ [] = []
        byThree f [x] = [f x]
        byThree f [x,y] = lines . f $ unlines [x,y]
        byThree f (x:y:z:ss) = case lines . f $ unlines [x,y,z] of
          []   -> byThree f ss
          a:as ->  a : byThree f (as ++ ss)
