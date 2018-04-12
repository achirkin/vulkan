{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write.Cabal
  ( genCabalFile
  ) where

import           Control.Arrow                        (first, second)
import qualified Data.List                            as L
import Paths_genvulkan (version)
import Data.Version (showVersion)
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           NeatInterpolation

import           VkXml.CommonTypes
import           Write.Feature

hardcodedModules :: [Text]
hardcodedModules =
  [ "Graphics.Vulkan"
  , "Graphics.Vulkan.Marshal"
  , "Graphics.Vulkan.Marshal.Proc"
  , "Graphics.Vulkan.Marshal.Create"
  , "Graphics.Vulkan.Marshal.Internal"
  , "Graphics.Vulkan.Constants"
  , "Graphics.Vulkan.Ext"
  ]

genCabalFile :: [ProtectDef]
                -- ^ Flags to enable native FFI for vulkan versions
             -> [(Text, Maybe ProtectDef)]
                -- ^ module names and if they are protected by compilation flags
             -> Text
genCabalFile coreVersions eModules = T.unlines $
      ( [text|
          --
          -- Do not modify this file directly
          -- it is autogenerated at genvulkan/src/Write/Cabal.hs
          --
          name:                vulkan-api
          version:             $library_version
          synopsis:            Low-level low-overhead vulkan api bindings
          description:
              Haskell bindings for vulkan api as described in vk.xml.
              .
              You can find some simple examples at <https://github.com/achirkin/vulkan/tree/master/vulkan-examples vulkan-examples> page
              or a more complete triangle rendering program at <https://github.com/achirkin/vulkan/tree/master/vulkan-triangles vulkan-triangles> page.
              .
              For further information, please refer to <https://github.com/achirkin/vulkan#readme README.md>.
          homepage:            https://github.com/achirkin/vulkan#readme
          license:             BSD3
          license-file:        LICENSE
          author:              Artem Chirkin
          maintainer:          chirkin@arch.ethz.ch
          copyright:           Copyright: (c) 2018 Artem Chirkin
          category:            vulkan, bsd3, graphics, library, opengl
          build-type:          Custom
          cabal-version:       >=1.24
          extra-source-files:
              include/vulkan/*.h

        |]
      : map mkFlagDef protectedGroups ++ map mkVersionFlagDef coreVersions
      )
   <> ( [text|
          library
              hs-source-dirs:      src, src-gen
              exposed-modules:
        |]
      : map (spaces <>) exposedBase
      ++ "    other-modules:"
      : map (spaces <>) otherBase
      )
   <> map (mkModules . second L.sort) protectedGroups
   <> map mkVersionPragma coreVersions
   <> tail ( T.lines
        [text|
          DUMMY (have to keep it here for NeatInterpolation to work properly)
              build-depends:
                  base >= 4.7 && < 5
              default-language:    Haskell2010
              ghc-options:         -Wall
              if os(windows)
                extra-libraries:   vulkan-1
              else
                extra-libraries:   vulkan
              include-dirs:        include

          source-repository head
              type:     git
              location: https://github.com/achirkin/vulkan
              subdir:   vulkan-api
        |]
      )
  where
    library_version = T.pack $ showVersion version
    spaces = "        "

    (otherBase, exposedBase) = L.partition isOtherModule
      . L.sort $ unprotected ++ hardcodedModules
    isOtherModule = T.isPrefixOf "Graphics.Vulkan.Types"

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


    mkFlagDef (p, _)
      | f <- unProtectFlag $ protectFlag p
      , g <- unProtectCPP $ protectCPP p
      = [text|
          flag $f
              description:
                Enable platform-specific extensions protected by CPP macros $g
              default: False
        |]

    mkModules (p,ms)
      | f <- unProtectFlag $ protectFlag p
      , g <- unProtectCPP $ protectCPP p
      , (otherMs, exposedMs) <- L.partition isOtherModule $ L.sort ms
      = T.unlines
      $ ("    if flag(" <> f <> ")")
      : ("      cpp-options: -D" <> g)
      :  "      exposed-modules:"
      : map (spaces <>) exposedMs
      ++ "      other-modules:"
      : map (spaces <>) otherMs

    mkVersionFlagDef p
      | f <- unProtectFlag $ protectFlag p
      , v <- coreVersion p
      = [text|
          flag $f
              description:
                Enable foreign-imported functions from Vulkan $v feature set
              default: False
        |]

    mkVersionPragma p
      | f <- unProtectFlag $ protectFlag p
      , g <- unProtectCPP $ protectCPP p
      = T.unlines
      [ "    if flag(" <> f <> ")"
      , "      cpp-options: -D" <> g
      ]
