{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write
  ( generateVkSource
  ) where

import           Control.Monad
import           Data.Semigroup
-- import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SimpleComments
-- import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Format
import           NeatInterpolation
import           Path
import           System.IO                            (writeFile)

-- import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types

import           Write.ModuleWriter
import           Write.Enums

generateVkSource :: Path b Dir
                    -- ^ multiline
                    --     haddock!
                    --
                    --   >>> show 1
                    --
                 -> VkXml ()
                 -> IO ()
generateVkSource outputDir vkXml = do

  let pMode = defaultParseMode
        { baseLanguage = Haskell2010
        , extensions =
          [ EnableExtension GeneralizedNewtypeDeriving
          , EnableExtension PatternSynonyms
          , UnknownExtension "Strict"
          ]
        }
      testS = T.unpack [text|
                module A
                  ( VkImageLayout (..)
                  , pattern VK_IMAGE_LAYOUT_UNDEFINED
                  , pattern VK_IMAGE_LAYOUT_GENERAL
                  ) where
                newtype VkImageLayout = VkImageLayout Int32
                                      deriving (Eq, Ord, Storable)

                pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout
                pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

                pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout
                pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout (-1)

                type Hello = Int32
            |]
  putStrLn testS
  putStrLn "-----------------"
  print $ (() <$) <$> parseModuleWithMode pMode testS



  ((), mr) <- runModuleWriter vkXml
    $ flip writeSections
           (types . unInorder $ globTypes vkXml) $ \t ->
      case vkTypeCat t of
        VkTypeCatBitmask -> genEnum t
        VkTypeCatEnum -> genEnum t
        _ -> pure ()

  let rez = uncurry exactPrint
          . ppWithCommentsMode defaultMode
          $ genModule "Graphics.Vulkan" mr
  -- putStrLn rez
  frez <- hfmt rez
  case frez of
    Left err -> do
      putStrLn $ "Could not format the code:\n" <> err
      writeFile (toFilePath $ outputDir </> [relfile|Vulkan.hs|]) rez
    Right (ss, rez') -> do
      forM_ ss $ \(Suggestion s) ->
        putStrLn $ "Formatting suggestion:\n    " <> s
      writeFile (toFilePath $ outputDir </> [relfile|Vulkan.hs|]) rez'





hfmt :: String -> IO (Either String ([Suggestion], String))
hfmt source = do
    sets <- autoSettings
    mfts <- formatters sets
    pure $ go mfts (Right ([], source))
  where
    go [] esr       = esr
    go _ (Left err) = Left err
    go (f:fs) (Right (suggs, txt)) = case format f (HaskellSource "" txt) of
        Left err -> Left err
        Right (Reformatted (HaskellSource _ txt') suggs')
          -> go fs (Right ( suggs ++ suggs', txt'))
