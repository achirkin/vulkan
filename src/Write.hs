{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write
  ( generateVkSource
  ) where

import           Control.Monad
import           Data.Semigroup
import qualified Data.Text                            as T
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Format
import           NeatInterpolation
import           Path
import           System.IO                            (writeFile)

import           VkXml.Sections
import           Write.ModuleWriter
import           Write.Types
import           Write.Types.Enums

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

                import B(pattern HeyPat)

                pattern VK_LOD_CLAMP_NONE :: (Fractional a, Eq a) => a
                pattern VK_LOD_CLAMP_NONE = 1000.0
            |]
  putStrLn testS
  putStrLn "-----------------"
  print $ (() <$) <$> parseModuleWithMode pMode testS



  ((), mr) <- runModuleWriter vkXml "Graphics.Vulkan" $ do
      genApiConstants
      genTypes

  let rez = uncurry exactPrint
          . ppWithCommentsMode defaultMode
          $ genModule mr
  rez `seq` putStrLn "Done generating; now apply hfmt to reformat code..."
  frez <- hfmt rez
  case frez of
    Left err -> do
      putStrLn $ "Could not format the code:\n" <> err
      writeFile (toFilePath $ outputDir </> [relfile|Vulkan.hs|]) rez
    Right (ss, rez') -> do
      forM_ ss $ \(Suggestion s) ->
        putStrLn $ "Formatting suggestion:\n    " <> s
      writeFile (toFilePath $ outputDir </> [relfile|Vulkan.hs|]) rez'




-- unfortunately, I have to disable hindent for now,
--  because it breaks haddock:
--   moves the first section declaration before the opening parenthesis
--   in the export list.
hfmt :: String -> IO (Either String ([Suggestion], String))
hfmt source = do
    sets <- autoSettings
    mfts <- sequence [hlint sets,  stylish sets]
     -- formatters sets   -- hindent sets,
    pure $ go mfts (Right ([], source))
  where
    go [] esr       = esr
    go _ (Left err) = Left err
    go (f:fs) (Right (suggs, txt)) = case format f (HaskellSource "" txt) of
        Left err -> Left err
        Right (Reformatted (HaskellSource _ txt') suggs')
          -> go fs (Right ( suggs ++ suggs', txt'))
