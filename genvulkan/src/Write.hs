{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write
  ( generateVkSource
  ) where

import           Control.Arrow                        (first)
import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import qualified Data.List                            as L
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
import           Path.IO
import           System.IO                            (writeFile)

import           VkXml.Sections
import           Write.ModuleWriter
import           Write.Types
import           Write.Types.Enum

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
          [
          ]
        }
      testS = T.unpack [text|
                module A
                  () where

                import Foreign.Ptr
                import Data.Void

                class HasVkPNext a where
                  vkPNext :: a -> Ptr Void
                  readVkPNext :: a -> IO (Ptr Void)
                  writeVkPNext :: a -> Ptr Void -> IO ()

                instance {-# OVERLAPPABLE #-} HasVkPNext a where
                  vkPNext _ = error "This type does not have field \"pNext\""
                  readVkPNext _ = error "This type does not have field \"pNext\""
                  writeVkPNext _ _ = error "This type does not have field \"pNext\""
            |]
  putStrLn testS
  putStrLn "-----------------"
  print $ (() <$) <$> parseModuleWithMode pMode testS



  createDirIfMissing True (outputDir </> [reldir|Graphics|])
  createDirIfMissing True (outputDir </> [reldir|Graphics|] </> [reldir|Vulkan|])

  do
    ((), mr) <- runModuleWriter vkXml "Graphics.Vulkan.SimpleTypes" $ do
        writePragma "Strict"
        genApiConstants
        genTypes1

    let rez = uncurry exactPrint
            . ppWithCommentsMode defaultMode
            $ genModule mr
    rez `deepseq` putStrLn "Done generating; now apply hfmt to reformat code..."
    frez <- hfmt rez
    case frez of
      Left err -> do
        putStrLn $ "Could not format the code:\n" <> err
        writeFile (toFilePath $ outputDir </> [relfile|Graphics/Vulkan/SimpleTypes.hs|])
          $ fixSourceHooks rez
      Right (ss, rez') -> do
        forM_ ss $ \(Suggestion s) ->
          putStrLn $ "Formatting suggestion:\n    " <> s
        writeFile (toFilePath $ outputDir </> [relfile|Graphics/Vulkan/SimpleTypes.hs|])
          $ fixSourceHooks rez'

  do
    ((), mr) <- runModuleWriter vkXml "Graphics.Vulkan.Structures" $ do
        writePragma "Strict"
        writeFullImport "Graphics.Vulkan.SimpleTypes"
        genTypes2

    let rez = uncurry exactPrint
            . ppWithCommentsMode defaultMode
            $ genModule mr
    rez `deepseq` putStrLn "Done generating; now apply hfmt to reformat code..."
    frez <- hfmt rez
    case frez of
      Left err -> do
        putStrLn $ "Could not format the code:\n" <> err
        writeFile (toFilePath $ outputDir </> [relfile|Graphics/Vulkan/Structures.hs|])
          $ fixSourceHooks rez
      Right (ss, rez') -> do
        forM_ ss $ \(Suggestion s) ->
          putStrLn $ "Formatting suggestion:\n    " <> s
        writeFile (toFilePath $ outputDir </> [relfile|Graphics/Vulkan/Structures.hs|])
          $ fixSourceHooks rez'




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
fixSourceHooks :: String -> String
fixSourceHooks = uncommentCPP . splitExportSections
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
