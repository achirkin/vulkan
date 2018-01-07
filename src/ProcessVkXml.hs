{-# LANGUAGE Strict #-}
module ProcessVkXml
  ( processVkXmlFile
  , generateVkSource
  , VkXml (..), InOrder (..)
  ) where

import           Control.Monad                (unless)
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary          (sourceFile)
import           Data.Semigroup
import           Path
import           Path.IO
import           Text.XML                     as Xml
import           Text.XML.Stream.Parse        as Xml

import           VkXml.Parser
import           VkXml.Sections

processVkXmlFile ::
       Path a File -- ^ path to vk.xml
    -> Path b Dir  -- ^ output directory for saving generated sources
    -> IO ()
processVkXmlFile vkXmlFile outputDir = do
    doesFileExist vkXmlFile >>= flip unless
      (error $ "vk.xml file located at " <> show vkXmlFile <> " is not found!")
    createDirIfMissing True outputDir

    putStrLn $ "Parsing file " <> show vkXmlFile <> ";"
    putStrLn $ "Output folder is " <> show outputDir <> ";"
    x <- runResourceT
       $  sourceFile (toFilePath vkXmlFile)
      =$= Xml.parseBytesPos Xml.def
       $$ parseWithLoc initLoc parseVkXml
    putStrLn "Done parsing, start generating..."
    y <- generateVkSource outputDir x
    putStrLn "Done generating."
    return y
  where
    initLoc = defParseLoc vkXmlFile

generateVkSource :: Path b Dir -> VkXml -> IO ()
generateVkSource _outputDir _vkXml = return ()
