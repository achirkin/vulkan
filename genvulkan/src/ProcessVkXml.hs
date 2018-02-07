{-# LANGUAGE Strict #-}
module ProcessVkXml
  ( processVkXmlFile
  , generateVkSource
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
import           Write

processVkXmlFile ::
       Path a File -- ^ path to vk.xml
    -> Path b Dir  -- ^ output directory for saving generated sources
    -> Path c File -- ^ path to cabal file to generate
    -> IO ()
processVkXmlFile vkXmlFile outputDir outCabalFile = do
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
    y <- generateVkSource outputDir outCabalFile x
    putStrLn "Done generating."
    return y
  where
    initLoc = defParseLoc vkXmlFile
