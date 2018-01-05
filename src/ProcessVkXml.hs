{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE Rank2Types                 #-}
module ProcessVkXml (processVkXmlFile, generateVkSource) where

import           Control.Monad                    (unless)
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader
import           Data.Conduit
import           Data.Conduit.Binary              (sourceFile)
import           Data.Semigroup
import           Data.XML.Types
import           Debug.Trace
import           Path
import           Path.IO
import           Text.XML                         as Xml
import           Text.XML.Stream.Parse            as Xml

import VkXml.Parser
import VkXml.Sections.Types
import VkXml.Sections.Enums

processVkXmlFile ::
       Path a File -- ^ path to vk.xml
    -> Path b Dir  -- ^ output directory for saving generated sources
    -> (forall m . (MonadReader ParseLoc m, MonadThrow m)
                 => Path b Dir -> Sink Event m r
       ) -- ^ Process parsed xml events somehow
    -> IO r
processVkXmlFile vkXmlFile outputDir pipe = do
    doesFileExist vkXmlFile >>= flip unless
      (error $ "vk.xml file located at " <> show vkXmlFile <> " is not found!")
    createDirIfMissing True outputDir

    putStrLn $ "Parsing file " <> show vkXmlFile <> ";"
    putStrLn $ "Output folder is " <> show outputDir <> ";"
    r <- runResourceT
       $  sourceFile (toFilePath vkXmlFile)
      =$= Xml.parseBytesPos Xml.def
       $$ parseWithLoc initLoc (pipe outputDir)
    putStrLn "Done!"
    return r
  where
    initLoc = defParseLoc vkXmlFile

generateVkSource ::
       VkXmlParser m
    => Path b Dir
    -> Sink Event m ()
generateVkSource outputDir = do
    mev <- await
    case mev of
      Nothing -> return ()
      Just ev -> do
        f ev
        generateVkSource outputDir

f :: VkXmlParser m => Event -> Sink Event m ()
f EventBeginDocument                     = return ()
f EventEndDocument                       = return ()
f (EventBeginDoctype _text _mexternalID) = return ()
f EventEndDoctype                        = return ()
f (EventInstruction _instruction)        = return ()
f ev@(EventBeginElement "types" _)       = do
    leftover ev
    typeMap <- parseTypes
    traceM $ "Parsed " <> show (length typeMap) <> " types."
f ev@(EventBeginElement "enums" _)       = do
    leftover ev
    menum <- parseEnums
    mapM_ traceShowM menum
f (EventBeginElement _name _attrs)       = return ()
f (EventEndElement _name)                = return ()
f (EventContent _content)                = return ()
f (EventComment _txt)                    = return ()
f (EventCDATA _txt)                      = return ()



-- newtype VkComment = VkComment { _unVkComment :: Text }
--   deriving (Eq, Ord, Show, Read, IsString)


-- data VkDoc
--   = VkDoc
--   { title :: Text
--   , comment :: VkComment
--   }
