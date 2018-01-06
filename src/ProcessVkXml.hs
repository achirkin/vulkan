{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE Strict                #-}
module ProcessVkXml (processVkXmlFile, generateVkSource) where

import           Control.Monad                (unless)
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary          (sourceFile)
import           Data.Semigroup
import           Data.XML.Types
import           Debug.Trace
import           Path
import           Path.IO
import           Text.XML                     as Xml
import           Text.XML.Stream.Parse        as Xml

import           VkXml.CommonTypes
import           VkXml.Parser
import           VkXml.Sections.Commands
import           VkXml.Sections.Enums
import           VkXml.Sections.Types

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
    mtypes <- parseTypes
    traceM $ "Parsed "
          <> show (maybe 0 (length . items . types) mtypes) <> " types."
f ev@(EventBeginElement "enums" _)       = do
    leftover ev
    menums <- parseEnums
    mapM_ (traceM . reportEnums) menums
f ev@(EventBeginElement "commands" _)       = do
    leftover ev
    mcoms <- parseCommands
    -- mapM_ traceShowM (maybe [] commands mcoms)
    traceM $ "Parsed "
          <> show (maybe 0 (length . commands) mcoms) <> " commands."
f (EventBeginElement _name _attrs)       = return ()
f (EventEndElement _name)                = return ()
f (EventContent _content)                = return ()
f (EventComment _txt)                    = return ()
f (EventCDATA _txt)                      = return ()


reportEnums :: VkEnums -> String
reportEnums VkEnums {memberEnums = xs, name = VkTypeName n }
  = "Parsed " <> show (length . items $ xs) <> " enums " <> show n <> "."
reportEnums VkBitmasks {memberMasks = xs, name = VkTypeName n }
  = "Parsed " <> show (length . items $ xs) <> " bitmasks " <> show n <> "."
reportEnums VkConstants {memberConsts = xs, name = VkTypeName n }
  = "Parsed " <> show (length . items $ xs) <> " constants " <> show n <> "."
