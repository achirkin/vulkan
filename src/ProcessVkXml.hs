{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module ProcessVkXml (processVkXmlFile, generateVkSource) where

import           Control.Monad                    (unless)
import           Control.Monad.Catch
import           Control.Monad.Except
-- import           Control.Monad.IO.Class
-- import           Control.Monad.Trans.Class
-- import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Control.Monad.Reader
import           Control.Arrow
import           Data.Conduit
import           Data.Conduit.Binary              (sourceFile)
import           Data.Conduit.Lift
import           Data.List                        (intercalate)
import           Data.Map                         (Map)
import qualified Data.Map.Strict                  as Map
-- import           Data.Maybe
import           Data.Semigroup
import           Data.String                      (IsString)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.XML.Types
import           Debug.Trace
import           Path
import           Path.IO
import qualified Data.XML.Types                   as Xml
import           Text.XML                         as Xml
import           Text.XML.Stream.Parse            as Xml
import           GHC.Stack

import VkXml.Parser
import VkXml.Sections.Types

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
f EventBeginDocument                     = return () -- liftIO $ putStrLn "Document started"
f EventEndDocument                       = return () -- liftIO $ putStrLn "Document ended"
f (EventBeginDoctype _text _mexternalID) = return () -- liftIO $ print text >> print mexternalID
f EventEndDoctype                        = return () -- liftIO $ putStrLn "Doctype ended"
f (EventInstruction _instruction)        = return () -- liftIO $ putStrLn "instruction" >> print instruction
f ev@(EventBeginElement "types" _)       = do
   leftover ev
   typeMap <- parseTypes
   traceShowM typeMap
f (EventBeginElement _name _attrs)       = return ()
f (EventEndElement _name)                = return () -- liftIO $ putStrLn "Element Ended" >> print name
f (EventContent _content)                = return () -- liftIO $ putStrLn "Content" >> print content
f (EventComment _txt)                    = return () -- liftIO $ putStrLn "Comment" >> print txt
f (EventCDATA _txt)                      = return () -- liftIO $ putStrLn "CDATA" >> print txt



newtype VkComment = VkComment { _unVkComment :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


data VkDoc
  = VkDoc
  { title :: Text
  , comment :: VkComment
  }
