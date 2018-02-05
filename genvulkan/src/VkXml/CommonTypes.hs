{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module VkXml.CommonTypes
  ( VkEnumValueName (..)
  , VkTypeName (..), VkMemberName (..), VkCommandName (..)
  , Sections (..), VkTagName (..), VkExtensionName (..)
  , parseSections
  , VkEnumName (..)
  , (<:>)
  ) where

import           Control.Monad.State.Class
import           Control.Monad.Trans.Class
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.String               (IsString)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.XML.Types
import           Text.XML.Stream.Parse

import           VkXml.Parser


newtype VkEnumValueName = VkEnumValueName { unVkEnumValueName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


newtype VkEnumName = VkEnumName { unVkEnumName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Type name
newtype VkTypeName = VkTypeName { unVkTypeName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | E.g. member of a struct
newtype VkMemberName = VkMemberName { unVkMemberName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Command name
newtype VkCommandName = VkCommandName { unVkCommandName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


newtype VkTagName = VkTagName { unVkTagName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)


newtype VkExtensionName = VkExtensionName { unVkExtensionName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)



-- | Parse a list of elements interspersed with comments,
--   that overwise would be a homogenous list of xml tag.
data Sections a
  = Sections
  { items    :: [a]
    -- ^ List of items in original order from xml file
  , comments :: [(Int, Text)]
    -- ^ Comments and their indices in between items.
    --   e.g. (0,"abc") means comment inserted before anything else.
    --   e.g. (length (items xs), "abc") means comment inserted after a.e. .
  } deriving Show

-- | Parse elements and comments in between them.
parseSections :: VkXmlParser m
              => Sink Event m (Maybe a) -- ^ how to parse elements
              -> Sink Event m (Sections a)
parseSections parseElem = evalStateC (0::Int) parseIt
  where
    prepComment c ~secs@(Sections _ cs) = secs{ comments = c:cs}
    prepItem    e ~secs@(Sections es _) = secs{ items    = e:es}
    parseIt = do
      mnewcomment <- tagIgnoreAttrs "comment" content
      me <- transPipe lift parseElem
      case (mnewcomment, me) of
        (Nothing, Nothing) -> pure $ Sections [] []
        (Just comment, Just e) -> do
          i <- get
          modify' (+1)
          prepComment (i,comment) . prepItem e <$> parseIt
        (Just comment, Nothing) -> do
          i <- get
          prepComment (i,comment) <$> parseIt
        (Nothing, Just e) -> do
          modify' (+1)
          prepItem e <$> parseIt

-- | Combine two comments vertically
(<:>) :: Text -> Text -> Text
a <:> b
  | T.null (T.strip a) = b
  | T.null (T.strip b) = a
  | otherwise          = T.unlines [a, "", b]
infixr 6 <:>
