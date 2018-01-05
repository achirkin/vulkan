{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VkXml.CommonTypes
  ( VkName (..)
  , VkEnumValueName (..)
  , VkTypeName (..), VkMemberName (..)
  ) where

import           Data.String                (IsString)
import           Data.Text                  (Text)

-- newtype VkEnumName = VkEnumName { unVkEnumName :: Text }
--   deriving (Eq, Ord, Show, Read, IsString)

newtype VkEnumValueName = VkEnumValueName { unVkEnumValueName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Type name
newtype VkTypeName = VkTypeName { unVkTypeName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | E.g. member of a struct
newtype VkMemberName = VkMemberName { unVkMemberName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)

-- | Some identifier
newtype VkName = VkName { unVkName :: Text }
  deriving (Eq, Ord, Show, Read, IsString)
