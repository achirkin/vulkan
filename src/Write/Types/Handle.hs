-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE QuasiQuotes           #-}
-- {-# LANGUAGE RecordWildCards       #-}
-- {-# LANGUAGE Strict                #-}
-- Generate enums and bitmasks
module Write.Types.Handle
  (
  ) where

-- import           Control.Monad
-- import           Control.Monad.Reader.Class
-- import           Data.Bits
-- import           Data.Word
-- import           Data.Semigroup
-- import           Data.Text                            (Text)
-- import qualified Data.Text                            as T
-- import qualified Data.Map.Strict           as Map
-- import qualified Data.Text.Read as T
-- import           Language.Haskell.Exts.SimpleComments
-- import           Language.Haskell.Exts.Syntax
-- import           NeatInterpolation
-- import           Numeric
--
-- import           VkXml.CommonTypes
-- import           VkXml.Sections
-- import           VkXml.Sections.Types
-- import           VkXml.Sections.Enums
--
-- import           Write.ModuleWriter
