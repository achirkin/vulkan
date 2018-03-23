{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
-- | Generate handles
module Write.Types.Handle
  ( genHandle
  ) where

import           Control.Monad.Reader.Class
import qualified Data.Map.Strict                      as Map
import           Data.Semigroup
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types

import           Write.ModuleWriter


genHandle :: Monad m => VkType -> ModuleWriter m ()
genHandle VkTypeSimple
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , typeData = VkTypeData
       { reference = [(htype, [])]
       , comment = mtxt2
       }
    }
  | htype == "VK_DEFINE_HANDLE" = go "Ptr"
  | htype == "VK_DEFINE_NON_DISPATCHABLE_HANDLE" = go "VkPtr"
  where
    tname = toQName vkTName
    tnametxt = qNameTxt tname
    tnameT = toQName $ VkTypeName . (<>"_T") $ unVkTypeName vkTName
    tnametxtT = qNameTxt tnameT
    rezComment = rezComment' >>= preComment . T.unpack
    rezComment' = if txt == mempty
                  then mtxt2
                  else case mtxt2 of
                    Nothing   -> Just txt
                    Just txt2 -> appendComLine (Just txt) txt2
    go t = do
      writePragma "EmptyDataDecls"
      writeImport $ DIThing t DITEmpty
      writeDecl . setComment rezComment $ parseDecl'
        [text|type $tnametxt = $t $tnametxtT|]
      writeDecl
        . setComment
          (preComment $ T.unpack [text|Opaque data type referenced by $tnametxt|])
        $ parseDecl'
        [text|data $tnametxtT|]
      writeExport $ DIThing tnametxt DITNo
      writeExport $ DIThing tnametxtT DITEmpty

genHandle VkTypeSimple
    { name = vkTName
    , attributes = VkTypeAttrs
        { alias = Just vkTAlias
        }
    } = ask >>= \vk -> case Map.lookup vkTAlias (globTypes vk) of
          Nothing -> return ()
          Just t  -> genHandle t{ name = vkTName }

genHandle t = error $ "genHandle: expected a simple handle type, but got: "
                  <> show t
