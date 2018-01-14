{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
-- | Generate handles
module Write.Types.Handle
  ( genHandle
  ) where

import           Data.Semigroup
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
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
    tname = toHaskellType vkTName
    tnametxt = qNameTxt tname
    tnameT = toHaskellType $ VkTypeName . (<>"T") $ unVkTypeName vkTName
    tnametxtT = qNameTxt tnameT
    rezComment = rezComment' >>= preComment . T.unpack
    rezComment' = if txt == mempty
                  then mtxt2
                  else case mtxt2 of
                    Nothing   -> Just txt
                    Just txt2 -> appendComLine (Just txt) txt2
    go t = do
      writePragma "EmptyDataDecls"
      writeDecl . setComment rezComment $ parseDecl'
        [text|type $tnametxt = $t $tnametxtT|]
      writeDecl
        . setComment
          (preComment $ T.unpack [text|Opaque data type referenced by $tnametxt|])
        $ parseDecl'
        [text|data $tnametxtT|]
      writeExport $ EAbs () (NoNamespace ()) tname
      writeExport $ EThingWith () (NoWildcard ()) tnameT []
genHandle t = error $ "genHandle: expected a simple handle type, but got: "
                  <> show t
