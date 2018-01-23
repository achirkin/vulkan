{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
-- | WIP
module Write.Types.Union
  ( genUnion
  ) where


import           Data.Semigroup
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation


import           VkXml.Sections.Types

import           Write.ModuleWriter


genUnion :: Monad m => VkType -> ModuleWriter m ()
genUnion VkTypeComposite
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    }
    = do
    writePragma "EmptyDataDecls"
    let ds = parseDecls [text|
          data $tnametxt
          |]

    mapM_ writeDecl
      . insertDeclComment (T.unpack tnametxt) rezComment
      $ ds

    writeExport $ EThingWith () (EWildcard () 0) tname []
  where
    tname = toHaskellType vkTName
    tnametxt = qNameTxt tname
    rezComment = rezComment'' >>= preComment . T.unpack
    rezComment'' = appendComLine rezComment'
                 $ T.unlines . map ("> " <>) $ T.lines "" -- c
    rezComment' = if txt == mempty
                  then Nothing
                  else Just txt
genUnion t
  = error $ "genStruct: expected a type with members, "
          <> "but got: "
          <> show t
