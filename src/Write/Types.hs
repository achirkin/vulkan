{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
module Write.Types
  ( genTypes
  ) where

import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Reader.Class
import qualified Control.Monad.Trans.RWS.Strict       as RWS
import           Control.Monad.Trans.State.Strict     (StateT)
import qualified Control.Monad.Trans.State.Strict     as State
import           Data.Semigroup
-- import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Types

import           Write.ModuleWriter
import           Write.Types.Enums


genTypes :: Monad m => ModuleWriter m ()
genTypes = hoist (`State.evalStateT` Nothing) genTypes'


genTypes' :: Monad m => ModuleWriter (StateT (Maybe VkTypeCategory) m) ()
genTypes' = do
    vkXml <- ask
    curlvl <- ModuleWriter $ RWS.gets currentSecLvl
    ModuleWriter . RWS.modify $ \mr -> mr { currentSecLvl = curlvl + 1}
    flip writeSections
         (types . unInorder $ globTypes vkXml) $ \t -> do
      oldcat <- lift State.get
      let curcat = vkTypeCat t
      when (oldcat /= Just curcat) $ do
        lift . State.put $ Just curcat
        writeSectionPre curlvl $ case curcat of
          VkTypeNoCat          -> "External types"
          VkTypeCatInclude     -> "Include pragmas"
          VkTypeCatDefine      -> "Define pragmas"
          VkTypeCatBasetype    -> "Base types"
          VkTypeCatBitmask     -> "Bitmasks"
          VkTypeCatHandle      -> "Handles"
          VkTypeCatEnum        -> "Enums"
          VkTypeCatFuncpointer -> "Function pointers"
          VkTypeCatStruct      -> "C structures"
          VkTypeCatUnion       -> "C unions"
      case vkTypeCat t of
        VkTypeNoCat       -> genNocatData t
        VkTypeCatBasetype -> genBasetypeAlias t
        VkTypeCatBitmask  -> genEnum t
        VkTypeCatEnum     -> genEnum t
        _                 -> pure ()
    ModuleWriter . RWS.modify $ \mr -> mr { currentSecLvl = curlvl + 1}





-- | At this moment, just define the data type with no constructors.
--   Later we will think what to do with it.
genNocatData :: Monad m => VkType -> ModuleWriter m ()
genNocatData VkTypeSimple
  { name = vkTName
  , attributes = VkTypeAttrs
      { requires = mreq
      }
  } = case tname of
      UnQual{} -> do
        writePragma "EmptyDataDecls"
        writeDecl . setComment rezComment $ parseDecl'
          [text|data $tnametxt|]
        writeExport $ EAbs () (NoNamespace ()) tname
      Qual{}   -> do
        requireType tname
        writeExport $ EAbs () (NoNamespace ()) tname
      _        -> pure ()
  where
    tname = toHaskellType vkTName
    tnametxt = qNameTxt tname
    rezComment = ((\s -> "Requires @" <> s <> "@") . unVkTypeName <$> mreq)
              >>= preComment . T.unpack
genNocatData t = error
  $ "genNocatData: data with no description, but got: "
  <> show t



-- | VkTypeCatBasetype
genBasetypeAlias :: Monad m => VkType -> ModuleWriter m ()
genBasetypeAlias t@VkTypeSimple
    { typeData = VkTypeData
       { reference = [(vkTRef, [])]
       }
    } = do
  requireType $ toHaskellType vkTRef
  genAlias t
genBasetypeAlias t
  = error $ "genBasetypeAlias: expected a simple basetype, but got: "
         <> show t
