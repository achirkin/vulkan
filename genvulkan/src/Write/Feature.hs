{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Feature
  ( genFeature, genRequire, showExts
  ) where

import           Control.Monad
import           Control.Monad.Morph              (hoist)
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class        (MonadState (..))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.Map                         as Map
import           Data.Maybe                       (maybe)
import           Data.Semigroup
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Language.Haskell.Exts.Syntax

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Commands          as Cs
import           VkXml.Sections.Types             as Ts
-- import           VkXml.Sections.Enums as Es
import           VkXml.Sections.Feature

import           Write.Commands
import           Write.ModuleWriter
import           Write.Types
import           Write.Types.Enum
-- import           Write.Util.DeclaredNames


genFeature :: Monad m => VkFeature -> ModuleWriter m ()
genFeature VkFeature {..} = hoist (`evalStateT` mempty) $ do
    curlvl <- getCurrentSecLvl
    vkXml <- ask
    let tps = globTypes vkXml
        cmds = globCommands vkXml
        -- ens = Map.fromList
        --     . map (\e -> (_vkEnumName e, e))
        --     $ Map.elems (globEnums vkXml) >>= items . _vkEnumsMembers . unInorder
            -- .. vkEnumMembers . traverse items
    writeSection curlvl $ T.unlines
      [ comment
      , ""
      , "@api = " <> api <> "@"
      , ""
      , "@name = " <> name <> "@"
      , ""
      , "@number = " <> number <> "@"
      ]
    pushSecLvl $ \lvl -> mapM_ (genRequire lvl tps cmds) reqList


genRequire :: MonadState (Set (ModuleName ())) m
          => Int
          -> Map.Map VkTypeName VkType
          -> Map.Map VkCommandName VkCommand
          -- -> Map.Map VkEnumName VkEnum
          -> VkRequire
          -> ModuleWriter m ()
genRequire curlvl tps cmds VkRequire {..} = do
  writeOptionsPragma (Just HADDOCK) "not-home"
  writeSection curlvl $
    comment <:> showExts requireExts
  forM_ (Set.toList $ foldMap (findAllRequiredTypes tps) requireTypes)
       $ \tname -> case Map.lookup tname tps of
        Nothing -> pure ()
        Just t  -> do

          otn <- lift get
          mIMod <- lookupDiModule $ DIThing (unVkTypeName tname) DITAll
          case mIMod of
            Nothing -> genType t
            Just imod ->
              if Set.member imod otn
              then pure ()
              else do
                lift $ put (Set.insert imod otn)
                case imod of
                  ModuleName () m -> writeFullImport m
                writeExportSpec $ EModuleContents () imod
                pure ()
  tNames <- fmap mconcat $
    forM requireComms $ \cname -> case Map.lookup cname cmds of
        Nothing -> pure mempty
        Just t  -> genCommand t

  emodNames <- foldM (\s (VkTypeName n) ->
                         fmap (Set.union s . maybe mempty Set.singleton)
                         . lookupDiModule $ DIThing n DITNo
                     ) mempty $ foldMap (findAllRequiredTypes tps) tNames

  otNames <- lift get
  lift $ put (emodNames `Set.union` otNames)
  forM_ (emodNames Set.\\ otNames) $ \mm@(ModuleName _ m) -> do
    writeFullImport m
    writeExportSpec $ EModuleContents () mm


  forM_ requireEnums $ enumPattern >=> mapM_ (writeExport . DIPat)


findAllRequiredTypes :: Map.Map VkTypeName VkType -> VkTypeName -> Set VkTypeName
findAllRequiredTypes tps = go . Set.singleton
  where
    go tns0 =
      let tns1 = Set.foldl (\s -> Set.union s
                                . maybe mempty (Set.fromList . requiresTypes)
                                . flip Map.lookup tps
                            ) mempty tns0
          tnsDiff = tns1 `Set.difference` tns0
          tnsUnion = tns1 `Set.union` tns0
      in if Set.null tnsDiff
         then tnsUnion
         else go tnsUnion


showExts :: [VkExtensionName] -> T.Text
showExts [] = ""
showExts as = "Required extensions: " <> showExts' as
  where
    showExts' []     = "."
    showExts' [x]    = "'" <> unVkExtensionName x <> "'."
    showExts' (x:xs) = "'" <> unVkExtensionName x <> "', " <> showExts' xs
