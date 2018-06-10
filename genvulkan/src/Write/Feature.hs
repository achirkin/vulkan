{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Feature
  ( genFeature, genRequire, showExts, coreVersion
  ) where

import           Control.Monad
import           Control.Monad.Morph              (hoist)
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class        (MonadState (..))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.Char                        as C
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, maybe)
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

-- | The protect definition this function returns does not turn off the feature
--   completely, but just replaces native FFI with @vkGet..ProcAddr@-located function.
genFeature :: Monad m => ProtectDef -> VkFeature -> ModuleWriter m ProtectDef
genFeature unsafeFFIDefaultDef VkFeature {..} = hoist (`evalStateT` mempty) $ do
    curlvl <- getCurrentSecLvl
    vkXml <- ask
    let tps = globTypes vkXml
        cmds = globCommands vkXml
        ffiPDef = nativeFFIProtectDef name
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
    pushSecLvl $ \lvl -> mapM_ (genRequire unsafeFFIDefaultDef (Just . NFFIGuarded $ ffiPDef) lvl tps cmds) reqList
    return ffiPDef


genRequire :: MonadState (Set (ModuleName ())) m
          => ProtectDef
          -> Maybe NativeFFI -- ^ whether to generate FFI
          -> Int
          -> Map.Map VkTypeName VkType
          -> Map.Map VkCommandName VkCommand
          -- -> Map.Map VkEnumName VkEnum
          -> VkRequire
          -> ModuleWriter m ()
genRequire unsafeFFIDefaultDef mNativeFFI curlvl tps cmds VkRequire {..} = do
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
  let nativeFFI = flip fromMaybe mNativeFFI
        $ NFFIGuarded . nativeFFIProtectDef
        $ fromMaybe "VK_VERSION_1_0" requireFeature
  tNames <- fmap mconcat $
    forM requireComms $ \cname -> case Map.lookup cname cmds of
        Nothing -> pure mempty
        Just t  -> genCommand unsafeFFIDefaultDef nativeFFI t

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


nativeFFIProtectDef :: T.Text -> ProtectDef
nativeFFIProtectDef t = ProtectDef
  { protectFlag = ProtectFlag $ "useNativeFFI-" <> T.map f (T.dropWhile (not . C.isDigit) t)
  , protectCPP  = ProtectCPP $ "NATIVE_FFI_" <> t
  }
  where
    f c | C.isDigit c = c
        | otherwise = '-'


coreVersion :: ProtectDef -> T.Text
coreVersion = T.map f . T.dropWhile (not . C.isDigit) . unProtectCPP . protectCPP
  where
    f c | C.isDigit c = c
        | otherwise = '.'
