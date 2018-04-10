{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Commands
  ( genCommand, NativeFFI (..)
  ) where

import           Control.Monad                        (forM_, when)
import           Control.Monad.Reader.Class
import           Data.Char                            (toUpper)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromMaybe, isJust)
import           Data.Semigroup
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Commands
import           VkXml.Sections.Enums

import           Write.ModuleWriter
import           Write.Types.Enum

-- | Ways to generate FFI code for vulkan commands.
data NativeFFI
  = NFFIDisable
    -- ^ Do not generate FFI callbacks at all
  | NFFIGuarded ProtectDef
    -- ^ Generate FFI callbacks if CPP definition is present
  deriving (Eq, Ord, Show)

data StubType
  = InstanceOp Bool -- is instance null?
  | DeviceOp
  | ErrorOp
  deriving Eq


genCommand :: Monad m => NativeFFI -> VkCommand -> ModuleWriter m (Set VkTypeName)
genCommand nativeFFI command@VkCommand
  { cName = vkname
  , cNameOrig = cnameOrigTxt
  , cReturnType = vkrt
  , cAttributes = attrs@VkCommandAttrs
    { cComment = mtxt
    }
  , cParameters = vkpams
  } = do

  -- enum patterns
  _ <- enumPattern VkEnum
    { _vkEnumName = VkEnumName (T.pack firstUpCName)
    , _vkEnumTName = Nothing
    , _vkEnumComment = ""
    , _vkEnumValue = VkEnumString cnameOrigTxt
    }

  -- command itself
  indeed <- isIdentDeclared $ DIVar unwrapFun
  if indeed
  then do
    writeAllImports
    writeAllExports
    return Set.empty
  else do
    regLink <- vkRegistryLink $ unVkCommandName vkname
    let funComment = appendComLine rezComment' regLink
        rezComment = funComment >>= preComment . T.unpack

    writePragma "ForeignFunctionInterface"
    writeFullImport "Graphics.Vulkan.Marshal"
    writeImport $ DIThing "FunPtr" DITEmpty
    forM_ (requiresTypes command) $ \p ->
      let t = unVkTypeName p
          dit = if "Vk" `T.isPrefixOf` t
                then DITAll else DITNo
      in do
        when ("Flags" `T.isInfixOf` t || "FlagBits" `T.isInfixOf` t) $ do
          writeOptionsPragma (Just GHC) "-fno-warn-unused-imports"
          writeImport $ DIThing "VkFlags" DITAll
        writeImport $ DIThing t dit

    case nativeFFI of
      NFFIDisable -> pure ()
      NFFIGuarded pDef -> do

        writePragma "CPP"
        writePragma "TypeApplications"
        writeImport $ DIVar "unsafeDupablePerformIO"
        writeFullImport "Graphics.Vulkan.Marshal.Proc"
        writeOptionsPragma (Just GHC) "-fno-warn-unused-imports"

        let pFlagTxt = unProtectFlag (protectFlag pDef)
            pCppTxt  = unProtectCPP  (protectCPP pDef)
            comment1 = Just . CodeComment AboveCode ' ' . T.unpack $ T.unlines
                     [ "|"
                     , fromMaybe mempty funComment
                     , "###ifdef " <> pCppTxt
                     ]
            comment2 = Just . CodeComment AboveCode ' ' $ T.unpack $ case stubType of
                InstanceOp _ ->
                  [text|
                    ###else
                    Note: without @$pFlagTxt@ cabal flag this function may call `vkGetInstanceProcAddr` every time you execute it.
                    Either lookup the function manually or enable @$pFlagTxt@ cabal flag to call it natively to make sure you get the best performance.
                  |]
                DeviceOp ->
                  [text|
                    ###else
                    Note: without @$pFlagTxt@ cabal flag this function may call `vkGetDeviceProcAddr` every time you execute it.
                    Either lookup the function manually or enable @$pFlagTxt@ cabal flag to call it natively to make sure you get the best performance.
                  |]
                ErrorOp ->
                  [text|
                    ###else
                    Warning: without @$pFlagTxt@ cabal flag this function returns error!
                    Either lookup the function manually or enable @$pFlagTxt@ cabal flag.
                  |]
            comment3 = Just $ CodeComment BelowCode ' ' "###endif"
            annWarn = case stubType of
              InstanceOp _ ->
                "This function could be very inefficient. "
                  ++ "It may call vkGetInstanceProcAddr every time you call it. "
                  ++ "I suggest you to either lookup the function address manually or enable flag " ++ T.unpack pFlagTxt
              DeviceOp ->
                "This function could be very inefficient. "
                  ++ "It may call vkGetDeviceProcAddr every time you call it. "
                  ++ "I suggest you to either lookup the function address manually or enable flag " ++ T.unpack pFlagTxt
              ErrorOp ->
                "This function will return error! "
                ++ "Either lookup the function address manually or enable flag " ++ T.unpack pFlagTxt


        -- foreign import unsafe
        writeDecl $ ForImp comment1 (CCall Nothing) (Just (PlayRisky Nothing))
                          (Just cnameOrigStr) (Ident Nothing cnameStr) funtype
        writeDecl $ TypeSig comment2 [Nothing <$ unqualify cname] funtype
        writeDecl $ parseDecl' $ case stubType of
            InstanceOp False -> [text|
                $cnameTxt d =
                   unsafeDupablePerformIO (vkGetInstanceProc @$vkInstanceProcSymbolT d) d
              |]
            InstanceOp True -> [text|
                $cnameTxt =
                   unsafeDupablePerformIO (vkGetInstanceProc @$vkInstanceProcSymbolT VK_NULL)
              |]
            DeviceOp -> [text|
                $cnameTxt d =
                   unsafeDupablePerformIO (vkGetDeviceProc @$vkInstanceProcSymbolT d) d
              |]
            ErrorOp -> [text|
                $cnameTxt = error $ "Cannot lookup C symbol \"$cnameTxt\" because its signature does not provide VkInstance argument. "
                         ++ "Either lookup the function manually or enable $pFlagTxt cabal flag."
              |]
        when (stubType /= ErrorOp) $
          writeDecl $ InlineSig Nothing True Nothing (Nothing <$ cname)
        writeDecl $ WarnPragmaDecl comment3 [([Nothing <$ unqualify cname], annWarn)]

        -- foreign import safe
        writeDecl $ ForImp comment1 (CCall Nothing) (Just (PlaySafe Nothing False))
                          (Just cnameOrigStr) (Ident Nothing cnameSafeStr) funtype
        writeDecl $ TypeSig comment2 [Nothing <$ unqualify cnameSafe] funtype
        writeDecl $ parseDecl'
          [text|$cnameSafeTxt = $cnameTxt|]
        writeDecl $ InlineSig Nothing True Nothing (Nothing <$ cnameSafe)
        writeDecl $ WarnPragmaDecl comment3 [([Nothing <$ unqualify cnameSafe], annWarn)]


    -- type synonym
    writeDecl $ TypeDecl rezComment
      (DHead Nothing $ Ident Nothing funTypeNameStrHS) funtype

    -- FunPtr type synonym
    writeDecl $ TypeDecl Nothing
      (DHead Nothing $ Ident Nothing funTypeNameStrPFN) funTypePFN

    -- unwrap C function pointer
    writeDecl $ parseDecl'
      [text|
        foreign import ccall "dynamic"
            $unwrapFun :: $funTypeNameTxtPFN -> $funTypeNameTxtHS
      |]

    -- symbol discovery instance
    writeImport $ DIThing "VulkanProc" DITAll
    writePragma "TypeFamilies"
    writePragma "FlexibleInstances"
    writeOptionsPragma (Just GHC) "-fno-warn-orphans"
    writeDecl $ parseDecl'
      [text|
        instance VulkanProc "$cnameOrigTxt" where
          type VkProcType "$cnameOrigTxt" = $funTypeNameTxtHS
          vkProcSymbol = $vkInstanceProcSymbol
          {-# INLINE vkProcSymbol #-}
          unwrapVkProcPtr = $unwrapFun
          {-# INLINE unwrapVkProcPtr #-}
      |]

    writeAllExports
    -- reexport all dependent types
    return . Set.fromList
           . filter (T.isInfixOf "Vk" . unVkTypeName)
           $ requiresTypes command
  where
    -- find out if we can use vkGetInstanceProcAddr or alike
    stubType = case (cnameOrigTxt, map paramT vkpams) of
      ("vkCreateInstance", _)
        -> InstanceOp True
      ("vkEnumerateInstanceLayerProperties", _)
        -> InstanceOp True
      ("vkEnumerateInstanceExtensionProperties", _)
        -> InstanceOp True
      (_, TyCon _ (UnQual _ (Ident _ "VkInstance")) : _)
        -> InstanceOp False
      (_, TyCon _ (UnQual _ (Ident _ "VkDevice")) : _)
        -> DeviceOp
      (_, _)
        -> ErrorOp

    genFFI = nativeFFI /= NFFIDisable
    vknameSafe = VkCommandName $ unVkCommandName vkname <> "Safe"
    cnameSafe = toQName vknameSafe
    cnameSafeTxt = qNameTxt cnameSafe
    cnameSafeStr = T.unpack cnameSafeTxt
    cname = toQName vkname
    cnameTxt = qNameTxt cname
    cnameStr = T.unpack cnameTxt
    cnameOrigStr = T.unpack cnameOrigTxt
    funTypeNameStrHS = "HS_" <> cnameStr
    funTypeNameStrPFN = "PFN_" <> cnameStr
    funTypeNameTxtHS = T.pack funTypeNameStrHS
    funTypeNameTxtPFN = T.pack funTypeNameStrPFN
    funTypePFN = TyApp Nothing
      (TyCon Nothing (UnQual Nothing (Ident Nothing "FunPtr")))
      (TyCon Nothing (UnQual Nothing (Ident Nothing funTypeNameStrHS)))
    rtype = (Nothing <$)
          $ TyApp () (TyCon () (UnQual () (Ident () "IO"))) (toType 0 vkrt)
    funtype = foldr accumRefs rtype vkpams
    paramT VkCommandParam {..}
      = let n = paramTypeRefLvl + if isJust paramArraySize
                                  then 1 else 0
        in amap (const . Just . CodeComment NextToCode '^' $ T.unpack paramName)
         . (Nothing <$)
         $ toType n paramType
    accumRefs vkp = TyFun Nothing (paramT vkp)

    c = T.unlines $
          [unVkTypeName vkrt <> " " <> unVkCommandName vkname]
       <> mapPam vkpams
       <> ["    )"]
    mapPam (x:xs) = "    ( " <> code x : map (("    , " <>) . code) xs
    mapPam []     = ["    ("]
    ml Nothing  _ = id
    ml (Just x) f = (f x :) . ("":)
    al [] _ = id
    al xs f = (f xs :) . ("":)
    rezComment' = appendComLine mtxt
                 $ T.unlines
                 . al (successcodes attrs)
                      (\x -> "Success codes: "
                          <> T.intercalate ", "
                            ( map (\t -> "'" <> unVkEnumName t <> "'")
                              x) <> "."
                      )
                 . al (errorcodes attrs)
                      (\x -> "Error codes: "
                          <> T.intercalate ", "
                            ( map (\t -> "'" <> unVkEnumName t <> "'")
                              x) <> "."
                      )
                 . al (queues attrs)
                      (\x -> "queues: "
                          <> T.intercalate ", "
                            ( map (\t -> "'" <> t <> "'")
                              x) <> "."
                      )
                 . ml (renderpass attrs) (\x -> "renderpass: @" <> x <> "@")
                 . ml (pipeline attrs)   (\x -> "pipeline: @" <> x <> "@")
                 . map ("> " <>) $ T.lines c


    firstUpCName = case T.unpack (unVkCommandName vkname) of
      ""     -> ""
      (x:xs) -> toUpper x : xs
    unwrapFun = T.pack $ "unwrap" <> firstUpCName
    vkInstanceProcSymbol = T.pack $ '_' : firstUpCName
    vkInstanceProcSymbolT = T.pack firstUpCName

    writeAllImports = do
      writeImport $ DIThing funTypeNameTxtHS DITNo
      writeImport $ DIThing funTypeNameTxtPFN DITNo
      writeImport $ DIVar unwrapFun
      when genFFI $ do
        writeImport . DIVar $ unVkCommandName vkname
        writeImport . DIVar $ unVkCommandName vknameSafe

    writeAllExports = do
      writeExport $ DIThing funTypeNameTxtHS DITNo
      writeExport $ DIThing funTypeNameTxtPFN DITNo
      writeExport $ DIVar unwrapFun
      when genFFI $ do
        writeExport . DIVar $ unVkCommandName vkname
        writeExport . DIVar $ unVkCommandName vknameSafe



genCommand genFFI acom@(VkCommandAlias comname comalias comnameOrig)
  = ask >>= \vk -> case Map.lookup comalias (globCommands vk) of
      Nothing -> error $
        "Could not find a command for an alias " <> show acom
      Just c  -> genCommand genFFI
        c{ cName = comname
         , cNameOrig = comnameOrig
         , cAttributes = (cAttributes c)
            { cComment = appendComLine (cComment (cAttributes c)) $
                 "This is an alias for `" <> unVkCommandName comalias <> "`."
            }
         }
