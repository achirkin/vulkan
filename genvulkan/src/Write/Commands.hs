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


genCommand :: Monad m => ProtectDef -> NativeFFI -> VkCommand -> ModuleWriter m (Set VkTypeName)
genCommand unsafeFFIDefaultDef nativeFFI command@VkCommand
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
  indeed <- isIdentDeclared $ DIThing funTypeNameTxtHS DITNo
  if indeed
  then do
    writeAllImports
    writeAllExports
    return Set.empty
  else do
    regLink <- vkRegistryLink $ unVkCommandName vkname
    let funComment = appendComLine rezComment' regLink
        rezComment = funComment >>= preComment . T.unpack

    writePragma "CPP"
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

    let
      pUnsafeFFIDefaultFlagTxt = unProtectFlag (protectFlag unsafeFFIDefaultDef)
      pUnsafeFFIDefaultCppTxt = unProtectCPP (protectCPP unsafeFFIDefaultDef)

    case nativeFFI of
      NFFIDisable -> pure ()
      NFFIGuarded pDef -> do

        writePragma "TypeApplications"
        writeImport $ DIVar "unsafeDupablePerformIO"
        writeFullImport "Graphics.Vulkan.Marshal.Proc"
        writeOptionsPragma (Just GHC) "-fno-warn-unused-imports"

        let pFlagTxt = unProtectFlag (protectFlag pDef)
            pCppTxt  = unProtectCPP  (protectCPP pDef)
            stubFunTxt = "my" <> T.drop 2 cnameTxt
            mkStubFun  = case stubType of
              InstanceOp False -> [text|
                  $stubFunTxt <- vkGetInstanceProc @$vkInstanceProcSymbolT vkInstance
                |]
              InstanceOp True -> [text|
                  $stubFunTxt <- vkGetInstanceProc @$vkInstanceProcSymbolT VK_NULL
                |]
              DeviceOp -> [text|
                  $stubFunTxt <- vkGetDeviceProc @$vkInstanceProcSymbolT vkDevice
                |]
              ErrorOp -> [text|
                  $stubFunTxt <- vkGetInstanceProc @$vkInstanceProcSymbolT vkInstance
                |]

            aboveCode = Just . CodeComment AboveCode ' '
            belowCode = Just . CodeComment BelowCode ' '

            comment = T.unpack . T.unlines $
               [ "|"
               , fromMaybe mempty funComment
               , ""
               , [text|
                    __Note:__ When @$pFlagTxt@ cabal flag is enabled, this function is linked statically
                              as a @foreign import@ call to C Vulkan loader.
                              Otherwise, it is looked up dynamically at runtime using dlsym-like machinery (platform-dependent).

                    Independently of the flag setting, you can lookup the function manually at runtime:

                    > $mkStubFun

                    or less efficient:

                    > $stubFunTxt <- vkGetProc @$vkInstanceProcSymbolT

                    __Note:__ @$cnameUnsafeTxt@ and @$cnameSafeTxt@ are the @unsafe@ and @safe@
                              FFI imports of this function, respectively. @$cnameTxt@ is an alias
                              of @$cnameUnsafeTxt@ when the @$pUnsafeFFIDefaultFlagTxt@ cabal flag
                              is enabled; otherwise, it is an alias of @$cnameSafeTxt@.
                 |]
               ]
            cppIfdef cpp = "###ifdef " ++ T.unpack cpp
            commentAndCppIfdef cpp = unlines [comment, cppIfdef cpp]
            cppElse = "###else"
            cppEndif = "###endif"

        -- foreign import unsafe
        writeDecl $ ForImp (aboveCode $ commentAndCppIfdef pCppTxt) (CCall Nothing) (Just (PlayRisky Nothing))
                          (Just cnameOrigStr) (Ident Nothing cnameUnsafeStr) funtype
        writeDecl $ TypeSig (aboveCode cppElse) [Nothing <$ unqualify cnameUnsafe] funtype
        writeDecl $ parseDecl' $ [text|
            $cnameUnsafeTxt = unsafeDupablePerformIO (vkGetProcUnsafe @$vkInstanceProcSymbolT)
          |]
        writeDecl $ InlineSig (belowCode cppEndif) False Nothing (Nothing <$ cnameUnsafe)

        -- foreign import safe
        writeDecl $ ForImp (aboveCode $ commentAndCppIfdef pCppTxt) (CCall Nothing) (Just (PlaySafe Nothing False))
                          (Just cnameOrigStr) (Ident Nothing cnameSafeStr) funtype
        writeDecl $ TypeSig (aboveCode cppElse) [Nothing <$ unqualify cnameSafe] funtype
        writeDecl $ parseDecl' $ [text|
            $cnameSafeTxt = unsafeDupablePerformIO (vkGetProcSafe @$vkInstanceProcSymbolT)
          |]
        writeDecl $ InlineSig (belowCode cppEndif) False Nothing (Nothing <$ cnameSafe)

        -- alias to default foreign import
        writeDecl $ TypeSig (aboveCode comment) [Nothing <$ unqualify cname] funtype
        writeDecl . setComment (aboveCode $ cppIfdef pUnsafeFFIDefaultCppTxt) $ parseDecl' [text|
            $cnameTxt = $cnameUnsafeTxt
          |]
        writeDecl . setComment (aboveCode cppElse) $ parseDecl' [text|
            $cnameTxt = $cnameSafeTxt
          |]
        writeDecl $ InlineSig (aboveCode cppEndif) True Nothing (Nothing <$ cname)

    -- type synonym
    writeDecl $ TypeDecl rezComment
      (DHead Nothing $ Ident Nothing funTypeNameStrHS) funtype

    -- FunPtr type synonym
    writeDecl $ TypeDecl Nothing
      (DHead Nothing $ Ident Nothing funTypeNameStrPFN) funTypePFN

    -- unwrap C function pointer
    writeDecl $ parseDecl'
      [text|
        foreign import ccall unsafe "dynamic"
            $unwrapFunUnsafe :: $funTypeNameTxtPFN -> $funTypeNameTxtHS
      |]
    writeDecl $ parseDecl'
      [text|
        foreign import ccall safe "dynamic"
            $unwrapFunSafe :: $funTypeNameTxtPFN -> $funTypeNameTxtHS
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
          unwrapVkProcPtrUnsafe = $unwrapFunUnsafe
          {-# INLINE unwrapVkProcPtrUnsafe #-}
          unwrapVkProcPtrSafe = $unwrapFunSafe
          {-# INLINE unwrapVkProcPtrSafe #-}
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
    vknameUnsafe = VkCommandName $ unVkCommandName vkname <> "Unsafe"
    cnameUnsafe = toQName vknameUnsafe
    cnameUnsafeTxt = qNameTxt cnameUnsafe
    cnameUnsafeStr = T.unpack cnameUnsafeTxt
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
          [cReturnTypeOrig command <> " " <> unVkCommandName vkname]
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
                      (\x -> "Queues: "
                          <> T.intercalate ", "
                            ( map (\t -> "'" <> t <> "'")
                              x) <> "."
                      )
                 . ml (renderpass attrs) (\x -> "Renderpass: @" <> x <> "@")
                 . ml (pipeline attrs)   (\x -> "Pipeline: @" <> x <> "@")
                 . map ("> " <>) $ T.lines c


    firstUpCName = case T.unpack (unVkCommandName vkname) of
      ""     -> ""
      (x:xs) -> toUpper x : xs
    unwrapFunUnsafe = T.pack $ "unwrap" <> firstUpCName <> "Unsafe"
    unwrapFunSafe = T.pack $ "unwrap" <> firstUpCName <> "Safe"
    vkInstanceProcSymbol = T.pack $ '_' : firstUpCName
    vkInstanceProcSymbolT = T.pack firstUpCName

    writeAllImports = do
      writeImport $ DIThing funTypeNameTxtHS DITNo
      writeImport $ DIThing funTypeNameTxtPFN DITNo
      -- writeImport $ DIVar unwrapFunUnsafe
      when genFFI $ do
        writeImport . DIVar $ unVkCommandName vkname
        writeImport . DIVar $ unVkCommandName vknameUnsafe
        writeImport . DIVar $ unVkCommandName vknameSafe

    writeAllExports = do
      writeExport $ DIThing funTypeNameTxtHS DITNo
      writeExport $ DIThing funTypeNameTxtPFN DITNo
      -- writeExport $ DIVar unwrapFunUnsafe -- don't really need it because it is exposed via VulkanProc instance
      when genFFI $ do
        writeExport . DIVar $ unVkCommandName vkname
        writeExport . DIVar $ unVkCommandName vknameUnsafe
        writeExport . DIVar $ unVkCommandName vknameSafe



genCommand unsafeFFIDefaultDef genFFI acom@(VkCommandAlias comname comalias comnameOrig)
  = ask >>= \vk -> case Map.lookup comalias (globCommands vk) of
      Nothing -> error $
        "Could not find a command for an alias " <> show acom
      Just c  -> genCommand unsafeFFIDefaultDef genFFI
        c{ cName = comname
         , cNameOrig = comnameOrig
         , cAttributes = (cAttributes c)
            { cComment = appendComLine (cComment (cAttributes c)) $
                 "This is an alias for `" <> unVkCommandName comalias <> "`."
            }
         }
