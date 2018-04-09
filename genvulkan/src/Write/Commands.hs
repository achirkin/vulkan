{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE QuasiQuotes                #-}
module Write.Commands
  ( genCommand
  ) where

import           Control.Monad                        (forM_, when)
import           Control.Monad.Reader.Class
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (isJust)
import           Data.Semigroup
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Data.Char (toUpper)
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections.Commands
import           VkXml.Sections.Enums
import           VkXml.Sections

import           Write.Types.Enum
import           Write.ModuleWriter


genCommand :: Monad m => Bool -> VkCommand -> ModuleWriter m (Set VkTypeName)
genCommand genFFI command@VkCommand
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
    let rezComment = appendComLine rezComment' regLink
                 >>= preComment . T.unpack

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

    when genFFI $ do
      -- foreign import unsafe
      writeDecl $ ForImp rezComment (CCall Nothing) (Just (PlayRisky Nothing))
                        (Just cnameOrigStr) (Ident Nothing cnameStr) funtype

      -- foreign import safe
      writeDecl $ ForImp rezComment (CCall Nothing) (Just (PlaySafe Nothing False))
                        (Just cnameOrigStr) (Ident Nothing cnameSafeStr) funtype

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
    vknameSafe = VkCommandName $ unVkCommandName vkname <> "Safe"
    cnameSafe = toQName vknameSafe
    cnameSafeStr = T.unpack $ qNameTxt cnameSafe
    cname = toQName vkname
    cnameStr = T.unpack $ qNameTxt cname
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
      "" -> ""
      (x:xs) -> toUpper x : xs
    unwrapFun = T.pack $ "unwrap" <> firstUpCName
    vkInstanceProcSymbol = T.pack $ '_' : firstUpCName

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
