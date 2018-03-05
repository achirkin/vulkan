{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Commands
  ( genCommand
  ) where

import           Control.Monad                        (forM_, when)
import           Data.Maybe                           (isJust)
import           Data.Semigroup
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax

import           VkXml.CommonTypes
import           VkXml.Sections.Commands

import           Write.ModuleWriter



genCommand :: Monad m => VkCommand -> ModuleWriter m (Set VkTypeName)
genCommand command@VkCommand
  { cName = vkname
  , cNameOrig = cnameOrigTxt
  , cReturnType = vkrt
  , cAttributes = attrs@VkCommandAttrs
    { cComment = mtxt
    }
  , cParameters = vkpams
  } = do
    regLink <- vkRegistryLink $ unVkCommandName vkname
    let rezComment = appendComLine rezComment' regLink
                 >>= preComment . T.unpack

    writePragma "ForeignFunctionInterface"
    writeFullImport "Graphics.Vulkan.Marshal"
    forM_ (requiresTypes command) $ \p ->
      let t = unVkTypeName p
          dit = if "Vk" `T.isPrefixOf` t
                then DITAll else DITNo
      in do
        when ("Flags" `T.isInfixOf` t || "FlagBits" `T.isInfixOf` t) $ do
          writeOptionsPragma (Just GHC) "-fno-warn-unused-imports"
          writeImport $ DIThing "VkFlags" DITAll
        writeImport $ DIThing t dit

    writeDecl $ ForImp rezComment (CCall Nothing) (Just (PlayRisky Nothing))
                      (Just cnameOrigStr) (Ident Nothing cnameStr) funtype

    writeDecl $ ForImp rezComment (CCall Nothing) (Just (PlaySafe Nothing False))
                      (Just cnameOrigStr) (Ident Nothing cnameSafeStr) funtype

    writeExport . DIVar $ unVkCommandName vkname
    writeExport . DIVar $ unVkCommandName vknameSafe
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
