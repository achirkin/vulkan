{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write.Commands
  ( genBaseCommands, genCommand
  ) where

import           Control.Monad                        (forM_, join)
import           Control.Monad.Reader.Class
import           Data.Maybe                           (isJust)
import           Data.Semigroup
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Commands
import           VkXml.Sections.Extensions
import           VkXml.Sections.Feature

import           Write.ModuleWriter


genBaseCommands :: Monad m => ModuleWriter m ()
genBaseCommands = do
    vkXml <- ask
    let featureComms = Set.fromList
                     . join
                     . map requireComms
                     . reqList . unInorder $ globFeature vkXml
        extComms = Set.fromList
                     $ extensions (unInorder $ globExtensions vkXml)
                       >>= extRequires >>= requireComms
        excludedComms = Set.union featureComms extComms

    forM_ (commands . unInorder $ globCommands vkXml) $ \c ->
      if (name :: VkCommand -> VkCommandName) c `Set.member` excludedComms
      then pure ()
      else genCommand c



genCommand :: Monad m => VkCommand -> ModuleWriter m ()
genCommand VkCommand
  { name = vkname
  , returnType = vkrt
  , attributes = attrs@VkCommandAttrs
    { comment = mtxt
    }
  , parameters = vkpams
  } = do
    regLink <- vkRegistryLink $ unVkCommandName vkname
    let rezComment = appendComLine rezComment' regLink
                 >>= preComment . T.unpack

    writePragma "ForeignFunctionInterface"
    writeFullImport "Graphics.Vulkan.Marshal"

    writeImport $ DIThing (qNameTxt . unqualifyQ . toHaskellName $ vkrt) DITNo
    forM_ vkpams $ \p ->
      writeImport $ DIThing (qNameTxt . unqualifyQ . toHaskellName $ paramType p) DITNo

    writeDecl $ ForImp rezComment (CCall Nothing) (Just (PlayRisky Nothing))
                      (Just cnameOrigStr) (Ident Nothing cnameStr) funtype

    writeExport $ DIVar $ qNameTxt cname
  where
    cname = unqualifyQ $ toHaskellName vkname
    cnameStr = T.unpack  $ qNameTxt cname
    cnameOrigStr = T.unpack $ unVkCommandName vkname
    -- funtypeTxt = T.pack $ prettyPrint funtype
    rtname = unqualifyQ $ toHaskellName vkrt
    rtype = (Nothing <$)
          $ TyApp () (TyCon () (UnQual () (Ident () "IO"))) (toType 0 rtname)
    funtype = foldr accumRefs rtype vkpams
    paramT VkCommandParam {..}
      = let n = paramTypeRefLvl + if isJust paramArraySize
                                  then 1 else 0
        in amap (const . Just . CodeComment NextToCode '^' $ T.unpack paramName)
         . (Nothing <$)
         . toType n $ unqualifyQ $ toHaskellName paramType
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
                 . ml (queues attrs)     (\x -> "queues: @" <> x <> "@")
                 . ml (renderpass attrs) (\x -> "renderpass: @" <> x <> "@")
                 . ml (pipeline attrs)   (\x -> "pipeline: @" <> x <> "@")
                 . map ("> " <>) $ T.lines c
