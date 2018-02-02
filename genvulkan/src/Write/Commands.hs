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
        selectTN (VkExtReqCommand tn) = [tn]
        selectTN _                    = []
        extComms = Set.fromList
                     . join . join . join
                     . map (map (map (selectTN . fst). items). extRequires)
                     . extensions . unInorder $ globExtensions vkXml
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
    writeImport "Foreign.C.Types" $ IThingAll () (Ident () "CChar")
    writeImport "Foreign.C.Types" $ IThingAll () (Ident () "CSize")
    writeImport "Foreign.C.Types" $ IThingAll () (Ident () "CInt")
    writeImport "Foreign.C.Types" $ IThingAll () (Ident () "CULong")
    writeImport "Foreign.C.Types" $ IThingAll () (Ident () "CFloat")
    writeImport "Foreign.Ptr"     $ IAbs () (NoNamespace ()) (Ident () "Ptr")
    writeImport "Data.Int"        $ IAbs () (NoNamespace ()) (Ident () "Int32")
    writeImport "Data.Word"       $ IAbs () (NoNamespace ()) (Ident () "Word32")
    writeImport "Data.Word"       $ IAbs () (NoNamespace ()) (Ident () "Word64")
    writeImport "Data.Void"       $ IAbs () (NoNamespace ()) (Ident () "Void")
    writeDecl $ ForImp rezComment (CCall Nothing) (Just (PlayRisky Nothing))
                      (Just cnameOrigStr) (Ident Nothing cnameStr) funtype

    writeExport $ EVar () (UnQual () (Ident () cnameStr))
  where
    cname = toHaskellVar vkname
    cnameStr = T.unpack  $ qNameTxt cname
    cnameOrigStr = T.unpack $ unVkCommandName vkname
    -- funtypeTxt = T.pack $ prettyPrint funtype
    rtname = toHaskellType vkrt
    rtype = (Nothing <$)
          $ TyApp () (TyCon () (UnQual () (Ident () "IO"))) (toType 0 rtname)
    funtype = foldr accumRefs rtype vkpams
    paramT VkCommandParam {..}
      = let n = paramTypeRefLvl + if isJust paramArraySize
                                  then 1 else 0
        in amap (const . Just . CodeComment NextToCode '^' $ T.unpack paramName)
         . (Nothing <$)
         . toType n $ toHaskellType paramType
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
                            ( map (\t -> "'" <> unVkEnumValueName t <> "'")
                              x) <> "."
                      )
                 . al (errorcodes attrs)
                      (\x -> "Error codes: "
                          <> T.intercalate ", "
                            ( map (\t -> "'" <> unVkEnumValueName t <> "'")
                              x) <> "."
                      )
                 . ml (queues attrs)     (\x -> "queues: @" <> x <> "@")
                 . ml (renderpass attrs) (\x -> "renderpass: @" <> x <> "@")
                 . ml (pipeline attrs)   (\x -> "pipeline: @" <> x <> "@")
                 . map ("> " <>) $ T.lines c
