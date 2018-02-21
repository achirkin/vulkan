{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Strict                #-}
-- | Generate function pointer types
module Write.Types.Funcpointer
  ( genFuncpointer
  ) where


import           Control.Arrow
import           Control.Monad                        (forM_)
import           Data.Char                            (toUpper)
import           Data.Semigroup
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections.Types

import           Write.ModuleWriter


genFuncpointer :: Monad m => VkType -> ModuleWriter m ()
genFuncpointer vkt@VkTypeSimple
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , typeData = VkTypeData
       { retType = Just (rtypeName, rstars)
       , reference = refs'
       , comment = mtxt2
       , code = c
       }
    }
    | rtype <- TyApp () (TyCon () (UnQual () (Ident () "IO")))
             $ toType rstars rtypeName
    , funtype <- foldr accumRefs rtype refs
    , pfuntype <- TyApp () (TyCon () (UnQual () (Ident () "FunPtr")))
                           (TyCon () tfname)
    = do
    writeFullImport "Graphics.Vulkan.Marshal"
    writeImport $ DIThing "Ptr" DITEmpty
    writeImport $ DIThing "FunPtr" DITEmpty
    writeImport $ DIThing "Void" DITEmpty
    writeImport $ DIThing "CString" DITNo
    writeImport $ DIThing "VkFlags" DITAll
    forM_ (requiresTypes vkt) $ \(VkTypeName t) ->
      writeImport $ DIThing t DITAll
    writeDecl . (Nothing <$) $
      TypeDecl () (DHead () $ unqualify tfname) funtype
    writeDecl . setComment rezComment
              . (Nothing <$)
              $
      TypeDecl () (DHead () $ unqualify tname) pfuntype
    writeDecl
      . setComment
        (preComment $ "Wrap haskell function into C-callable FunPtr.\n"
                    <> "Note, you need to free resources after using it.")
      $ parseDecl'
      [text|
        foreign import ccall "wrapper"
            $newFun :: $tfnametxt -> IO $tnametxt
      |]
    writeDecl $ parseDecl'
      [text|
        foreign import ccall "dynamic"
            $unwrapFun :: $tnametxt -> $tfnametxt
      |]
    writeExport $ DIThing tnametxt DITNo
    writeExport $ DIThing tfnametxt DITNo
    writeExport $ DIVar newFun
    writeExport $ DIVar unwrapFun
  where
    tname = toQName vkTName
    tnametxt = unVkTypeName vkTName
    tfname = toQName . VkTypeName $ "HS_" <> tnamebasetxt
    tfnametxt = qNameTxt tfname
    tnamebasetxt = T.drop 1 . T.dropWhile ('_' /=) $ unVkTypeName vkTName
    tnamebasetxtC = case T.uncons tnamebasetxt of
                      Just (x, xs) -> toUpper x `T.cons` xs
                      Nothing      -> tnamebasetxt
    newFun = "new" <> tnamebasetxtC
    unwrapFun = "unwrap" <> tnamebasetxtC
    refs = map (second length) refs'
    accumRefs (tn, k) = TyFun () (toType (fromIntegral k) tn)
    rezComment = rezComment'' >>= preComment . T.unpack
    rezComment'' = appendComLine rezComment'
                 $ T.unlines . map ("> " <>) $ T.lines c
    rezComment' = if txt == mempty
                  then mtxt2
                  else case mtxt2 of
                    Nothing   -> Just txt
                    Just txt2 -> appendComLine (Just txt) txt2
genFuncpointer t
  = error $ "genFuncpointer: expected a simple funcpointer type, "
          <> "defined as 'typedef typename (..', but got: "
          <> show t
