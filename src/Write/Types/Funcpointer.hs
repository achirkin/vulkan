{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
-- | Generate function pointer types
module Write.Types.Funcpointer
  ( genFuncpointer
  ) where


import           Control.Arrow
import           Data.Semigroup
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax

import           VkXml.CommonTypes
import           VkXml.Sections.Types

import           Write.ModuleWriter


genFuncpointer :: Monad m => VkType -> ModuleWriter m ()
genFuncpointer VkTypeSimple
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , typeData = VkTypeData
       { reference = refs'
       , comment = mtxt2
       , code = c
       }
    }
    | rtypeTxt:_ <-  T.words
                   . T.strip . snd
                   . T.breakOnEnd "typedef" . fst
                   $ T.breakOn (unVkTypeName vkTName) c
    , rtype <- TyApp () (TyCon () (UnQual () (Ident () "IO")))
             . uncurry (flip toType)
             . first toHaskellType $ countStars rtypeTxt
    = do
    writeImport "Foreign.Ptr"
      $ IAbs () (NoNamespace ()) (Ident () "Ptr")
    writeImport "Foreign.Ptr"
      $ IAbs () (NoNamespace ()) (Ident () "FunPtr")
    writeImport "Data.Void"
      $ IAbs () (NoNamespace ()) (Ident () "Void")
    writeDecl . setComment rezComment
              . (Nothing <$)
              $
      TypeDecl ()
      (DHead () $ unqualify tname)
      (foldr accumRefs rtype refs)
    writeExport $ EAbs () (NoNamespace ()) tname
  where
    tname = toHaskellType vkTName
    countStars s | "*" `T.isSuffixOf` s = second (1+) $ countStars (T.dropEnd 1 s)
                 | otherwise = (VkTypeName s, 0)
    refs = map (second length) refs'
    accumRefs (tn, k) = TyFun () (toType (fromIntegral k) (toHaskellType tn))
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
