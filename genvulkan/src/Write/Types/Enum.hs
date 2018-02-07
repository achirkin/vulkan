{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
-- Generate enums and bitmasks
module Write.Types.Enum
  ( genEnum
  , genAlias
  , genApiConstants
  , enumPattern
  ) where

import           Control.Monad
import           Control.Monad.Reader.Class
import qualified Control.Monad.Trans.RWS.Strict       as RWS
import qualified Data.Map.Strict                      as Map
import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           NeatInterpolation

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Enums
import           VkXml.Sections.Types

import           Write.ModuleWriter


genApiConstants :: Monad m => ModuleWriter m ()
genApiConstants = do
  glvl <- ModuleWriter $ RWS.gets currentSecLvl
  writeSection glvl "API Constants"
  vk <- ask
  pushSecLvl . const $ mapM_ genEnums
    (unInorder <$> Map.lookup Nothing (globEnums vk))


-- | Lookup an enum in vk.xml and generate code for it
genEnum :: Monad m => VkType -> ModuleWriter m ()
genEnum t = ask >>= \vk -> case unInorder <$> Map.lookup (Just tname) (globEnums vk) of
      Nothing -> genAlias t
      Just e  -> genEnums e
  where
    tname = (name :: VkType -> VkTypeName) t

genEnums :: Monad m => VkEnums -> ModuleWriter m ()
genEnums VkEnums {..} = do
    writePragma "GeneralizedNewtypeDeriving"
    writePragma "DeriveDataTypeable"
    writePragma "DeriveGeneric"

    writeImport "GHC.Generics" $ IAbs () (NoNamespace ()) (Ident () "Generic")
    writeImport "Data.Data"    $ IAbs () (NoNamespace ()) (Ident () "Data")
    writeImport "Data.Bits"    $ IAbs () (NoNamespace ()) (Ident () "Bits")
    writeImport "Data.Bits"    $ IAbs () (NoNamespace ()) (Ident () "FiniteBits")
    writeImport "Foreign.Storable"
      $ IAbs () (NoNamespace ()) (Ident () "Storable")
    when _vkEnumsIsBits $ writeImport "Data.Bits"
      $ IAbs () (NoNamespace ()) (Ident () "Bits")

    forM_ _vkEnumsTypeName $ \tname -> do
      newInt32TypeDec tname derives _vkEnumsComment
      genEnumShow tname allPNs
      genEnumRead tname allPNs

    writeSections enumPattern _vkEnumsMembers
  where
    derives = (if _vkEnumsIsBits then ("Bits":).("FiniteBits":) else id)
              ["Eq","Ord","Bounded","Storable","Enum", "Data", "Generic"]

    allPNs = map (unVkEnumName . _vkEnumName) . items $ _vkEnumsMembers


genEnumShow :: Monad m => VkTypeName -> [Text] -> ModuleWriter m ()
genEnumShow (VkTypeName tname) xs =
    writeDecl $ parseDecl' $ T.unlines
       $  [text|instance Show $tname where|]
       : map (\x -> "  " <>
                  [text|showsPrec _ $x = showString "$x"|]
             ) xs
      ++ ["  " <> [text|showsPrec p ($tname x) = showParen (p >= 11) (showString "$tname " . showsPrec 11 x)|]
         ]


genEnumRead :: Monad m => VkTypeName -> [Text] -> ModuleWriter m ()
genEnumRead (VkTypeName tname) xs = do

    writeImport "Text.Read.Lex" $ IThingWith () (Ident () "Lexeme") [ConName () (Ident () "Ident")]
    writeImport "GHC.Read"      $ IVar () (Ident () "expectP")
    writeImport "GHC.Read"      $ IVar () (Ident () "choose")
    writeImport "Text.Read"     $ IThingAll () (Ident () "Read")
    writeImport "Text.Read"     $ IVar () (Ident () "parens")
    writeImport "Text.ParserCombinators.ReadPrec" $ IVar () (Ident () "prec")
    writeImport "Text.ParserCombinators.ReadPrec" $ IVar () (Ident () "step")
    writeImport "Text.ParserCombinators.ReadPrec" $ IVar () (Symbol () "+++")

    writeDecl $ parseDecl'
       $  [text|
            instance Read $tname where
              readPrec = parens ( choose [
          |]
       <> ( T.unlines . map ("        " <>) $ T.lines $
           T.intercalate ", " ( map (\x -> [text|("$x", pure $x)|]) xs ) <>
           [text|
                                         ] +++
                                  prec 10 (expectP (Ident "$tname") >> ($tname <$> step readPrec))
                                )
           |]
          )

genAlias :: Monad m => VkType -> ModuleWriter m ()
genAlias t@VkTypeComposite{..}
    = error $ "genAlias: did not expect "
           <> show (vkTypeCat t) <> " being a composite type (VkTypeComposite)!"
genAlias VkTypeSimple
    { name = vkTName
    , attributes = VkTypeAttrs
        { comment = txt
        }
    , typeData = VkTypeData
       { reference = [(vkTRef, _)]
       , comment = mtxt2
       -- , code = ccode
       }
    } = do


    writePragma "GeneralizedNewtypeDeriving"
    writePragma "DeriveDataTypeable"
    writePragma "DeriveGeneric"
    writeImport "Data.Coerce"  $ IVar () (Ident () "coerce")
    writeImport "GHC.Generics" $ IAbs () (NoNamespace ()) (Ident () "Generic")
    writeImport "Data.Data"    $ IAbs () (NoNamespace ()) (Ident () "Data")
    writeImport "Data.Bits"    $ IAbs () (NoNamespace ()) (Ident () "Bits")
    writeImport "Data.Bits"    $ IAbs () (NoNamespace ()) (Ident () "FiniteBits")
    writeImport "Data.Ix"      $ IAbs () (NoNamespace ()) (Ident () "Ix")

    writeDecl . setComment rezComment $ parseDecl'
      [text|
        newtype $tnametxt = $tnametxt $treftxt
          deriving ( Eq, Ord, Num, Bounded, Enum, Integral
                   , Ix, Bits, FiniteBits, Storable, Real, Data, Generic)
      |]

    mapM_ writeDecl $ parseDecls
      [text|
        instance Show $tnametxt where
          {-# INLINE show #-}
          show ($tnametxt x) = show x

        instance Read $tnametxt where
          {-# INLINE readsPrec #-}
          readsPrec = coerce (readsPrec :: Int -> ReadS $treftxt)
      |]

    writeExport $ EThingWith () (EWildcard () 0) tname []
  where
    tname = toHaskellName vkTName
    tnametxt = qNameTxt tname
    treftxt = qNameTxt $ toHaskellName vkTRef
    rezComment = rezComment' >>= preComment . T.unpack
    rezComment' = if txt == mempty
                  then mtxt2
                  else case mtxt2 of
                    Nothing   -> Just txt
                    Just txt2 -> appendComLine (Just txt) txt2
genAlias t@VkTypeSimple
    { typeData = td@VkTypeData
       { reference = []
       }
    } = genAlias t
     { typeData = td { reference = [("VkFlags", [])] } }
genAlias t = error $ "genAlias: expected a simple enum type, but got: "
                  <> show t




newInt32TypeDec :: Monad m
                => VkTypeName -> [Text] -> Text -> ModuleWriter m ()
newInt32TypeDec vkTName insts com = do
    regLink <- vkRegistryLink $ unVkTypeName vkTName
    let tinsts = T.intercalate ", " insts
        rezComment = preComment . T.unpack $ com <:> regLink

    writePragma "GeneralizedNewtypeDeriving"
    writeDecl . setComment rezComment $ parseDecl'
      [text|
        newtype $tnametxt = $tnametxt Int32
          deriving ($tinsts)
      |]

    writeExport $ EThingWith () (EWildcard () 0) tname []
  where
    tname = toHaskellName vkTName
    tnametxt = qNameTxt tname


enumPattern :: Monad m => VkEnum -> ModuleWriter m ()
enumPattern VkEnum {..} = do
    writePragma "PatternSynonyms"
    case _vkEnumValue of
      VkEnumReference -> return ()
      VkEnumString s
        | tyval <- "\"" <> s <> "\""
        , patval <- "Ptr \"" <> s <> "\0\"#" -> do
        writePragma "MagicHash"
        writeImport "Foreign.C.String" (IAbs () (NoNamespace ()) (Ident () "CString"))
        writeImport "GHC.Ptr" (IThingAll () (Ident () "Ptr"))
        mapM_ writeDecl
          . insertDeclComment (T.unpack patnametxt) rezComment
          $ parseDecls [text|
              pattern $patnametxt :: CString
              pattern $patnametxt = $patval

              type $patnametxt = $tyval
            |]
        writeExport $ EAbs () (NoNamespace ()) patname
      VkEnumIntegral n tnametxt
        | Just (VkTypeName cname) <- _vkEnumTName
        , patval <- T.pack $
            if n < 0 then "(" ++ show n ++ ")" else show n
         -> do
        writeDecl . setComment rezComment
                  $ parseDecl' [text|pattern $patnametxt :: $tnametxt|]
        writeDecl $ parseDecl' [text|pattern $patnametxt = $cname $patval|]
        | Nothing <- _vkEnumTName
        , patval <- T.pack $ show n
          -> do
            writeImport "Foreign.C.String" (IAbs () (NoNamespace ()) (Ident () "CString"))
            writeImport "GHC.Ptr" (IThingAll () (Ident () "Ptr"))
            mapM_ writeDecl
              . insertDeclComment (T.unpack patnametxt) rezComment
              $ parseDecls [text|
                  pattern $patnametxt :: $tnametxt
                  pattern $patnametxt = $patval
                |]
            -- For positive constants, we can make a type-level value
            when (n >= 0) $ do
              writeDecl $ parseDecl'
                [text|type $patnametxt = $patval|]
              writeExport $ EAbs () (NoNamespace ()) patname
      VkEnumFractional r
        | patval <- T.pack $ show (realToFrac r :: Double) -> do
            writeDecl . setComment rezComment
                      $ parseDecl' [text|pattern $patnametxt :: (Fractional a, Eq a) => a|]
            writeDecl $ parseDecl' [text|pattern $patnametxt =  $patval|]

      VkEnumAlias (VkEnumName aliasname) ->
          writeDecl . setComment rezComment
                    $ parseDecl' [text|pattern $patnametxt = $aliasname|]

    writeExport $ EAbs () (PatternNamespace ()) patname
  where
    patname = toHaskellName _vkEnumName
    patnametxt = qNameTxt patname
    rezComment = preComment $ T.unpack _vkEnumComment
