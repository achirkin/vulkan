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
  , genBitmaskPair
  ) where

import           Control.Monad
import           Data.Maybe
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



genBitmaskPair :: Monad m
               => VkType
                  -- ^ type category bitmask
               -> VkType
                  -- ^ corresponding bits
               -> ModuleWriter m ()
genBitmaskPair tbm te
  = ask >>= \vk -> case Map.lookup (Just bitsName) (globEnums vk) of
      Nothing -> error $ "genBitmaskPair: Could not find a corresponding enum for bits type "
                      ++ show te
      Just VkEnums {..}  -> do
        writePragma "GeneralizedNewtypeDeriving"
        writePragma "DeriveDataTypeable"
        writePragma "DeriveGeneric"
        writePragma "DataKinds"
        writePragma "KindSignatures"
        writePragma "StandaloneDeriving"
        writePragma "TypeSynonymInstances"
        writePragma "PatternSynonyms"
        writePragma "FlexibleInstances"
        writeFullImport "Graphics.Vulkan.Marshal"
        writeImport $ DIThing "Generic" DITNo
        writeImport $ DIThing "Data" DITNo
        writeImport $ DIThing "Storable" DITNo
        writeImport $ DIThing "Bits" DITNo
        writeImport $ DIThing "FiniteBits" DITNo
        writeImport $ DIThing "VkFlags" DITAll

        let allPNs = map (unVkEnumName . _vkEnumName) . items $ _vkEnumsMembers
        mapM_ writeDecl $ parseDecls
          [text|
            newtype $baseNameTxt (a :: FlagType) = $baseNameTxt VkFlags
              deriving ( Eq, Ord, Storable, Data, Generic)

            type $flagNameTxt = $baseNameTxt FlagMask
            type $bitsNameTxt = $baseNameTxt FlagBit

            pattern $bitsNameTxt :: VkFlags -> $baseNameTxt FlagBit
            pattern $bitsNameTxt n = $baseNameTxt n

            pattern $flagNameTxt :: VkFlags -> $baseNameTxt FlagMask
            pattern $flagNameTxt n = $baseNameTxt n

            deriving instance Bits       ($baseNameTxt FlagMask)
            deriving instance FiniteBits ($baseNameTxt FlagMask)
            deriving instance Integral   ($baseNameTxt FlagMask)
            deriving instance Num        ($baseNameTxt FlagMask)
            deriving instance Bounded    ($baseNameTxt FlagMask)
            deriving instance Enum       ($baseNameTxt FlagMask)
            deriving instance Real       ($baseNameTxt FlagMask)

          |]

        genEnumShow (VkTypeName $ "(" <> baseNameTxt <> " a)") baseNameTxt allPNs
        genEnumRead (VkTypeName $ "(" <> baseNameTxt <> " a)") baseNameTxt allPNs

        pats <- mapM ( bitmaskPattern
                       (baseNameTxt <> " a")
                        baseNameTxt
                      )  $ items _vkEnumsMembers
        let patCNames = map (ConName () . Ident () . T.unpack)
                      $ baseNameTxt : flagNameTxt : bitsNameTxt : pats
            ilist = IThingAll () (Ident () (T.unpack baseNameTxt))
            elist = EThingWith () (NoWildcard ())
                      (UnQual () (Ident () $ T.unpack baseNameTxt))
                      patCNames


        writeExport $ DIThing flagNameTxt DITNo
        writeExport $ DIThing bitsNameTxt DITNo
        writeExportExplicit (DIThing flagNameTxt DITAll)
          [ ilist
          , IAbs () (NoNamespace ()) (Ident () (T.unpack flagNameTxt))]
          [ elist ]
        writeExportExplicit (DIThing bitsNameTxt DITAll)
          [ ilist
          , IAbs () (NoNamespace ()) (Ident () (T.unpack bitsNameTxt))]
          [ ]
  where
    flagName = tname tbm
    bitsName = tname te
    baseNameTxt = T.replace "Flags" "Bitmask" flagNameTxt
    flagNameTxt = unVkTypeName flagName
    bitsNameTxt = unVkTypeName bitsName
    tname = (name :: VkType -> VkTypeName)


genApiConstants :: Monad m => ModuleWriter m ()
genApiConstants = do
  glvl <- ModuleWriter $ RWS.gets currentSecLvl
  writeSection glvl "API Constants"
  writeImport $ DIThing "Word32" DITNo
  vk <- ask
  pushSecLvl . const $ mapM_ genEnums
    (Map.lookup Nothing (globEnums vk))


-- | Lookup an enum in vk.xml and generate code for it
genEnum :: Monad m => VkType -> ModuleWriter m ()
genEnum t = ask >>= \vk -> case Map.lookup (Just tname) (globEnums vk) of
      Nothing -> genAlias t
      Just e  -> genEnums e
  where
    tname = (name :: VkType -> VkTypeName) t

genEnums :: Monad m => VkEnums -> ModuleWriter m ()
genEnums VkEnums {..} = do
    writePragma "GeneralizedNewtypeDeriving"
    writePragma "DeriveDataTypeable"
    writePragma "DeriveGeneric"
    writeFullImport "Graphics.Vulkan.Marshal"


    writeImport $ DIThing "Generic" DITEmpty
    writeImport $ DIThing "Data" DITEmpty
    writeImport $ DIThing "Storable" DITEmpty
    when _vkEnumsIsBits $ do
      writeImport $ DIThing "Bits" DITNo
      writeImport $ DIThing "FiniteBits" DITNo

    mtname <- forM _vkEnumsTypeName $ \tname -> do
      regLink <- vkRegistryLink $ unVkTypeName tname
      let tinsts = T.intercalate ", " derives
          rezComment = preComment . T.unpack $ _vkEnumsComment <:> regLink
          tnametxt = unVkTypeName tname

      writeDecl . setComment rezComment $ parseDecl'
        [text|
          newtype $tnametxt = $tnametxt $baseType
            deriving ($tinsts)
        |]
      genEnumShow tname (unVkTypeName tname) allPNs
      genEnumRead tname (unVkTypeName tname) allPNs
      return tnametxt

    epats <- (>>= maybeToList) <$> mapM enumPattern (items _vkEnumsMembers)
    case mtname of
      Nothing ->
        mapM_ (writeExport . DIPat) epats
      Just tnameTxt -> do
        let patCNames = map (ConName () . Ident () . T.unpack)
                      $ tnameTxt : epats
            ilist = IThingAll () (Ident () (T.unpack tnameTxt))
            elist = EThingWith () (NoWildcard ())
                      (UnQual () (Ident () $ T.unpack tnameTxt))
                      patCNames
        writeExportExplicit (DIThing tnameTxt DITAll)
          [ ilist ]
          [ elist ]
  where
    derives = (if _vkEnumsIsBits then ("Bits":).("FiniteBits":) else id)
              ["Eq","Ord","Num","Bounded","Storable","Enum", "Data", "Generic"]
    baseType = if _vkEnumsIsBits then "VkFlags" else "Int32"

    allPNs = map (unVkEnumName . _vkEnumName) . items $ _vkEnumsMembers


genEnumShow :: Monad m => VkTypeName -> Text -> [Text] -> ModuleWriter m ()
genEnumShow (VkTypeName tname) constr xs =
    writeDecl $ parseDecl' $ T.unlines
       $  [text|instance Show $tname where|]
       : map (\x -> "  " <>
                  [text|showsPrec _ $x = showString "$x"|]
             ) xs
      ++ ["  " <> [text|showsPrec p ($constr x) = showParen (p >= 11) (showString "$constr " . showsPrec 11 x)|]
         ]


genEnumRead :: Monad m => VkTypeName -> Text -> [Text] -> ModuleWriter m ()
genEnumRead (VkTypeName tname) constr xs = do

    writeImport $ DIThing "Lexeme" DITAll
    writeImport $ DIThing "Read" DITAll
    writeImport $ DIVar "expectP"
    writeImport $ DIVar "choose"
    writeImport $ DIVar "parens"
    writeImport $ DIVar "prec"
    writeImport $ DIVar "step"
    writeImport $ DIVar "(+++)"

    writeDecl $ parseDecl'
       $  [text|
            instance Read $tname where
              readPrec = parens ( choose [
          |]
       <> ( T.unlines . map ("        " <>) $ T.lines $
           T.intercalate ", " ( map (\x -> [text|("$x", pure $x)|]) xs ) <>
           [text|
                                         ] +++
                                  prec 10 (expectP (Ident "$constr") >> ($constr <$> step readPrec))
                                )
           |]
          )

genAlias :: Monad m => VkType -> ModuleWriter m ()
genAlias VkTypeSimple
    { name = VkTypeName s
    , attributes = VkTypeAttrs
      { requires = Just (VkTypeName refname)
      , comment = txt
      }
    , typeData = VkTypeData
       { reference = [("VkFlags", [])] }
    }
    | tnameDeclared <- DIThing s DITNo
    = do
  indeed <- isIdentDeclared tnameDeclared
  if indeed
  then do
    writeImport tnameDeclared
    writeExport tnameDeclared
  else do
    writeImport $ DIThing refname DITNo
    writeDecl . setComment (preComment $ T.unpack txt) $ parseDecl'
      [text|type $s = $refname|]
    writeExport tnameDeclared
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

    writeImport $ DIVar "coerce"
    writeImport $ DIThing "Generic" DITNo
    writeImport $ DIThing "Data" DITNo
    writeImport $ DIThing "Bits" DITNo
    writeImport $ DIThing "FiniteBits" DITNo
    writeImport $ DIThing "Storable" DITNo
    writeImport $ DIThing treftxt DITNo

    writeDecl . setComment rezComment $ parseDecl'
      [text|
        newtype $tnametxt = $tnametxt $treftxt
          deriving ( Eq, Ord, Num, Bounded, Enum, Integral
                   , Bits, FiniteBits, Storable, Real, Data, Generic)
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

    writeExport $ DIThing tnametxt DITAll
  where
    -- tname = toQName vkTName
    tnametxt = unVkTypeName vkTName
    treftxt = unVkTypeName vkTRef
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





bitmaskPattern :: Monad m => Text -> Text -> VkEnum -> ModuleWriter m Text
bitmaskPattern tnameTxt constrTxt
  VkEnum
  { _vkEnumName = VkEnumName patnameTxt
  , _vkEnumComment = comm
  , _vkEnumValue = VkEnumIntegral n _
  } = do
    writeDecl . setComment rezComment
              $ parseDecl' [text|pattern $patnameTxt :: $tnameTxt|]
    writeDecl $ parseDecl' [text|pattern $patnameTxt = $constrTxt $patVal|]
    return patnameTxt
  where
    patVal = T.pack $ show n
    rezComment = preComment $ T.unpack comm
bitmaskPattern _ _ p = error $ "Unexpected bitmask pattern " ++ show p


enumPattern :: Monad m => VkEnum -> ModuleWriter m (Maybe Text)
enumPattern VkEnum {..} = writePragma "PatternSynonyms" >>
  case _vkEnumValue of
    VkEnumReference -> do
          enames <- lookupDeclared (unVkEnumName _vkEnumName)
          mapM_ writeImport enames
          mapM_ writeExport enames
          return Nothing

    VkEnumString s
      | tyval <- "\"" <> s <> "\""
      , patval <- "Ptr \"" <> s <> "\0\"#"
      , _patnametxt <- "_" <> patnametxt
      , is_patnametxt <- "is_" <> patnametxt
        -> do
          writePragma "MagicHash"
          writePragma "ViewPatterns"
          writeFullImport "Graphics.Vulkan.Marshal"
          writeImport $ DIThing "CString" DITNo
          writeImport $ DIThing "Ptr" DITAll
          mapM_ writeDecl
            . insertDeclComment (T.unpack patnametxt) rezComment
            $ parseDecls [text|
                pattern $patnametxt :: CString
                pattern $patnametxt <- ( $is_patnametxt -> True )
                  where
                    $patnametxt = $_patnametxt

                {-# INLINE $_patnametxt #-}
                $_patnametxt :: CString
                $_patnametxt = $patval

                {-# INLINE $is_patnametxt #-}
                $is_patnametxt :: CString -> Bool
                $is_patnametxt = eqCStrings $_patnametxt

                type $patnametxt = $tyval
              |]
          writeExport $ DIThing patnametxt DITNo
          writeExport $ DIPat patnametxt
          return Nothing

    VkEnumIntegral n tnametxt
      | Just (VkTypeName cname) <- _vkEnumTName
      , patval <- T.pack $
          if n < 0 then "(" ++ show n ++ ")" else show n
        -> do
          writeImport $ DIThing tnametxt DITAll
          writeDecl . setComment rezComment
                    $ parseDecl' [text|pattern $patnametxt :: $tnametxt|]
          writeDecl $ parseDecl' [text|pattern $patnametxt = $cname $patval|]
          return $ Just patnametxt

      | Nothing <- _vkEnumTName
      , patval <- T.pack $ show n
        -> do
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
            writeExport $ DIThing patnametxt DITNo
          writeExport $ DIPat patnametxt
          return Nothing

    VkEnumFractional r
      | patval <- T.pack $ show (realToFrac r :: Double) -> do
          writeDecl . setComment rezComment
                    $ parseDecl' [text|pattern $patnametxt :: (Fractional a, Eq a) => a|]
          writeDecl $ parseDecl' [text|pattern $patnametxt =  $patval|]
          writeExport $ DIPat patnametxt
          return Nothing

    VkEnumAlias (VkEnumName aliasname) -> do
          writeImport $ DIPat aliasname
          writeDecl . setComment rezComment
                    $ parseDecl' [text|pattern $patnametxt = $aliasname|]
          writeExport $ DIPat patnametxt
          return Nothing
  where
    patname = toQName _vkEnumName
    patnametxt = qNameTxt patname
    rezComment = preComment $ T.unpack _vkEnumComment
