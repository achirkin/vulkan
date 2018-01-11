{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write
  (generateVkSource
  , ModuleWriter (..), runModuleWriter
  , ModuleWriting (..), genModule
  ) where

import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Path
import           System.IO                            (writeFile)

-- import           Language.Haskell.Exts.Build
import Data.Foldable (toList)
import           Data.Bits
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (mapAccumL)
import           Control.Monad
import           Control.Monad.Trans.RWS.Strict
import           Control.Monad.Fix
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Applicative
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Exts.Extension
import           Numeric
import           NeatInterpolation
import GHC.Stack (HasCallStack)

import           VkXml.CommonTypes
import           VkXml.Sections
import           VkXml.Sections.Enums                 as Enums



generateVkSource :: Path b Dir
                    -- ^ multiline
                    --     haddock!
                    --
                    --   >>> show 1
                    --
                 -> VkXml ()
                 -> IO ()
generateVkSource outputDir vkXml = do

  let pMode = defaultParseMode
        { baseLanguage = Haskell2010
        , extensions =
          [ EnableExtension GeneralizedNewtypeDeriving
          , EnableExtension PatternSynonyms
          , UnknownExtension "Strict"
          ]
        }
      testS = T.unpack [text|
                pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout
                pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0

                pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout
                pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout (-1)
            |]
  putStrLn testS
  putStrLn "-----------------"
  print $ ((Nothing :: Maybe CodeComment) <$)
       <$> parseModuleWithMode pMode testS
  print $ ((Nothing :: Maybe CodeComment) <$) . fst
       <$> parseModuleWithComments pMode testS

  let mf = Module Nothing
            (Just $ ModuleHead Nothing
                            (ModuleName Nothing "Graphics.Vulkan") Nothing Nothing
             )
             [ LanguagePragma Nothing [Ident Nothing "PatternSynonyms"]
             , LanguagePragma Nothing [Ident Nothing "GeneralizedNewtypeDeriving"]
             , LanguagePragma Nothing [Ident Nothing "Strict"]
             ]
             [ simpleImport
                 ( ModuleName Nothing "Data.Int" )
                 [ IAbs Nothing (NoNamespace Nothing) (Ident Nothing "Int32") ]
             ,  simpleImport
                 ( ModuleName Nothing "Data.Bits" )
                 [ IAbs Nothing (NoNamespace Nothing) (Ident Nothing "Bits") ]
             ,  simpleImport
                 ( ModuleName Nothing "Foreign.Storable" )
                 [ IThingAll Nothing (Ident Nothing "Storable") ]
             ]
  putStrLn "-------------------------------------------------------------------"
  -- putStrLn . prettyPrint $ mf $ genEnums vkXml

  let rez = uncurry exactPrint . ppWithCommentsMode defaultMode . mf $ genEnums vkXml
  -- putStrLn rez

  writeFile (toFilePath $ outputDir </> [relfile|Vulkan.hs|]) rez

genEnums :: VkXml a -> [Decl (Maybe CodeComment)]
genEnums vkXml = allEnums >>= genEnum . unInorder
  where
    -- allTypes = unInorder $ globTypes vkXml
    allEnums = globEnums vkXml
    genEnum vkenum =
        if tname == VkTypeName "API Constants"
        then tPats
        else newInt32TypeDec tname (enumDerives vkenum) tcomment : tPats
      where
        tname = (name :: VkEnums -> VkTypeName) vkenum
        tcomment = (comment :: VkEnums -> Maybe Text) vkenum
        tPats = enumPats tname vkenum

    enumDerives VkEnums {..} = ["Eq","Ord","Storable"]
    enumDerives VkBitmasks {..} = ["Eq","Ord","Bits","Storable"]
    enumDerives VkConstants {..} = ["Eq","Ord","Storable"]

    enumPats tname VkEnums {..}
      = items memberEnums >>= enumPattern tname
    enumPats tname VkBitmasks {..}
      = items memberMasks >>= bitmaskPattern tname
    enumPats _tname VkConstants {..} = []



type A = Maybe CodeComment

simpleImport :: ModuleName A -> [ImportSpec A] -> ImportDecl A
simpleImport mname ispecs = ImportDecl
  { importAnn = Nothing
  , importModule = mname
  , importQualified = False
  , importSrc = False
  , importSafe = False
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs =
      Just (ImportSpecList Nothing False ispecs)
  }



newInt32TypeDec :: VkTypeName -> [Text] -> Maybe Text -> Decl A
newInt32TypeDec (VkTypeName tname) insts com
    = amap (const rezComment) rezDef
  where
    tinsts = T.intercalate ", " insts
    rezDef = parseDecl'
      [text|
        newtype $tname = $tname Int32
          deriving ($tinsts)
      |]
    rezComment = com >>= preComment . T.unpack



bitmaskPattern :: VkTypeName -> VkBitmaskValue -> [Decl A]
bitmaskPattern tname VkBitmaskBitpos {..}
    = enumPattern tname $ VkEnumValue name c v
  where
    v = shiftL 1 (fromIntegral bitpos)
    c = appendComLine comment
      $ "Bit position @" <> T.pack (show bitpos) <> "@"
bitmaskPattern tname VkBitmaskValue {..}
    = enumPattern tname $ VkEnumValue name c value
  where
    c = appendComLine comment
      $ "Bitmask value @0x" <> T.pack (showHex value "@")

enumPattern :: VkTypeName -> VkEnumValue -> [Decl A]
enumPattern (VkTypeName tname) VkEnumValue {..}
    = [ amap (const rezComment) rezSig
      , rezPat
      ]
  where
    patname = unVkEnumValueName name
    patval = if value < 0 then "(" <> T.pack (show value) <> ")"
                          else T.pack (show value)
    rezSig = parseDecl' [text|pattern $patname :: $tname|]
    rezPat = parseDecl' [text|pattern $patname = $tname $patval|]
    rezComment = comment >>= preComment . T.unpack




appendComLine :: Maybe Text -> Text -> Maybe Text
appendComLine Nothing c = Just c
appendComLine (Just s) c
  | "" <- T.strip s = Just c
  | otherwise = Just $ T.stripEnd s <> "\n\n" <> c


parseDecl' :: HasCallStack => Text -> Decl A
parseDecl' t = case parseDeclWithMode vkParseMode (T.unpack t) of
  err@ParseFailed{} -> error $ show err
  ParseOk r -> Nothing <$ r

vkParseMode :: ParseMode
vkParseMode = defaultParseMode
      { baseLanguage = Haskell2010
      , extensions =
        [ EnableExtension GeneralizedNewtypeDeriving
        , EnableExtension PatternSynonyms
        , UnknownExtension "Strict"
        ]
      }


{-
What do we need to write in the writing monad?

 * Type and function imports: Set (ImportSpec A)
 * Type, pattern, constructor, function declarations: Seq (Decl A)
 * Export list with sections etc.: Seq (ExportSpec A)

-}


genModule :: String -> ModuleWriting -> Module A
genModule mname ModuleWriting {..}
    = Module Nothing
        (Just $ ModuleHead Nothing
          (ModuleName Nothing mname) Nothing (Just exports)
        )
        pragmas imports decs
  where
    genPragma s = LanguagePragma Nothing [Ident Nothing s]
    genImport (m, specs)
      = simpleImport (Nothing <$ m) (fmap (const Nothing) <$> toList specs)
    pragmas = genPragma <$> toList mPragmas
    imports = genImport <$> Map.toList mImports
    decs = toList mDecs
    exports = ExportSpecList Nothing $ toList mExports

data ModuleWriting
  = ModuleWriting
  { mImports   :: Map (ModuleName ()) (Set (ImportSpec ()))
  , mPragmas   :: Set String
  , mDecs      :: Seq (Decl A)
  , mExports   :: Seq (ExportSpec A)
  , pendingSec :: Maybe String
  }


newtype ModuleWriter m a
  = ModuleWriter
  { unModuleWriter :: RWST (VkXml ()) () ModuleWriting m a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadFail, MonadIO
             , Alternative, MonadPlus, MonadReader (VkXml ()))

runModuleWriter :: Functor m
                => VkXml () -> ModuleWriter m a -> m (a, ModuleWriting)
runModuleWriter vkxml mw
    = f <$> runRWST (unModuleWriter mw) vkxml
                    (ModuleWriting mempty mempty mempty mempty mempty)
  where
    f (a,s,_) = (a, s)


-- | Add a symbol to module imports
writeImport :: Monad m
            => String
            -> ImportSpec ()
            -> ModuleWriter m ()
writeImport mname is = ModuleWriter . modify $
    \mr -> mr {mImports = Map.alter f (ModuleName () mname) $ mImports mr}
  where
    f Nothing = Just $ Set.singleton is
    f (Just iss) = case mapAccumL check (Just is) $ toList iss of
        (Nothing, iss') -> Just $ Set.fromList iss'
        (Just is', iss') -> Just $ Set.fromList (is':iss')
    check Nothing b                                   = (Nothing, b)
    check (Just a@IVar{}) b@IVar{} | a == b           = (Nothing, b)
    check (Just a@IAbs{}) b@IAbs{} | a == b           = (Nothing, b)
    check (Just a@IThingAll{}) b@IThingAll{} | a == b = (Nothing, b)
    check (Just (IThingWith _ an as)) (IThingWith _ bn bs)
      | an == bn
      = (Nothing, IThingWith () an $ List.union as bs)
    check (Just (IThingWith _ an _)) (IThingAll _ bn)
      | an == bn
      = (Nothing, IThingAll () bn)
    check ma b = (ma, b)

-- | Add a declaration to a module
writeDecl :: Monad m => Decl A -> ModuleWriter m ()
writeDecl d = ModuleWriter . modify $
  \mr -> mr {mDecs = mDecs mr Seq.|> d}

-- | Add a pragma to a module head
writePragma :: Monad m => String -> ModuleWriter m ()
writePragma pname = ModuleWriter . modify $
  \mr -> mr {mPragmas = Set.insert pname $ mPragmas mr}

-- | Add an export declaration to a module export list
writeExport :: Monad m => ExportSpec () -> ModuleWriter m ()
writeExport espec = ModuleWriter . modify $
   \mr -> mr { mExports = mExports mr Seq.|> ((f <$> pendingSec mr) <$ espec)
             , pendingSec = Nothing
             }
  where
    f = CodeComment AboveCode ' '

-- | Add a section split to a module export list
writeSection :: Monad m
             => Int    -- ^ Section level - number of star symbols
                       --   (zero for simple text)
             -> Text   -- ^ Section name
                       -- (And section content on further lines)
             -> ModuleWriter m ()
writeSection lvl txt
    | [] <- ss = pure ()
    | s1:srest <- ss
    , s <- (indent ++ s1) : srest
    = ModuleWriter . modify $ \mr -> mr { pendingSec = f (pendingSec mr) s}
  where
    f Nothing  s = Just $ unlines s
    f (Just t) s = Just . unlines $ lines t ++ "":s
    indent = if lvl <= 0 then "" else reverse $ ' ' : replicate lvl '*'
    ss = lines . T.unpack $ T.strip txt
