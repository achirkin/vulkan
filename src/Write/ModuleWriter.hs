{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
module Write.ModuleWriter
  ( A, ModuleWriting (..), ModuleWriter (..)
  , runModuleWriter, genModule
  , writeImport, writeExport
  , writePragma, writeDecl
  , writeSection
    -- * Helpers
  , appendComLine, parseDecl', setComment
  , writeWithComments, vkRegistryLink
  , writeSections
  ) where

import           Data.Semigroup
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.RWS.Strict       (RWST (..), modify, gets)
import           Data.Foldable                        (toList)
import qualified Data.List                            as List
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Sequence                        (Seq)
import qualified Data.Sequence                        as Seq
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Traversable                     (mapAccumL)
import           GHC.Stack                            (HasCallStack)
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax

import           VkXml.Sections
import           VkXml.CommonTypes
import qualified VkXml.Sections.Feature               as Feature


type A = Maybe CodeComment


data ModuleWriting
  = ModuleWriting
  { mImports      :: Map (ModuleName ()) (Set (ImportSpec ()))
  , mPragmas      :: Set String
  , mDecs         :: Seq (Decl A)
  , mExports      :: Seq (ExportSpec A)
  , pendingSec    :: Maybe String
  , currentSecLvl :: Int
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
                    (ModuleWriting mempty mempty mempty mempty mempty 1)
  where
    f (a,s,_) = (a, s)


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
        (Nothing, iss')  -> Just $ Set.fromList iss'
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
   \mr -> mr { mExports = mExports mr Seq.|>
                          setComment (f <$> pendingSec mr) (Nothing <$ espec)
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


-- | Write section elements interspersed with comments as section delimers.
--   Determine subsection level automatically.
writeSections :: Monad m
              => (a -> ModuleWriter m ())
              -> Sections a
              -> ModuleWriter m ()
writeSections f secs = do
    slvl <- ModuleWriter $ gets currentSecLvl
    let g = ModuleWriter . modify
          $ \mr -> mr { currentSecLvl = slvl + 1}
    writeWithComments slvl (\a -> g >> f a) secs

-- | Write section elements interspersed with comments as section delimers
writeWithComments :: Monad m
                  => Int
                  -> (a -> ModuleWriter m ())
                  -> Sections a
                  -> ModuleWriter m ()
writeWithComments slvl f Sections {..} = go 0 items comments
  where
    go _ [] cs = writeSection 0 . T.unlines $ map snd cs
    go i (a:as) [] = f a >> go i as []
    go i (a:as) (c@(j, txt):cs)
      | i >= j = writeSection slvl txt >> go i (a:as) cs
      | otherwise = f a >> go (i+1) as (c:cs)



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



appendComLine :: Maybe Text -> Text -> Maybe Text
appendComLine Nothing c = Just c
appendComLine (Just s) c
  | "" <- T.strip s = Just c
  | otherwise = Just $ T.stripEnd s <> "\n\n" <> c


parseDecl' :: HasCallStack => Text -> Decl A
parseDecl' t = case parseDeclWithMode vkParseMode (T.unpack t) of
  err@ParseFailed{} -> error $ show err
  ParseOk r         -> Nothing <$ r


setComment :: Annotated m
           => Maybe CodeComment
           -> m A -> m A
setComment mcc = amap (const mcc)



vkParseMode :: ParseMode
vkParseMode = defaultParseMode
      { baseLanguage = Haskell2010
      , extensions =
        [ EnableExtension GeneralizedNewtypeDeriving
        , EnableExtension PatternSynonyms
        , UnknownExtension "Strict"
        ]
      }

-- | Get a link to current definition in vulkan registry
vkRegistryLink :: Monad m
               => Text -- ^ name of the thing (part of the link)
                       --     e.g. type name.
               -> ModuleWriter m Text
vkRegistryLink tname = do
    vkXml <- ask
    pure $ "<https://www.khronos.org/registry/vulkan/specs/"
        <> Feature.number (unInorder $ globFeature vkXml)
        <> "/man/html/" <> tname <> ".html "
        <> tname <> " registry at www.khronos.org>"
