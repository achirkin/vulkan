{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE UndecidableInstances       #-}
module Write.ModuleWriter
  ( A, DeclaredNames, ModuleWriting (..), ModuleWriter (..)
  , DeclaredIdent (..), DIThingMembers (..)
  , runModuleWriter, genModule
  , lookupDiModule, lookupDiModuleImports
  , isIdentDeclared, lookupDeclared, getCurrentModuleName
  , writeImport, writeFullImport, writeExport, writeExportExplicit
  , writeExportSpec
  , writePragma, writeOptionsPragma, writeDecl
  , writeSection --, writeSectionPre
    -- * Helpers
  , appendComLine, parseDecl', setComment
  , parseDecls, insertDeclComment
  , writeWithComments, vkRegistryLink
  , writeSections
  , foldSectionsWithComments, pushSecLvl, getCurrentSecLvl, i2espec
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.RWS.Strict       (RWST (..), gets, modify)
import           Data.Coerce
import           Data.Foldable                        (toList)
import qualified Data.List                            as List
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (isJust, maybeToList)
import           Data.Semigroup
import           Data.Sequence                        (Seq)
import qualified Data.Sequence                        as Seq
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Traversable                     (mapAccumL)
import           GHC.Generics                         (Generic)
import           GHC.Stack                            (HasCallStack)
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax

import           Write.Util.DeclaredNames
import           Write.Util.NFData                    ()

import           VkXml.CommonTypes
import           VkXml.Sections
import qualified VkXml.Sections.Feature               as Feature



type A = Maybe CodeComment


data ModuleWriting
  = ModuleWriting
  { mName         :: ModuleName ()
  , globalNames   :: DeclaredNames
  , mImports      :: Map (ModuleName ()) (Set (ImportSpec ()))
  , mFullImports  :: Set (ModuleName ())
  , mPragmas      :: Set String
  , mOptPragmas   :: Set (Maybe Tool, String)
  , mDecs         :: Seq (Decl A)
  , mExports      :: Seq (ExportSpec A)
  , pendingSec    :: Seq (Int, Text)
  , currentSecLvl :: Int
  } deriving (Generic)

instance NFData ModuleWriting

newtype ModuleWriter m a
  = ModuleWriter
  { unModuleWriter :: RWST VkXml () ModuleWriting m a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadFail, MonadIO
             , Alternative, MonadPlus, MonadReader VkXml
             , MFunctor, MonadTrans)


runModuleWriter :: Functor m
                => VkXml
                -> String -- ^ module name
                -> DeclaredNames
                -> ModuleWriter m a -> m (a, ModuleWriting)
runModuleWriter vkxml mname gNames mw
    = f <$> runRWST (unModuleWriter mw) vkxml
                    (ModuleWriting (ModuleName () mname) gNames mempty
                                   mempty mempty mempty mempty mempty mempty 1)
  where
    f (a,s,_) = (a, s)


genModule :: ModuleWriting -> Module A
genModule ModuleWriting {..}
    = Module Nothing
        (Just $ ModuleHead Nothing (Nothing <$ mName) Nothing (Just exports))
        pragmas imports decs
  where
    genPragma s = LanguagePragma Nothing [Ident Nothing s]
    genOptPr (mt,s) = OptionsPragma Nothing mt s
    genImport (m, specs)
      | not $ m `Set.member` mFullImports
      = [simpleImport (Nothing <$ m) (fmap (const Nothing) <$> toList specs)]
      | otherwise = []
    genFullImport m
      = ImportDecl Nothing (Nothing <$ m) False False False Nothing Nothing Nothing
    pragmas = (genOptPr  <$> toList mOptPragmas)
           ++ (genPragma <$> toList mPragmas)
    imports = (Map.toList mImports >>= genImport)
           ++ (genFullImport <$> Set.toList mFullImports)
    decs = toList mDecs
    exports = ExportSpecList Nothing $ toList mExports


lookupDiModule :: Monad m => DeclaredIdent -> ModuleWriter m (Maybe (ModuleName ()))
lookupDiModule di = ModuleWriter $ gets (fmap fst . Map.lookup di . globalNames)

lookupDiModuleImports :: Monad m => DeclaredIdent
                     -> ModuleWriter m (Maybe (ModuleName (), [ImportSpec ()]))
lookupDiModuleImports di = ModuleWriter $ gets (Map.lookup di . globalNames)


isIdentDeclared :: Monad m => DeclaredIdent -> ModuleWriter m Bool
isIdentDeclared di = ModuleWriter $ gets $ \mr ->
  let mm = fmap ((mName mr ==) . fst) . Map.lookup di $ globalNames mr
  in Just False == mm


lookupDeclared :: (Monad m, Coercible a Text)
               => a -> ModuleWriter m [DeclaredIdent]
lookupDeclared a = do
    gn <- ModuleWriter $ gets globalNames
    let mb2l x = [ x | isJust (Map.lookup x gn) ]
        mb2m x = x <$ Map.lookup x gn
    return $
         mb2l (DIVar n)
      <> mb2l (DIPat n)
      <> maybeToList ( mb2m (DIThing n DITAll)
                   <|> mb2m (DIThing n DITEmpty)
                   <|> mb2m (DIThing n DITNo)
                      )
  where
    n = coerce a

-- | Add a symbol to module imports
writeImport :: Monad m
            => DeclaredIdent
            -> ModuleWriter m ()
writeImport di = do
    mmname <- fmap ((,) di) <$> lookupDiModuleImports di
    mmnameNo <- fmap ((,) diNo) <$> lookupDiModuleImports diNo
    mapM_  (uncurry writeImport') (mmname <|> mmnameNo)
  where
    diNo = case di of
      DIThing n _ -> DIThing n DITNo
      _           -> di

writeImport' :: Monad m
            => DeclaredIdent
            -> (ModuleName (), [ImportSpec ()])
            -> ModuleWriter m ()
writeImport' di (m,[]) = writeImport'' m (diToImportSpec di)
writeImport' _ (m,xs)  = mapM_ (writeImport'' m) xs

writeImport'' :: Monad m
              => ModuleName ()
              -> ImportSpec ()
              -> ModuleWriter m ()
writeImport'' mname is = ModuleWriter . modify $
      \mr -> mr {mImports = if mName mr /= mname
                            then Map.alter f mname $ mImports mr
                            else mImports mr
                }
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

-- | Add a symbol to module imports
writeFullImport :: Monad m
                => String
                -> ModuleWriter m ()
writeFullImport mname = ModuleWriter . modify $
    \mr -> mr {mFullImports = Set.insert (ModuleName () mname) $ mFullImports mr}


-- | Add a declaration to a module
writeDecl :: Monad m => Decl A -> ModuleWriter m ()
writeDecl d = case rnf d of
    () -> ModuleWriter . modify $ \mr -> mr {mDecs = mDecs mr Seq.|> d}

-- | Add a pragma to a module head
writePragma :: Monad m => String -> ModuleWriter m ()
writePragma pname = ModuleWriter . modify $
  \mr -> mr {mPragmas = Set.insert pname $ mPragmas mr}


writeExportExplicit :: Monad m
                    => DeclaredIdent
                    -> [ImportSpec ()]
                    -> [ExportSpec ()]
                    -> ModuleWriter m ()
writeExportExplicit di is es = do
  mapM_ writeExportSpec es
  ModuleWriter . modify $
   \mr -> mr { globalNames = Map.insert di (mName mr, is) (globalNames mr) }


writeOptionsPragma :: Monad m => Maybe Tool -> String -> ModuleWriter m ()
writeOptionsPragma mt pname = ModuleWriter . modify $
  \mr -> mr {mOptPragmas = Set.insert (mt, pname) $ mOptPragmas mr}

-- | Add an export declaration to a module export list
writeExport :: Monad m => DeclaredIdent -> ModuleWriter m ()
writeExport di = do
  writeExportSpec (diToExportSpec di)
  ModuleWriter . modify $
   \mr -> mr
     { globalNames =
       foldr (\n m -> case Map.lookup n m of
                Just _  -> m
                Nothing -> Map.insert n (mName mr, [diToImportSpec n]) m
             ) (globalNames mr) dis
      }
  where
    dis = case di of
      (DIThing n DITAll)   -> [DIThing n DITAll, DIThing n DITEmpty, DIThing n DITNo]
      (DIThing n DITEmpty) -> [DIThing n DITEmpty, DIThing n DITNo]
      (DIThing n DITNo)    -> [DIThing n DITNo]
      _ -> [di]

-- | Add an export declaration to a module export list
writeExportSpec :: Monad m => ExportSpec () -> ModuleWriter m ()
writeExportSpec espec = ModuleWriter . modify $
   \mr -> mr { mExports = mExports mr Seq.|>
                          setComment (f $ pendingSec mr) (Nothing <$ espec)
             , pendingSec = mempty
             }
  where
    -- the whole thing below is to compile comments
    f txts = case removeLastNewline . unlines . g $ toList txts >>= normalize of
       "" -> Nothing
       s  -> Just $ CodeComment AboveCode ' ' s
    -- take a list of one-liners as arguments
    g []                = []
    g ((i, s):(0,t):xs) | i > 0 = (indent i ++ s):"|":t:g xs
    g ((i,s):xs)        = (indent i ++ s) : g xs
    indent lvl = if lvl <= 0 then "" else reverse $ ' ' : replicate lvl '*'
    removeLastNewline []     = []
    removeLastNewline ['\n'] = []
    removeLastNewline (x:xs) = x : removeLastNewline xs
    normalize (lvl, txt) = lastWithNewline . setlvls . lines . T.unpack $ T.strip txt
      where
        setlvls []     = []
        setlvls (x:xs) = (lvl, x) : map ((,) 0) xs
        lastWithNewline []      = []
        lastWithNewline [(i,s)] = [(i,s ++ "\n")]
        lastWithNewline (x:xs)  = x : lastWithNewline xs



-- | Add a section split to a module export list
writeSection :: Monad m
             => Int    -- ^ Section level - number of star symbols
                       --   (zero for simple text)
             -> Text   -- ^ Section name
                       -- (And section content on further lines)
             -> ModuleWriter m ()
writeSection lvl txt = ModuleWriter . modify $
  \mr -> mr { pendingSec = pendingSec mr Seq.|> (lvl, txt)}


-- | Write section elements interspersed with comments as section delimers.
--   Determine subsection level automatically.
writeSections :: Monad m
              => (a -> ModuleWriter m ())
              -> Sections a
              -> ModuleWriter m ()
writeSections f secs = do
    slvl <- ModuleWriter $ gets currentSecLvl
    writeWithComments slvl (pushSecLvl . const . f) secs


-- | Go over element in a sections list,
--   provide a list of comments preceding each element.
foldSectionsWithComments :: Monad m
                         => ([Text] -> a -> ModuleWriter m ())
                            -- ^ What to do with element and comments before it.
                         -> ([Text] -> ModuleWriter m ())
                            -- ^ What to do with last comments.
                         -> Sections a
                         -> ModuleWriter m ()
foldSectionsWithComments f g Sections {..} = go 0 items comments
  where
    go _ [] cs = g $ map snd cs
    go i (a:as) cs
      = let (curCS, remCs) = first (map snd) $ span ((i>=) . fst) cs
        in  f curCS a >> go (i+1) as remCs

-- | Increase section level by 1, run an action with that level,
--   and decrease section level back to original state.
pushSecLvl :: Monad m
           => (Int -> ModuleWriter m a)
           -> ModuleWriter m a
pushSecLvl f = do
  slvl <- ModuleWriter $ gets currentSecLvl
  ModuleWriter . modify $ \mr -> mr { currentSecLvl = slvl + 1}
  r <- f (slvl + 1)
  ModuleWriter . modify $ \mr -> mr { currentSecLvl = slvl}
  return r

getCurrentSecLvl :: Monad m => ModuleWriter m Int
getCurrentSecLvl = ModuleWriter $ gets currentSecLvl

getCurrentModuleName :: Monad m => ModuleWriter m (ModuleName ())
getCurrentModuleName = ModuleWriter $ gets mName


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

parseDecls :: HasCallStack => Text -> [Decl A]
parseDecls t = case parseModuleWithMode vkParseMode (T.unpack t) of
    err@ParseFailed{} -> error $ show err
    ParseOk (Module _ _ _ _ r) -> fmap (Nothing <$) r
    ParseOk _ -> error "declarations should be a part of a simple module."

setComment :: Annotated m
           => Maybe CodeComment
           -> m A -> m A
setComment mcc = amap (const mcc)

-- | Insert comment into a declaration with a given name.
insertDeclComment :: String
                  -> Maybe CodeComment
                  -> [Decl A]
                  -> [Decl A]
insertDeclComment _ Nothing xs = xs
insertDeclComment _ _ [] = []
insertDeclComment s c (x:xs)
  | TypeDecl _ h _ <- x, matchDHead h
    = setComment c x : xs
  | TypeFamDecl _ h _ _ <- x, matchDHead h
    = setComment c x : xs
  | ClosedTypeFamDecl _ h _ _ _ <- x, matchDHead h
    = setComment c x : xs
  | DataDecl _ _ _ h _ _ <- x, matchDHead h
    = setComment c x : xs
  | GDataDecl _ _ _ h _ _ _ <- x, matchDHead h
    = setComment c x : xs
  | DataFamDecl _ _ h _ <- x, matchDHead h
    = setComment c x : xs
  --  TypeInsDecl l (Type l) (Type l) <- x, matchDHead h
  --   = setComment c x : xs
  --  DataInsDecl l (DataOrNew l) (Type l) [QualConDecl l] (Maybe (Deriving l)) <- x, matchDHead h
  --   = setComment c x : xs
  --  GDataInsDecl l (DataOrNew l) (Type l) (Maybe (Kind l)) [GadtDecl l] (Maybe (Deriving l)) <- x, matchDHead h
  --   = setComment c x : xs
  | ClassDecl _ _ h _ _ <- x, matchDHead h
    = setComment c x : xs
   -- InstDecl _ _ (IRule _ _ _ dh) _ <- x, matchDHead h
   --  = setComment c x : xs
  --  DerivDecl l (Maybe (Overlap l)) (InstRule l) <- x, matchDHead h
  --   = setComment c x : xs
  --  InfixDecl l (Assoc l) (Maybe Int) [Op l] <- x, matchDHead h
  --   = setComment c x : xs
  --  DefaultDecl l [Type l] <- x, matchDHead h
  --   = setComment c x : xs
  --  SpliceDecl l (Exp l) <- x, matchDHead h
  --   = setComment c x : xs
  | TypeSig _ (n:_) _ <- x, matchName n
    = setComment c x : xs
  | PatSynSig _ (n:_) _ _ _ _ <- x, matchName n
    = setComment c x : xs
  --  FunBind l [Match l] <- x, matchDHead h
  --   = setComment c x : xs
  --  PatBind l (Pat l) (Rhs l) (Maybe (Binds l)) <- x, matchDHead h
  --   = setComment c x : xs
  --  PatSyn l (Pat l) (Pat l) (PatternSynDirection l) <- x, matchDHead h
  --   = setComment c x : xs
  | ForImp _ _ _ _ n _ <- x, matchName n
    = setComment c x : xs
  | ForExp _ _ _ n _ <- x, matchName n
    = setComment c x : xs
  --  RulePragmaDecl l [Rule l] <- x, matchDHead h
  --   = setComment c x : xs
  --  DeprPragmaDecl l [([Name l], String)] <- x, matchDHead h
  --   = setComment c x : xs
  --  WarnPragmaDecl l [([Name l], String)] <- x, matchDHead h
  --   = setComment c x : xs
  --  InlineSig l Bool (Maybe (Activation l)) (QName l) <- x, matchDHead h
  --   = setComment c x : xs
  --  InlineConlikeSig l (Maybe (Activation l)) (QName l) <- x, matchDHead h
  --   = setComment c x : xs
  --  SpecSig l (Maybe (Activation l)) (QName l) [Type l] <- x, matchDHead h
  --   = setComment c x : xs
  --  SpecInlineSig l Bool (Maybe (Activation l)) (QName l) [Type l] <- x, matchDHead h
  --   = setComment c x : xs
  --  InstSig l (InstRule l) <- x, matchDHead h
  --   = setComment c x : xs
  --  AnnPragma l (Annotation l) <- x, matchDHead h
  --   = setComment c x : xs
  --  MinimalPragma l (Maybe (BooleanFormula l)) <- x, matchDHead h
  --   = setComment c x : xs
  --  RoleAnnotDecl l (QName l) [Role l] <- x, matchDHead h
  --   = setComment c x : xs
  | otherwise
    = x : insertDeclComment s c xs
  where
    matchName (Ident _ t)  = t == s
    matchName (Symbol _ t) = t == s
    matchDHead (DHead _ t)     = matchName t
    matchDHead (DHInfix _ _ t) = matchName t
    matchDHead (DHParen _ t)   = matchDHead t
    matchDHead (DHApp _ t _)   = matchDHead t

vkParseMode :: ParseMode
vkParseMode = defaultParseMode
      { baseLanguage = Haskell2010
      , extensions =
        [ EnableExtension GeneralizedNewtypeDeriving
        , EnableExtension PatternSynonyms
        , EnableExtension EmptyDataDecls
        , EnableExtension ViewPatterns
        , EnableExtension RoleAnnotations
        , EnableExtension MagicHash
        , EnableExtension UnboxedTuples
        , EnableExtension DataKinds
        , EnableExtension TypeOperators
        , EnableExtension FlexibleContexts
        , EnableExtension FlexibleInstances
        , EnableExtension TypeFamilies
        , EnableExtension TypeApplications
        , EnableExtension UnliftedFFITypes
        , EnableExtension UndecidableInstances
        , EnableExtension MultiParamTypeClasses
        , EnableExtension StandaloneDeriving
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
        <> Feature.number (last $ globFeature vkXml)
        <> "-extensions/html/vkspec.html#" <> tname <> " "
        <> tname <> " registry at www.khronos.org>"



i2espec :: ImportSpec a -> ExportSpec a
i2espec (IVar a n)          = EVar a (UnQual a n)
i2espec (IAbs a n m)        = EAbs a n (UnQual a m)
i2espec (IThingAll a n)     = EThingWith a (EWildcard a 0) (UnQual a n) []
i2espec (IThingWith a n cn) = EThingWith a (NoWildcard a) (UnQual a n) cn
