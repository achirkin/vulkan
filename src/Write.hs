
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE Strict                #-}
module Write
  (generateVkSource
  ) where

import           Data.Semigroup
import qualified Data.Text                            as T
import           Path
import           System.IO                            (writeFile)

-- import           Language.Haskell.Exts.Build
import           Data.Bits
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SimpleComments
import           Language.Haskell.Exts.Syntax
import           Numeric
import           Text.Shakespeare.Text

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

  let f1 = [st|
module F1 where
-- | Our Good func 1
func1 :: String -> String
func1 = ("asgsd" ++)

-- | This is a hello data!
--   Two-line comment
data Hello
  = HelloA -- ^ hello A
  | HelloB -- ^ hello B
  deriving (Eq, Show)
           |]
      f2 = [st|
module FModule where

-- | Our Good func 2
func2 :: Int -> String
func2 = show -- we show it here!
           |]
      f1s = T.unpack f1
      f2s = T.unpack f2
      f1parsed = case parseModuleWithComments defaultParseMode f1s of
        failed@ParseFailed {} -> error $ show failed
        ParseOk ( Module l _ _ _ decs
                , comments
                ) -> (l, decs, comments)
        _ -> undefined
      f2parsed = case parseModuleWithComments defaultParseMode f2s of
        failed@ParseFailed {} -> error $ show failed
        ParseOk ( Module l _ _ _ decs
                , comments
                ) -> (l, decs, comments)
        _ -> undefined
  -- putStrLn f1s
  -- putStrLn f2s
  putStrLn "-----------------+++"
  print f1parsed
  putStrLn "-----------------+++"
  print f2parsed
  putStrLn "-----------------+++"


  let m = Module Nothing
         (Just $ ModuleHead Nothing
                            (ModuleName Nothing "GoodModule") Nothing Nothing
         ) [] []
          [ TypeSig Nothing [Ident Nothing "func1"] (TyFun Nothing (TyCon Nothing (UnQual Nothing (Ident Nothing "String"))) (TyCon Nothing (UnQual Nothing (Ident Nothing "String"))))
          , PatBind Nothing (PVar Nothing (Ident Nothing "func1")) (UnGuardedRhs Nothing (LeftSection Nothing (Lit Nothing (String Nothing "asgsd" "asgsd")) (QVarOp Nothing (UnQual Nothing (Symbol Nothing "++"))))) Nothing
          , DataDecl (Just "This is a data declaration") (DataType Nothing) Nothing (DHead Nothing (Ident Nothing "Hello")) [QualConDecl Nothing Nothing Nothing (ConDecl Nothing (Ident Nothing "HelloA") [])
          , QualConDecl Nothing Nothing Nothing (ConDecl Nothing (Ident Nothing "HelloB") [])] (Just (Deriving Nothing [IRule Nothing Nothing Nothing (IHCon Nothing (UnQual Nothing (Ident Nothing "Eq")))
          , IRule Nothing Nothing Nothing (IHCon Nothing (UnQual Nothing (Ident Nothing "Show")))]))
          , TypeSig (Just "My first comment\naslo multiline!") [Ident Nothing "func2"] (TyFun Nothing (TyCon Nothing (UnQual Nothing (Ident Nothing "Int"))) (TyCon Nothing (UnQual Nothing (Ident Nothing "String"))))
          , PatBind Nothing (PVar Nothing (Ident Nothing "func2")) (UnGuardedRhs Nothing (Var Nothing (UnQual Nothing (Ident Nothing "show")))) Nothing
          ]

  putStrLn "-----------------"
  putStrLn $ prettyPrint m
  putStrLn "-----------------"
  putStrLn $ uncurry exactPrint . ppWithComments $ m
  putStrLn "-------------------------------------------------------------------"
  let testS = T.unpack [st|
newtype VkImageLayout = VkImageLayout Int32
                      deriving (Eq, Ord, Storable)
-- | Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
pattern VK_IMAGE_LAYOUT_UNDEFINED :: VkImageLayout
pattern VK_IMAGE_LAYOUT_UNDEFINED = VkImageLayout 0
-- | General layout when image can be used for any kind of access
pattern VK_IMAGE_LAYOUT_GENERAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_GENERAL = VkImageLayout 1
            |]
  putStrLn testS
  putStrLn "-----------------"
  print $ ((Nothing :: Maybe CodeComment) <$) . fst <$> parseModuleWithComments defaultParseMode testS
  putStrLn $ prettyPrint $ simpleNewtypeDecl "VkStructureType" "Int32" . pure $ deriveInstances ["Eq","Show","Ord","Storable"]
  let mf = Module Nothing
            (Just $ ModuleHead Nothing
                            (ModuleName Nothing "Graphics.Vulkan") Nothing Nothing
             )
             [ LanguagePragma Nothing [Ident Nothing "PatternSynonyms"]
             , LanguagePragma Nothing [Ident Nothing "GeneralizedNewtypeDeriving"]
             , LanguagePragma Nothing [Ident Nothing "Strict"]
             ]
             [ ImportDecl
               { importAnn = Nothing
               , importModule = ModuleName Nothing "Data.Int"
               , importQualified = False
               , importSrc = False
               , importSafe = False
               , importPkg = Nothing
               , importAs = Nothing
               , importSpecs =
                   Just (ImportSpecList Nothing False
                     [IAbs Nothing (NoNamespace Nothing) (Ident Nothing "Int32")
                     ])
               }
              , ImportDecl
                { importAnn = Nothing
                , importModule = ModuleName Nothing "Data.Bits"
                , importQualified = False
                , importSrc = False
                , importSafe = False
                , importPkg = Nothing
                , importAs = Nothing
                , importSpecs =
                    Just (ImportSpecList Nothing False
                      [IAbs Nothing (NoNamespace Nothing) (Ident Nothing "Bits")
                      ])
                }
              , ImportDecl
                { importAnn = Nothing
                , importModule = ModuleName Nothing "Foreign.Storable"
                , importQualified = False
                , importSrc = False
                , importSafe = False
                , importPkg = Nothing
                , importAs = Nothing
                , importSpecs =
                    Just (ImportSpecList Nothing False
                      [IThingAll Nothing
                        (Ident Nothing "Storable")
                      ])
                }
             ]
  putStrLn "-------------------------------------------------------------------"
  -- putStrLn . prettyPrint $ mf $ genEnums vkXml

  let rez = uncurry exactPrint . ppWithComments . mf $ genEnums vkXml
  -- putStrLn rez

  writeFile (toFilePath $ outputDir </> [relfile|Vulkan.hs|]) rez

genEnums :: VkXml a -> [Decl (Maybe CodeComment)]
genEnums vkXml = allEnums >>= genEnum . unInorder
  where
    -- allTypes = unInorder $ globTypes vkXml
    allEnums = globEnums vkXml
    genEnum vkenum =
      if tName == "API Constants"
      then tPatBinding
      else
         amap (const tComment)
          ( simpleNewtypeDecl tName "Int32" $ enumDerives vkenum )
          : tPatBinding
       where
         tName = enumName vkenum
         tConstr = simpleConstr tName
         tComment = enumComment vkenum
         tPats = enumPats tConstr vkenum
         tPatBinding = case tPats of
           [] -> []
           xs -> [FunBind Nothing xs]
    enumName VkEnums {..}     = T.unpack $ unVkTypeName name
    enumName VkBitmasks {..}  = T.unpack $ unVkTypeName name
    enumName VkConstants {..} = T.unpack $ unVkTypeName name

    enumComment VkEnums {..}     = comment >>= preComment . T.unpack
    enumComment VkBitmasks {..}  = comment >>= preComment . T.unpack
    enumComment VkConstants {..} = comment >>= preComment . T.unpack

    enumDerives VkEnums {..} = Just
        $ deriveInstances ["Eq","Ord","Storable"]
    enumDerives VkBitmasks {..} = Just
        $ deriveInstances ["Eq","Ord","Bits","Storable"]
    enumDerives VkConstants {..} = Just
        $ deriveInstances ["Eq","Ord","Storable"]

    enumPats tConstr VkEnums {..}
      = map (genEnumPat tConstr) $ items memberEnums
    enumPats tConstr VkBitmasks {..}
      = map (genBitMaskPat tConstr) $ items memberMasks
    enumPats _tConstr VkConstants {..} = []

    genEnumPat tConstr VkEnumValue {..}
      = amap (const $ comment >>= preComment . T.unpack)
      $ enumPattern tConstr (T.unpack $ unVkEnumValueName name) value

    genBitMaskPat tConstr VkBitmaskBitpos {..}
        = genEnumPat tConstr $ VkEnumValue name c v
      where
        v = shiftL 1 (fromIntegral bitpos)
        c = appendComLine comment
          $ "Bit position " <> T.pack (show bitpos) <> "."
    genBitMaskPat tConstr VkBitmaskValue {..}
        = genEnumPat tConstr $ VkEnumValue name c value
      where
        c = appendComLine comment
          $ "Bitmask value " <> T.pack (showHex value ".")

    appendComLine Nothing c = Just c
    appendComLine (Just s) c
      | "" <- T.strip s = Just c
      | otherwise = Just $ T.stripEnd s <> "\n\n" <> c


type A = Maybe CodeComment

simpleInstRule :: String -> InstRule A
simpleInstRule className = IRule Nothing Nothing Nothing (IHCon Nothing (UnQual Nothing (Ident Nothing className)))

deriveInstances :: [String] -> Deriving A
deriveInstances = Deriving Nothing . map simpleInstRule

simpleConDecl :: String -> [String] -> QualConDecl A
simpleConDecl cname pams
  = QualConDecl Nothing Nothing Nothing
     (ConDecl Nothing (Ident Nothing cname)
        $ map (TyCon Nothing . UnQual Nothing . Ident Nothing) pams
     )

simpleDataDecl :: String
               -> [QualConDecl A]
               -> Maybe (Deriving A)
               -> Decl A
simpleDataDecl dname cdecs
    = DataDecl Nothing dataOrNew Nothing
              (DHead Nothing (Ident Nothing dname)) cdecs
  where
    dataOrNew = case cdecs of
      [QualConDecl _ _ _ (ConDecl _ _ [_])] -> NewType Nothing
      [QualConDecl _ _ _ (RecDecl _ _ [_])] -> NewType Nothing
      _                                     -> DataType Nothing


simpleNewtypeDecl :: String
                  -> String
                  -> Maybe (Deriving A)
                  -> Decl A
simpleNewtypeDecl dname t
    = simpleDataDecl dname [simpleConDecl dname [t]]


type Constr = QName A

simpleConstr :: String -> Constr
simpleConstr s = UnQual Nothing (Ident Nothing s)




enumPattern :: Constr -> String -> Int -> Match A
enumPattern constr ename eval =
  Match Nothing (Ident Nothing "pattern")
    [PApp Nothing (UnQual Nothing (Ident Nothing ename)) []]
    (UnGuardedRhs Nothing
     (App Nothing (Con Nothing constr) . f  $ fromIntegral eval)
    ) Nothing
  where
    f x | x < 0 = Paren Nothing
                    (NegApp Nothing
                      (Lit Nothing
                        (Int Nothing (abs x) $ show (abs x))
                      )
                    )
        | otherwise = Lit Nothing (Int Nothing x $ show x)
