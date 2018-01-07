{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
module Write
  (generateVkSource
  ) where

import           Data.Semigroup
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Path

-- import           Language.Haskell.Exts.Build
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.ExactPrint
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           Text.Shakespeare.Text
-- import Text.PrettyPrint

import           VkXml.Sections


generateVkSource :: Path b Dir
                    -- ^ multiline
                    --     haddock!
                    --
                    --   >>> show 1
                    --
                 -> VkXml ()
                 -> IO ()
generateVkSource _outputDir _vkXml = do

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
  putStrLn $ uncurry exactPrint . toExactHaddocked $ m
  putStrLn "-----------------"



toExactHaddocked :: Module (Maybe Text)
                 -> (Module SrcSpanInfo, [Comment])
toExactHaddocked m@(Module _ _ _ _ decs)
  = case parseModule $ prettyPrint m of
      err@ParseFailed {} -> error $ show err
      ParseOk m' ->
        let f (Module l h p i _) [] procDecs comms
              = (Module l h p i $ reverse procDecs, comms)
            f om@(Module _ _ _ _ odecs) (mtxt:ms) procDecs ocomms
              = let (Module l h p i ndecs, ncomms)
                       = insertPreCommentsSSI mtxt (srcInfoSpan . ann $ head odecs) om
                in f (Module l h p i $ tail ndecs) ms (head ndecs : procDecs)
                       $ ocomms <> ncomms
            f _ _ _ _ = undefined
        in f m' (map ann decs) [] []
toExactHaddocked _ = error "unexpected module layout"


-- -- | Make a textual comment into a top-level declaration documentation.
-- topLevelDecDoc :: Int -> Maybe Text -> [Comment]
-- topLevelDecDoc _ Nothing = []
-- topLevelDecDoc n (Just txt) = mkComment n lns
--   where
--     lns = map T.unpack . indent $ T.lines txt
--     indent []     = []
--     indent (x:xs) = (" | " <> x) : map ("   " <>) xs
--     mkComment _ [] = []
--     mkComment i (x:xs)
--       = Comment False (SrcSpan "<unknown>.hs" i 1 i $ 3 + length x) x
--         : mkComment (i+1) xs
--
-- -- | Shift this many lines up or down
-- shiftLinesSpan :: Int -> SrcSpan -> SrcSpan
-- shiftLinesSpan n s@SrcSpan {..}
--   = s
--   { srcSpanStartLine = srcSpanStartLine + n
--   , srcSpanEndLine = srcSpanEndLine + n
--   }
--
-- shiftLines :: Int -> SrcSpanInfo -> SrcSpanInfo
-- shiftLines n SrcSpanInfo {..}
--   = SrcSpanInfo
--   { srcInfoSpan = shiftLinesSpan n srcInfoSpan
--   , srcInfoPoints = map (shiftLinesSpan n) srcInfoPoints
--   }
--
-- shiftComment :: Int -> Comment -> Comment
-- shiftComment n (Comment a s c) =  Comment a (shiftLinesSpan n s) c
--
-- -- | Add top-level haddock comment on top of the source,
-- --   expanding spaninfo down.
-- catCommentSpan :: Maybe Text -> SrcSpanInfo -> (SrcSpanInfo, [Comment])
-- catCommentSpan mtxt SrcSpanInfo {srcInfoSpan = s, srcInfoPoints = ps} =
--     ( SrcSpanInfo
--       { srcInfoSpan = s
--          { srcSpanEndLine = srcSpanEndLine s + n
--          }
--       , srcInfoPoints = map (shiftLinesSpan n) ps
--       }
--     , cmts )
--   where
--     cmts = topLevelDecDoc (srcSpanStartLine s) mtxt
--     n = length cmts
--
-- -- | Combine two sources vertically
-- catSources :: SrcSpanInfo -> SrcSpanInfo -> SrcSpanInfo
-- catSources s1 s2
--   = let n = fst . spanSize $ srcInfoSpan s1
--         s2' = shiftLines n s2
--     in SrcSpanInfo
--        { srcInfoSpan = mergeSrcSpan (srcInfoSpan s1) (srcInfoSpan s2')
--        , srcInfoPoints = srcInfoPoints s1 <> srcInfoPoints s2'
--        }


insertPreCommentsSSI :: Functor f
                     => Maybe Text
                     -> SrcSpan -- ^ location of an element
                                --   for comments to be attached
                     -> f SrcSpanInfo -> (f SrcSpanInfo, [Comment])
insertPreCommentsSSI mtxt locs = flip (,) cmts . fmap f
  where
    cmtLoc = SrcLoc (srcSpanFilename locs)
                    startL
                    (srcSpanStartColumn locs)
    cmts = mkComments cmtLoc '|' mtxt
    startL = srcSpanStartLine locs
    lineN = length cmts
    f SrcSpanInfo {srcInfoSpan = s, srcInfoPoints = ps}
      = SrcSpanInfo
      { srcInfoSpan = g s, srcInfoPoints = fmap g ps }
    g s | srcSpanEndLine s < startL
          = s
        | srcSpanStartLine s >= startL
          = s { srcSpanStartLine = srcSpanStartLine s + lineN
              , srcSpanEndLine = srcSpanEndLine s + lineN
              }
        | otherwise
          = s { srcSpanEndLine = srcSpanEndLine s + lineN }

-- | Make a textual comment into a documentation.
mkComments :: SrcLoc -- ^ location of the comment start
           -> Char -- ^ special comment character (i.e. "*" or "^" or "|")
           -> Maybe Text -- ^ text to put into a comment (multiline)
           -> [Comment]
mkComments _ _ Nothing = []
mkComments SrcLoc {..} c (Just txt) = mkComment srcLine lns
  where
    lns = indent . lines $ T.unpack txt
    indent []     = []
    indent (x:xs) = (' ':c:' ':x) : map ("   " ++) xs
    mkComment _ [] = []
    mkComment i (x:xs)
      = Comment False
        (SrcSpan srcFilename i srcColumn i $ srcColumn + 2 + length x) x
        : mkComment (i+1) xs
