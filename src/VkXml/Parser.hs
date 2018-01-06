{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE Strict                     #-}

module VkXml.Parser
  ( ParseLoc (..), defParseLoc
  , VkXmlParseException (..)
  , VkXmlParser
  , runAttrParser
  , parseWithLoc
  , parseFailed
  , awaitReq
  , parseTag
  , parseTagForceAttrs
  , unContent
  , forceAttr
  , decOrHex
  ) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.Zip
import           Data.Conduit
import           Data.Conduit.Lift
import           Data.List                 (intercalate)
import           Data.Semigroup
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Read            as T
import           Data.XML.Types
import           GHC.Stack
import           Path
import           Text.XML.Stream.Parse
import           Unsafe.Coerce

-- * Error reporing


defParseLoc :: Path b File
            -> ParseLoc
defParseLoc = ParseLoc Nothing [] . toFilePath

newtype VkXmlParserT m a
  = VkXmlParserT {runVkXmlParserT :: ReaderT ParseLoc m a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadTrans
           , MonadFix, MonadIO, MonadPlus, MonadZip
           , MonadReader ParseLoc, MonadState s
           )

instance MonadThrow m => MonadThrow (VkXmlParserT m) where
  throwM err' = VkXmlParserT $ do
      eLoc <- ask
      let (cs, err) = case fromException (toException err') of
            Nothing                          -> (callStack, show err')
            Just (VkXmlParseException cs' e) -> (cs', show e)
      throwM . VkXmlParseException cs . T.pack
        $ "\nParsing failed at " <> show eLoc <> ":\n  " <> err
        <> maybe "" (\e -> "\nlast opening tags were:\n" <> T.unpack e)
                    (mBeginTag eLoc)
        <> "\n" <> prettyCallStack cs
    where
      mBeginTag eLoc = case treePath eLoc of
        []      -> Nothing
        [x]     -> Just $ mkTag x
        [x,y]   -> Just $ mkTag y <> "\n  " <> mkTag x
        x:y:z:_ -> Just $ mkTag z <> "\n  " <> mkTag y <> "\n    " <> mkTag x
      mkTag (n,as) =
                  "<" <> nameLocalName n
                   <> T.concat (map mkAttr as)
                   <> ">"
      mkAttr (n, cs) = " " <> nameLocalName n <> "=\""
                           <> T.intercalate ","  (map unContent cs)
                           <> "\""


-- | Location of parsing in the xml file
data ParseLoc = ParseLoc
  { posRange :: Maybe PositionRange
  , treePath :: [(Name, [(Name, [Content])])]
  , filePath :: FilePath
  }

instance Show ParseLoc where
  show ParseLoc {..}
    = "{"
       <> (intercalate "." . map (T.unpack . nameLocalName . fst) $ reverse treePath)
       <> "} in " <> filePath
                <> maybe "" ((":[" <>) . (<> "]") . show ) posRange

data VkXmlParseException
  = VkXmlParseException
  { exceptCS              :: CallStack
  , unVkXmlParseException :: Text
  }
instance Show VkXmlParseException where
  show msg = "VkXmlParseException: " <> T.unpack (unVkXmlParseException msg)
instance Exception VkXmlParseException

-- | A constraint to allow parsing with exception throwing
type VkXmlParser m =
  ( MonadReader ParseLoc m
  , MonadThrow m
  )

-- | Show a meaningful message with error location
--   if parsing xml file has failed.
parseFailed :: (VkXmlParser m, HasCallStack) => String -> m a
parseFailed msg =
  throwM . VkXmlParseException callStack $ T.pack msg


-- | Add full location information to the parsing events
parseWithLoc :: Monad m
             => ParseLoc
             -> ConduitM Event o (VkXmlParserT m) r
             -> ConduitM EventPos o m r
parseWithLoc initLoc pipe = parseWithLoc' initLoc =$= readLoc initLoc pipe

goDownTree :: (Name, [(Name, [Content])]) -> ParseLoc -> ParseLoc
goDownTree n pl = pl { treePath = n : treePath pl}

goUpTree :: ParseLoc -> ParseLoc
goUpTree pl = pl { treePath = drop 1 $ treePath pl}

updatePos :: Maybe PositionRange -> ParseLoc -> ParseLoc
updatePos pr pl = pl { posRange = pr }

parseWithLoc' :: Monad m => ParseLoc -> Conduit EventPos m (ParseLoc, Event)
parseWithLoc' defLoc
  = evalStateC defLoc
  $ awaitForever
  $ \(mloc, ev) -> do
    updatedLoc <- lift $ do
      modify' (updatePos mloc)
      case ev of
        EventBeginElement n attrs -> modify' (goDownTree (n,attrs))
        EventEndElement _         -> modify' goUpTree
        _                         -> return ()
      get
    yield ( updatedLoc, ev )

-- | Move location information from upstream to MonadReader environment
readLoc :: Monad m
        => ParseLoc
        -> ConduitM Event o (VkXmlParserT m) r
        -> ConduitM (ParseLoc, Event) o m r
readLoc defLoc pipe
    = evalStateC defLoc
    $ awaitForever updateStateAndYield
   =$= transPipe (\x -> get >>= lift . runReaderT (runVkXmlParserT x)) pipe
  where
    updateStateAndYield (l, ev) = lift (put l) >> yield ev

-- * Helper utils


-- | Fail with `parseFailed` if encounter end of input
awaitReq :: (VkXmlParser m, HasCallStack) => Consumer Event m Event
awaitReq = await >>= maybe (parseFailed "unexpected end of input") pure


-- | Wrapper on top of `tag'` function
--    tries to consume only tag content
--    and cut the upstream for the inner conduit.
--   If name matcher or attr parser fail,
--    events are sent back via leftovers and conduit returns the flow control.
parseTag :: VkXmlParser m
         => Name
            -- ^ match the name exactly
         -> ReaderT ParseLoc AttrParser b
            -- ^ parse tag attributes
         -> (b -> ConduitM Event o m c)
            -- ^ consume stuff inside the tag only
         -> ConduitM Event o m (Maybe c)
parseTag name attrParser pipeF' = do
    getAttrs <- runReaderT attrParser <$> ask
    tag' (matching (name==)) getAttrs
         (\b -> evalStateC (0 :: Int) pipeGuard =$= pipeF' b)
  where
    pipeGuard = awaitReq >>= \ev -> case ev of
      EventBeginElement {} -> do
        modify' (+1)
        yield ev
        pipeGuard
      EventEndElement {} -> do
        modify' (\x -> x-1)
        curNesting <- get
        if curNesting < 0
        then leftover ev
        else do
          yield ev
          pipeGuard
      _ -> yield ev >> pipeGuard

-- | Raise an exception if attributes failed to parse
parseTagForceAttrs :: (VkXmlParser m, HasCallStack)
                   => Name
                      -- ^ match the name exactly
                   -> ReaderT ParseLoc AttrParser b
                      -- ^ parse tag attributes
                   -> (b -> ConduitM Event o m c)
                      -- ^ consume stuff inside the tag only
                   -> ConduitM Event o m (Maybe c)
parseTagForceAttrs name attrParser pipeF' = do
    mr <- parseTag name attrParser pipeF'
    case mr of
      Just r  -> return $ Just r
      Nothing -> await >>=
       \case
          Just ev@(EventBeginElement n attrs)
           | n == name -> runAttrParser attrParser attrs
              >>= \x -> x `seq` parseFailed
               ( "failed to parse tag attributes for " <> show ev
               )
          Just (EventContent c)
            | T.null (T.strip (unContent c))
            -> parseTagForceAttrs name attrParser pipeF'
          Nothing -> return Nothing
          Just ev -> leftover ev >> return Nothing

-- | Use GHC hachery to get
--      `xml-conduit-1.7.0/Text.XML.Stream.Parse.runAttrParser`
runAttrParser :: (VkXmlParser m, HasCallStack)
              => ReaderT ParseLoc AttrParser a -> [(Name, [Content])] -> m a
runAttrParser p x = do
    loc <- ask
    let p' = runReaderT p loc
        pf :: [(Name, [Content])]
           -> Either SomeException ([(Name, [Content])], a)
        pf = p' `seq` unsafeCoerce p'
    case pf x of
      Right ([], r) -> pure r
      Right (xs, _) -> parseFailed
                    $ "some attributes left unhandled: " <> show
                      ( map (\(n, cs) -> T.unpack ( nameLocalName n)
                                      <> " = " <> unwords (map uncontent cs)
                            ) xs )
      Left (SomeException e) -> throwM e
  where
    uncontent = T.unpack . unContent

forceAttr :: HasCallStack => Name -> ReaderT ParseLoc AttrParser Text
forceAttr n = do
  mv <- lift (attr n)
  case mv of
    Nothing -> parseFailed $ "Missing tag attribute " <> show n
    Just v  -> return v

unContent :: Content -> Text
unContent (ContentText t)   = t
unContent (ContentEntity t) = t

decOrHex :: Integral a => T.Reader a
decOrHex t = if "0x" `T.isPrefixOf` t
             then T.hexadecimal t
             else T.decimal t
