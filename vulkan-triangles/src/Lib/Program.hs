{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
-- | Provide a `Program` monad to execute vulkan actions, carry state,
--   manage allocated resources, and process exceptions.
--
--   Note the strictness: we don't want to pile up unevaluated thunks in
--   the program state, but @Strict@ pragma would ruin continuation monad;
--   thus, we have to keep a careful balance between strict and lazy functions.
module Lib.Program
    ( Program (..), Program', runProgram
    , MonadIO (..)
      -- * Resource management
    , allocResource, allocResource', locally
      -- * State manipulation
    , ProgramState (..), MonadState (..), modify, modify', gets
      -- * Exception handling
    , VulkanException (..), MonadError (..), runVk
    , try, bracket, finally, throwVkMsg
      -- * Logging
    , logDebug, logInfo, logWarn, logError
      -- * Development build and debugging
    , isDev, inDev
      -- * Vulkan marshal utilies
    , liftIOWith, withVkPtr
      -- * Other
    , getTime
    , LoopControl (..)
    , checkStatus
    , asyncRedo
    , occupyThreadAndFork
    ) where


import           Control.Concurrent
import           Control.Exception              (Exception, catch,
                                                 displayException, throw,
                                                 throwTo)
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import qualified Control.Monad.Logger           as Logger
import qualified Control.Monad.Logger.CallStack as LoggerCS
import           Control.Monad.State.Class
import           Data.IORef
import           Data.String                    (fromString)
import           Data.Time.Clock.System
import           Data.Tuple                     (swap)
import           GHC.Stack
import           Graphics.Vulkan
import           Graphics.Vulkan.Core_1_0
import           System.Exit


data ProgramState
  = ProgramState
  { currentStatus :: VkResult
    -- ^ Result of the last vulkan command.
    --   We may need it to check if result is some non-zero non-error code.
  , loggingFunc   :: Logger.Loc
                  -> Logger.LogSource
                  -> Logger.LogLevel
                  -> Logger.LogStr -> IO ()
    -- ^ Enable monad-logger.
  , startTime :: SystemTime
    -- ^ Time for animations and physics
  }

iProgState :: IO ProgramState
iProgState = do
  -- get logger function from Control.Monad.Logger transformer
  logFun <- Logger.runStdoutLoggingT $ Logger.LoggingT pure
  time <- getSystemTime
  return ProgramState
    { currentStatus = VK_SUCCESS
    , loggingFunc   = logFun
    , startTime     = time
    }

-- | Program is modelled as a combination of several transformers:
--
--   * ReaderT + IORef to model state
--   * ContT for exception handling and careful resource management
--   * ExceptT for exception handling
newtype Program r a = Program
  { unProgram :: IORef ProgramState
              -> (Either VulkanException a -> IO r) -> IO r }

-- | A common case when program parameter @r@ is restricted to be
--   either conter of the monad or an error.
type Program' a = Program (Either VulkanException a) a


runProgram :: (Either VulkanException a -> IO r)
           -> Program r a -> IO r
runProgram c p = iProgState >>= newIORef >>= flip (unProgram p) c


-- | Allocate some resource and return it,
--   free that resource after the continuation is executed.
--
--   If exception happens during allocation, the freeing does not happen.
--
allocResource :: (a -> Program' ()) -- ^ free resource
              -> Program r a -- ^ allocate resource
              -> Program r a
allocResource free alloc = Program $ \ref c ->
  unProgram alloc ref $ \case
    Left e -> c (Left e)
    Right a -> c (Right a) >>= \r -> r <$ unProgram (free a) ref pure
{-# INLINE allocResource #-}

-- | The same as `allocResource`, but does not prepend a
--   resource release action to the continuation.
--   Instead, return the resource-release-action-continuation-prepending action.
--   The latter may be envoked later to add the resource-release-action to
--   the beginning of the continuation.
--
--   This gives a more granular control over when the release of a resource
--   should happen. For example, when the resource allocation and release
--   should happen in the same order.
allocResource' :: (a -> Program' ()) -- ^ free resource
               -> Program r a -- ^ allocate resource
               -> Program r (a, Program r ())
allocResource' free alloc = Program $ \ref c ->
  unProgram alloc ref $ \case
    Left e -> c (Left e)
    Right a -> c (Right (a, Program $ \ref' c' ->
                                 c' (Right ()) >>=
                                      \r -> r <$ unProgram (free a) ref' pure))
{-# INLINE allocResource' #-}

-- | Run nested continuations locally:
--     fully execute the program in IO;
--     all allocated resources are released before this function returns.
--
--   Note: this means none of the resources allocated in the nested program
--         can be used outside of it! Never return them from `locally`!
locally :: Program' a
        -> Program r a
locally p = Program $ \ref c -> unProgram p ref pure >>= c
{-# INLINE locally #-}

instance Functor (Program r) where
  fmap f p = Program $ \ref c -> unProgram p ref (c . fmap f)
  {-# INLINE fmap #-}

instance Applicative (Program r) where
  pure x = Program $ const ($ Right x)
  {-# INLINE pure #-}
  pf <*> px = Program $
    \ref c -> unProgram pf ref $ \g -> unProgram px ref (c . (g <*>) )
  {-# INLINE (<*>) #-}

instance Monad (Program r) where
  return = pure
  {-# INLINE return #-}
  px >>= k = Program $
    \ref c -> unProgram px ref $ \case
      Right x -> unProgram (k x) ref c
      Left e  -> c (Left e)
  {-# INLINE (>>=) #-}

instance MonadIO (Program r) where
  liftIO m = Program $ const (Right <$> m >>=)
  {-# INLINE liftIO #-}

instance MonadState ProgramState (Program r) where
  get = Program $ \ref -> (Right <$> readIORef ref >>=)
  {-# INLINE get #-}
  put s = Program $ \ref -> (Right <$> writeIORef ref s >>=)
  {-# INLINE put #-}
  state f = Program $ \ref -> (Right <$> atomicModifyIORef' ref (swap . f) >>=)
  {-# INLINE state #-}


-- | Use this to throw all exceptions in this project
data VulkanException
  = VulkanException
  { vkeCode    :: Maybe VkResult
  , vkeMessage :: String
  } deriving (Eq, Show, Read)

instance Exception VulkanException where
  displayException (VulkanException Nothing msg)
    = unlines
    [ ""
    , "Vulkan exception:"
    , "*** " ++ msg
    ]
  displayException (VulkanException (Just c) msg)
    = unlines
    [ ""
    , "Vulkan error: " ++ show c
    , "*** " ++ msg
    ]

instance MonadError VulkanException (Program r) where
  throwError e = Program $ const ($ Left e)
  {-# INLINE throwError #-}
  catchError px catcher = Program $ \ref c -> unProgram px ref $ \case
    Left e  -> unProgram (catcher e) ref c
    Right r -> c (Right r)
  {-# INLINE catchError #-}


-- | Throw VulkanException without error code
throwVkMsg :: HasCallStack => String -> Program r a
throwVkMsg msg = throwError . VulkanException Nothing $ unlines
  [ msg
  , prettyCallStack callStack
  ]
{-# INLINE throwVkMsg #-}



-- | An adaptation of @bracket@ from `Control.Exception`.
--
--   This does not handle vanilla Haskell exceptions, and should only be used
--   to catch the exception defined in this module.
bracket :: Program r a        -- ^ computation to run first (\"acquire resource\")
        -> (a -> Program r b) -- ^ computation to run last (\"release resource\")
        -> (a -> Program r c) -- ^ computation to run in-between
        -> Program r c        -- returns the value from the in-between computation
bracket before after thing = do
  a <- before
  er <- try (thing a)
  _ <- after a
  Program $ const ($ er)
{-# INLINE bracket #-}


-- | A specialised variant of 'bracket' with just a computation to run
--   afterward.
--
--  An adaptation of @finally@ from `Control.Exception`
--
--   This does not handle vanilla Haskell exceptions, and should only be used
--   to catch the exception defined in this module.
finally :: Program r a  -- ^ computation to run first
        -> Program r b  -- ^ computation to run afterward (even if an exception
                        -- was raised)
        -> Program r a  -- returns the value from the first computation
finally a sequel = do
  er <- try a
  _ <- sequel
  Program $ const ($ er)
{-# INLINE finally #-}


-- | An adaptation of @try@ from `Control.Exception`
--
--   This does not handle vanilla Haskell exceptions, and should only be used
--   to catch the exception defined in this module.
try :: Program r a -> Program r (Either VulkanException a)
try a = Program $ \ref c -> unProgram a ref $ c . Right
{-# INLINE try #-}


-- | Run vulkan command, throwing an exception if its result is an error.
runVk :: HasCallStack => IO VkResult -> Program r ()
runVk action = do
  r <- liftIO action
  state $ \s -> ((), s { currentStatus = r })
  when (r < 0) . throwError . VulkanException (Just r)
    $ "Vulkan command returned an error VkResult\n"
    ++ prettyCallStack callStack
{-# INLINE runVk #-}


instance Logger.MonadLogger (Program r) where
  monadLoggerLog loc ls ll msg = do
    logFun <- gets loggingFunc
    liftIO $ logFun loc ls ll (Logger.toLogStr msg)
  {-# INLINE monadLoggerLog #-}


logDebug :: HasCallStack => String -> Program r ()
#ifdef DEVELOPMENT
logDebug = LoggerCS.logDebug . fromString
#else
logDebug = const (pure ())
#endif
{-# INLINE logDebug #-}

logInfo :: HasCallStack => String -> Program r ()
logInfo = LoggerCS.logInfo . fromString
{-# INLINE logInfo #-}

logWarn :: HasCallStack => String -> Program r ()
logWarn = LoggerCS.logWarn . fromString
{-# INLINE logWarn #-}

logError :: HasCallStack => String -> Program r ()
logError = LoggerCS.logError . fromString
{-# INLINE logError #-}


isDev :: Bool
#ifdef DEVELOPMENT
isDev = True
#else
isDev = False
#endif
{-# INLINE isDev #-}

inDev :: Applicative m => m () -> m ()
#ifdef DEVELOPMENT
inDev = id
#else
inDev = const (pure ())
#endif
{-# INLINE inDev #-}


-- | Run an IO action with a callback in Program monad
liftIOWith :: ((a -> IO (Either VulkanException b))
                    -> IO (Either VulkanException b)
              )
           -> (a -> Program' b) -> Program r b
liftIOWith iof pf = Program $ \ref c ->
  iof (\a -> unProgram (pf a) ref pure) >>= c
{-# INLINE liftIOWith #-}


withVkPtr :: VulkanMarshal a
          => a
          -> (Ptr a -> Program' b)
          -> Program r b
withVkPtr x = liftIOWith (withPtr x)
{-# INLINE withVkPtr #-}

-- | Low latency time in seconds since the start
getTime :: Program r Double
getTime = do
    now <- liftIO getSystemTime
    start <- startTime <$> get
    let deltaSeconds = systemSeconds now - systemSeconds start
        -- Have to nanoseconds convert from Word64 before subtraction to allow negative delta.
        deltaNanoseconds :: Int64 = fromIntegral (systemNanoseconds now) - fromIntegral (systemNanoseconds start)
        -- Seconds in Double keep at least microsecond-precision for 285 years.
        -- Float is not good enough even for millisecond-precision over more than a few hours.
        seconds :: Double = fromIntegral deltaSeconds + fromIntegral deltaNanoseconds / 1e9
    return seconds


data LoopControl = ContinueLoop | AbortLoop deriving Eq


checkStatus :: Either VulkanException () -> IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure


-- | Like forkIO, but prints when the thread starts and ends, and tells if it ends with an exception
debugForkIO :: IO () -> IO ThreadId
debugForkIO action = forkFinally (announce >> action) finish where
  announce = do
    tid <- myThreadId
    putStrLn $ "New Thread (" ++ show tid ++ ")"
  finish res = do
    tid <- myThreadId
    let resStr = case res of Right _ -> "normally"
                             Left ex -> "with an exception: " ++ show ex
    putStrLn $ "Terminated Thread (" ++ show tid ++ ") " ++ resStr


data RedoSignal = SigRedo | SigExit deriving Eq


-- | Allows restarting the given prog in a new thread while the old one is still running.
--
--   Enables deferred deallocation.
asyncRedo :: (Program s () -> Program' ()) -> Program r ()
asyncRedo prog = go where
  go = do
    control <- liftIO $ newEmptyMVar
    let trigger = do
          success <- liftIO $ tryPutMVar control SigRedo
          when (not success) $ throwVkMsg "asyncRedo action tried to signal more than once"
          liftIO yield
    -- this program launches the redo-thread and continues with result () immediately
    Program $ \ref c -> do
      -- TODO use forkOS when using unsafe ffi calls?
      -- don't need the threadId
      _ <- forkIO $ do
        unProgram (prog trigger) ref pure >>= checkStatus
        -- When the redo-thread exits, we only need to signal exit to the parent
        -- if nothing else has been signalled yet.
        tryPutMVar control SigExit >> return ()
      -- can't have a real result after forking, but the continuation needs to be called
      c (Right ())
    sig <- liftIO $ takeMVar control
    when (sig == SigRedo) go


-- | For C functions that have to run in the main thread as long as the program runs.
occupyThreadAndFork :: Program' () -> Program' () -> Program r ()
occupyThreadAndFork mainProg deputyProg = Program $ \ref c -> do
  mainThreadId <- myThreadId
  _ <- forkFinally (runInBoundThread $ unProgram deputyProg ref pure >>= checkStatus) $ \res ->
    case res of Left exception -> throw exception
                Right _        -> throwTo mainThreadId ExitSuccess
  exitCode <- catch (unProgram mainProg ref pure >>= checkStatus >> return ExitSuccess) $
                    \(exitCode :: ExitCode) -> return exitCode
  c (Right ()) >> exitWith exitCode
