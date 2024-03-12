{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Data.Pickle ( Tags
                   , StatsDConfig(..)
                   , MetricData
                   , defaultConfig
                   , setupPickle
                   , metric
                   , gauge
                   , counter
                   , timer
                   , showT
                   )
where

import Data.Map.Strict qualified as M
import Data.Maybe ( fromMaybe )
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Control.Exception ( SomeException, bracketOnError, try )
import Control.Monad ( void, when )
import Network.Socket ( getAddrInfo, connect, socket, close, defaultProtocol, AddrInfo(addrAddress, addrFamily), Socket, SocketType(Datagram) )
import Network.Socket.ByteString (send)
import System.IO.Unsafe ( unsafePerformIO )
import Control.Concurrent.STM ( atomically, newTVarIO, readTVar, readTVarIO, retry, writeTVar, modifyTVar', TVar )
import Data.Pickle.MetricData ( MetricData(renderMetricData) )

-- | Tags for DogStatsD. Use empty text for rhs to send a keyed tag with no value. Example: M.fromList [("flag", "")]
type Tags = M.Map T.Text T.Text

-- | Configuration for the UDP connection used
data StatsDConfig = StatsDConfig { statsdHost      :: T.Text -- ^ Host of statsd server
                                 , statsdPort      :: T.Text -- ^ Port of statsd server
                                 , statsdPrefix    :: T.Text -- ^ Prefix concatenated to all metrics names in our program
                                 , statsdTags      :: Tags   -- ^ 'mappend'-ed tags for all stats we report
                                 , statsdVerbose   :: Bool   -- ^ Whether to print all metrics to stdout
                                 }

-- | 'Pickle' is our Data Dog (get it?) and he holds on to our sock and config.
--   `pickle` is a little MVar bed which Pickle likes to sleep in. He is a good boy.
data Pickle = Pickle { pickleSock :: Socket
                     , pickleCfg  :: StatsDConfig
                     }

-- | Default config used for StatsD UDP connection ()
defaultConfig :: StatsDConfig
defaultConfig = StatsDConfig { statsdHost    = "127.0.0.1"
                             , statsdPort    = "8125"
                             , statsdPrefix  = ""
                             , statsdTags    = M.empty
                             , statsdVerbose = False
                             }

{-|
Start up our statsd client. You probably want to do this first in main:

 > main = do
       setupPickle defaultConfig
       ...

Subsequent calls to 'setupPickle' will close the existing connection and create
a new one with the updated settings. If multiple threads race to setup the
connection, the last one to finish wins.
-}
setupPickle :: StatsDConfig -> IO ()
setupPickle cfg = bracketOnError checkPickle (const finish) setPickle
    where checkPickle = atomically $ do
            gp <- readTVar pickle
            -- Someone else is setting up a connection, wait for them:
            when (gpSetupRunning gp) retry
            writeTVar pickle (gp { gpSetupRunning = True})
            pure (gpPickle gp)
          -- If anything bad happens, unblock other 'setupPickle' callers.
          finish = atomically $ do
            modifyTVar' pickle (\gp -> gp { gpSetupRunning = False })
          setPickle Nothing = do
            pick <- initPickle cfg
            atomically $ do
                writeTVar pickle (GlobalPickle False (Just pick))
          setPickle (Just oldPick) = do
              newPick <- initPickle cfg
              atomically $ do
                  writeTVar pickle (GlobalPickle False (Just newPick))
              -- Close the old pickle connection once we're done.
              close (pickleSock oldPick)

-- | Send a gauge.
gauge :: (MetricData a) => T.Text -> a -> Maybe Tags -> IO ()
gauge name val mTags = metric "g" name val mTags Nothing

-- | Send a counter.
counter :: (MetricData a) => T.Text -> a -> Maybe Tags -> Maybe Float -> IO ()
counter = metric "c"

-- | Send a timer.
timer :: (MetricData a) => T.Text -> a -> Maybe Tags -> Maybe Float -> IO ()
timer = metric "ms"

-- | Send a metric. Parses the options together. This function makes a
--   best-effort to send the metric; no metric-sending exceptions will be
--   thrown. The metric won't be sent if 'setupPickle' hasn't been called yet.
metric :: (MetricData a)
      => T.Text      -- ^ metric kind in character form (g,c,ms,s)
      -> T.Text      -- ^ metric name
      -> a           -- ^ metric value
      -> Maybe Tags  -- ^ Tags for metric
      -> Maybe Float -- ^ Sampling rate for applicable metrics.
      -> IO ()
metric kind n val mTags mSampling = do
    mPick <- gpPickle <$> readTVarIO pickle
    case mPick of
        Nothing -> pure () -- no connection, give up.
        Just (Pickle sock cfg) -> do
            let tags = renderTags $ fromMaybe M.empty mTags <> statsdTags cfg
                sampling = maybe "" (\s -> "|@" <> showT s ) mSampling
                name = statsdPrefix cfg <> n
                msg  = name <> ":" <> renderMetricData val <> "|" <> kind <> sampling <> tags
            when (statsdVerbose cfg) (T.putStrLn $ "Sending metric: " <> msg)
            void (try $ send sock $ T.encodeUtf8 msg :: IO (Either SomeException Int))

-- | Parse tags into string to send.
renderTags :: Tags -> T.Text
renderTags tags
    | M.null tags = ""
    | otherwise   = parsed where
        parsed  = "|#" <> catted
        catted  = T.intercalate "," $ fmap (\(k, v) -> k <> (if T.null v then "" else ":" <> v)) $ M.toList tags

data GlobalPickle = GlobalPickle {
    gpSetupRunning :: Bool
  , gpPickle       :: Maybe Pickle
  }

-- | Internal TVar keeping track of singleton connection.
pickle :: TVar GlobalPickle
pickle = unsafePerformIO $ newTVarIO $ GlobalPickle False Nothing
{-# NOINLINE pickle #-}

-- | Start the connection for our Pickle
initPickle :: StatsDConfig -> IO Pickle
initPickle cfg = do
    when (statsdVerbose cfg) $ putStrLn "Initializing Pickle StatsD Client.."
    addrinfos <- getAddrInfo Nothing (Just $ T.unpack $ statsdHost cfg) (Just $ T.unpack $ statsdPort cfg)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect sock (addrAddress serveraddr)
    pure $ Pickle sock cfg

-- | Internal utility to show something as Text
showT :: (Show a) => a -> T.Text
showT = T.pack . show
