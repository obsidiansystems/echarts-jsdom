{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.IORef
import qualified Data.Text as T
import Data.Word
import Obelisk.Backend
import Snap

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      latest <- newIORef Nothing
      _ <- forkIO $ forever $ do
        writeIORef latest =<< getCpuStat
        threadDelay 5000
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_Listen :=> Identity () -> do
          liftIO (readIORef latest) >>= \case
            Nothing -> writeText "No Data"
            Just f -> forM_ [minBound..] $ \cs -> do
              writeText $ T.pack (show (cs, f cs)) <> "\n"
  , _backend_routeEncoder = backendRouteEncoder
  }

data CpuStat
   = CpuStat_User
   | CpuStat_Nice
   | CpuStat_System
   | CpuStat_Idle
   | CpuStat_Iowait
   | CpuStat_Irq
   | CpuStat_Softirq
   | CpuStat_Steal
   | CpuStat_Guest
   | CpuStat_GuestNice
   deriving (Show, Read, Eq, Ord, Enum, Bounded)

getCpuStat :: IO (Maybe (CpuStat -> Word64))
getCpuStat = do
  s <- readFile "/proc/stat"
  evaluate $ length s -- Make readFile strict
  pure $ do
    cpuSummaryLine : _ <- pure $ lines s
    [user, nice, system, idle, iowait, irq, softirq, steal, guest, guestNice] <- pure $ map read $ words $ drop 4 cpuSummaryLine
    pure $ \case
      CpuStat_User -> user
      CpuStat_Nice -> nice
      CpuStat_System -> system
      CpuStat_Idle -> idle
      CpuStat_Iowait -> iowait
      CpuStat_Irq -> irq
      CpuStat_Softirq -> softirq
      CpuStat_Steal -> steal
      CpuStat_Guest -> guest
      CpuStat_GuestNice -> guestNice
