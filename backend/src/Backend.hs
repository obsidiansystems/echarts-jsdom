{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time
import Data.Word
import Network.WebSockets.Snap
import qualified Network.WebSockets as WS
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      connections :: MVar (Int, Map Int WS.Connection) <- newMVar (0, Map.empty)
      let loop mold = do
            t <- getCurrentTime
            stat <- getCpuStat
            let x = do
                  (oldt, oldstat) <- mold
                  f <- stat
                  let user = f CpuStat_User
                      olduser = oldstat CpuStat_User
                      pct :: Double = (realToFrac $ user - olduser) / (realToFrac $ diffUTCTime t oldt)
                  return (pct, f)
            case x of
              Nothing -> do
                threadDelay 100000
                loop $ fmap (\a -> (t, a)) stat
              Just (pct, stat') -> do
                let msg = WS.Text (encode (t, pct)) Nothing
                _ <- withMVar connections $ \(_, conns) ->
                  forM conns $ \c -> WS.sendDataMessage c msg
                threadDelay 100000
                loop $ Just (t, stat')
      _ <- forkIO $ loop Nothing
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_Listen :=> Identity () -> runWebSocketsSnap $ \c -> do
          conn <- WS.acceptRequest c
          modifyMVar_ connections $ \(next, conns) -> return (next + 1, Map.insert next conn conns)
          _ <- forever $ do
            _ <- WS.receiveDataMessage conn
            return ()
          return ()
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
  _ <- evaluate $ length s -- Make readFile strict
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
