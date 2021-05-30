{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Todo.Metrics
  ( Metrics,
    showPage,
    create,
    modify,
    patch,
    delete,
    RegisteredMetrics,
    HasRegisteredMetrics,
    metrics,
    getMetric,
  )
where

import App.Metrics (AppMetrics (metricsRegistered))
import App.Monad (AppM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (asks)
import Data.HKD (TraversableHKD (traverseHKD))
import Data.Has (Has (obtain))
import System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT, registerHistogram)
import System.Metrics.Prometheus.Metric.Histogram as Histogram
import System.Metrics.Prometheus.MetricId (Name (..))

buckets :: [UpperBound]
buckets = [1, 2 .. 20] <> [30, 40 .. 200] <> [300, 400] <> [500, 1000 .. 2000] <> [3000, 4000 .. 10000]

data Metrics m = Metrics
  { showPage :: m Histogram,
    create :: m Histogram,
    modify :: m Histogram,
    patch :: m Histogram,
    delete :: m Histogram
  }

instance TraversableHKD Metrics where
  traverseHKD f Metrics {..} =
    Metrics
      <$> f showPage
      <*> f create
      <*> f modify
      <*> f patch
      <*> f delete

type RegisteredMetrics = Metrics Identity

type HasRegisteredMetrics f = Has RegisteredMetrics (f Identity)

metrics :: Metrics (RegistryT IO)
metrics =
  Metrics
    { showPage = registerHistogram (Name "todo_show_page") mempty buckets,
      create = registerHistogram (Name "todo_create") mempty buckets,
      modify = registerHistogram (Name "todo_modify") mempty buckets,
      patch = registerHistogram (Name "todo_patch") mempty buckets,
      delete = registerHistogram (Name "todo_delete") mempty buckets
    }

getMetric ::
  forall f a.
  HasRegisteredMetrics f =>
  (RegisteredMetrics -> Identity a) ->
  (AppM f) a
getMetric g = asks $ (runIdentity . g . obtain . metricsRegistered) . (obtain @(AppMetrics f))
