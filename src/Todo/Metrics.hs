{-# LANGUAGE RecordWildCards #-}

module Todo.Metrics
  ( showPage,
    create,
    modify,
    patch,
    delete,
    metrics,
    Metrics,
    WithTodoMetrics (..),
  )
where

import Control.Monad.Identity (Identity)
import Data.HKD (FunctorHKD (mapHKD), TraversableHKD (traverseHKD))
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

metrics :: Metrics (RegistryT IO)
metrics =
  Metrics
    { showPage = registerHistogram (Name "todo_show_page") mempty buckets,
      create = registerHistogram (Name "todo_create") mempty buckets,
      modify = registerHistogram (Name "todo_modify") mempty buckets,
      patch = registerHistogram (Name "todo_patch") mempty buckets,
      delete = registerHistogram (Name "todo_delete") mempty buckets
    }

instance FunctorHKD Metrics where
  mapHKD f m =
    Metrics
      { showPage = f $ showPage m,
        create = f $ create m,
        modify = f $ modify m,
        patch = f $ patch m,
        delete = f $ delete m
      }

instance TraversableHKD Metrics where
  traverseHKD f Metrics {..} =
    Metrics
      <$> f showPage
      <*> f create
      <*> f modify
      <*> f patch
      <*> f delete

class WithTodoMetrics f where
  getTodoMetrics :: f Identity -> Metrics Identity
