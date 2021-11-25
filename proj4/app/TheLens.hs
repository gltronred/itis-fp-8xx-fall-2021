-- | Working with Lenses

{-# LANGUAGE TemplateHaskell #-}

module TheLens where

import Control.Lens
import Control.Lens.TH

data Point = Point
  { _x :: Double
  , _y :: Double
  } deriving (Eq,Show)

data Figure
  = Circle
    { _origin :: Point
    , _radius :: Double
    }
  | Polygon
    { _origin :: Point
    , _points :: [Point]
    }
  deriving (Eq,Show)

makeLenses ''Point

makeLenses ''Figure
makePrisms ''Figure
