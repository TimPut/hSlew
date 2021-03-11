{- |
Copyright: (c) 2021 tim put
SPDX-License-Identifier: GPL-3.0-only
Maintainer: tim put <timput@gmail.com>

See README for more info
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HSlew where

import Control.Parallel.Strategies
import Control.Monad (join)
import GHC.Generics

data Design = Design
  { sunTeeth :: Int
  , planetTeeth :: Int
  , ringInnerTeeth :: Int
  , ringOuterTeeth :: Int
  , carrierTeeth :: Int
  , ringDriveTeeth :: Int
  , carrierDriveTeeth :: Int
  , ratio :: Double
  , c2cDist :: Double
  , gearModule :: Double
  , planetPositions :: [(Double,Double)]
  }
  deriving (Show, Generic, NFData)

data DesignConstraints = DesignConstraints
  { minSunDiam :: Double
  , maxNumTeeth :: Int
  , minNumTeeth :: Int
  , minModule :: Double
  , maxModule :: Double
  , minPlanetDiam :: Double
  , maxDiam :: Double
  , minRatio :: Double
  , maxRatio :: Double
  , minDeltaRing :: Double
  }

defDes = DesignConstraints { minSunDiam = 12
                           , maxNumTeeth = 100
                           , minNumTeeth = 11
                           , minModule = 0.5
                           , maxModule = 2
                           , minPlanetDiam = 10
                           , maxDiam = 70
                           , minRatio = 5000
                           , maxRatio = 6500
                           , minDeltaRing = 12}

design :: DesignConstraints -> [Design]
design dcs = let minSunTeeth = max (minNumTeeth dcs) (ceiling ((minSunDiam dcs) / (maxModule dcs))) :: Int
                 maxSunTeeth = min (floor ((maxDiam dcs) / (minModule dcs))) (maxNumTeeth dcs) :: Int
             in join ([go dcs sun | sun <- [minSunTeeth..maxSunTeeth]]
                     `using` parList rdeepseq)
             -- we split the work on sun tooth count for parallelization
             -- could have split on other params equally.
  where
    go dcs s = [Design
                 { sunTeeth = s
                 , planetTeeth = p
                 , ringInnerTeeth = ri
                 , ringOuterTeeth = ro
                 , carrierTeeth   = c
                 , ringDriveTeeth = rd
                 , carrierDriveTeeth = cd
                 , ratio = r
                 , c2cDist = d
                 , gearModule = m
                 , planetPositions = pps
                 }
               | m <- steps (minModule dcs) (maxModule dcs) 10
               , diam m s > minSunDiam dcs
               , p <- [minNumTeeth dcs..maxNumTeeth dcs]
               , diam m p > minPlanetDiam dcs
               , let ri = s + 2 * p :: Int
               , ro <- [fromIntegral(ceiling(fromIntegral ri+(minDeltaRing dcs / m)))..maxNumTeeth dcs]
               , diam m ro < maxDiam dcs
               , rd <- [minNumTeeth dcs..maxNumTeeth dcs]
               , cd <- [minNumTeeth dcs..maxNumTeeth dcs]
               , let d = (dist m rd ro)
               , let c = rd + ro - cd
               , c > 0
               , let r = recip $ driveRatio rd ro cd c p s
               , let r' = abs r in minRatio dcs < r' && r' < maxRatio dcs
               -- hardcode three planets
               , let pps = fmap (angleToPosition (fromIntegral (s + p) * m/2)) $ positionPlanets p s 3
               ]

steps :: Double -> Double -> Int -> [Double]
steps lb ub n = [lb,lb+(ub-lb)/(fromIntegral n)..ub]

-- Center to Center distance between gears
dist :: Double -> Int -> Int -> Double
dist m t1 t2 = (fromIntegral (t1 + t2)*m)/2

-- gear diameter
diam m t = fromIntegral t * m

-- Compute total drive ratio of differential arrangement
driveRatio :: Int -> Int -> Int -> Int -> Int -> Int -> Double
driveRatio rd ro cd c p s = ((ra * (fromIntegral cd / fromIntegral c))
                              + (rb * (fromIntegral rd / fromIntegral ro)))
          where
            ri = fromIntegral (s + 2 * p)
            ra = 1 + (ri / fromIntegral s)
            rb = -1 * (ri / fromIntegral s)

-- https://woodgears.ca/gear/planetary.html
angleBetweenPlanets p s = (2*pi) / (r + s)
    where r = 2 * p + s

angleToPosition :: Double -> Double -> (Double, Double)
angleToPosition rho theta = (rho * cos theta
                            ,rho * sin theta)

-- we may end up with tooth counts which do not allow symmetric planet placement
-- we instead place the planets at the nearest location to symmetric where the
-- teeth line up properly
positionPlanets :: Int -> Int -> Int -> [Double]
positionPlanets p s numPlanets = fmap (\s -> fromIntegral s * alpha) (init [0,steps `div` numPlanets..steps])
    where
      steps = floor ((2*pi) / alpha)
      alpha = angleBetweenPlanets (fromIntegral p) (fromIntegral s)
