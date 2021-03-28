{- |
Copyright: (c) 2021 tim put
SPDX-License-Identifier: GPL-3.0-only
Maintainer: tim put <timput@gmail.com>

See README for more info
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module HSlew (Design(..)
             ,DesignConstraints(..)
             ,design
             ,toScad) where

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
  deriving stock (Show, Generic)
deriving anyclass instance NFData Design

toScad :: Design -> String
toScad d = "sunTeeth = " ++ show (sunTeeth d) ++ ";\n"
           ++ "planetTeeth = " ++ show (planetTeeth d) ++ ";\n"
           ++ "ringInner = " ++ show (ringInnerTeeth d) ++ ";\n"
           ++ "ringOuter = " ++ show (ringOuterTeeth d) ++ ";\n"
           ++ "carrierTeeth = " ++ show (carrierTeeth d) ++ ";\n"
           ++ "ringDrive = " ++ show (ringDriveTeeth d) ++ ";\n"
           ++ "carrierDrive = " ++ show (carrierDriveTeeth d) ++ ";\n"
           ++ "ratio = " ++ show (ratio d) ++ ";\n"
           ++ "c2cDist = " ++ show (c2cDist d) ++ ";\n"
           ++ "mod = " ++ show (gearModule d) ++ ";\n"
           ++ "ps = " ++ (fmap brackets . show . planetPositions $ d) ++ ";\n"
  where
    brackets '(' = '['
    brackets ')' = ']'
    brackets c = c

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

{-
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
-}
design :: DesignConstraints -> [Design]
design dcs = let minSunTeeth = max (minNumTeeth dcs) (ceiling ((minSunDiam dcs) / (maxModule dcs))) :: Int
                 maxSunTeeth = min (floor ((maxDiam dcs) / (minModule dcs))) (maxNumTeeth dcs) :: Int
             in join ([go dcs sun | sun <- [minSunTeeth..maxSunTeeth]]
                     `using` parList rdeepseq)
             -- we split the work on sun tooth count for parallelization
             -- could have split on other params equally.
  where
    go dc s = [Design
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
              | m <- if (minModule dc) == (maxModule dc)
                    then [(minModule dc)]
                    else steps (minModule dc) (maxModule dc) 10
              , diam m s > minSunDiam dc
              , p <- [minNumTeeth dc..maxNumTeeth dc]
              , diam m p > minPlanetDiam dc
              , let ri = s + 2 * p :: Int
              , ro <- [ceiling(fromIntegral ri+(minDeltaRing dc / m))..maxNumTeeth dc]
              , diam m ro < maxDiam dc
              , rd <- [minNumTeeth dc..maxNumTeeth dc]
              , cd <- [minNumTeeth dc..maxNumTeeth dc]
              , let d = (dist m rd ro)
              , let c = rd + ro - cd
              , c > 0
              , let r = recip $ driveRatio rd ro cd c p s
              , let r' = abs r in minRatio dc < r' && r' < maxRatio dc
              -- hardcode three planets
              , let pps = fmap (angleToPosition (fromIntegral (s + p) * m/2)) $ positionPlanets p s 3
              ]

steps :: Double -> Double -> Int -> [Double]
steps lb ub n = [lb,lb+(ub-lb)/(fromIntegral n)..ub]

-- Center to Center distance between gears
dist :: Double -> Int -> Int -> Double
dist m t1 t2 = (fromIntegral (t1 + t2)*m)/2

-- gear diameter
diam :: Double -> Int -> Double
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
angleBetweenPlanets :: Double -> Double -> Double
angleBetweenPlanets p s = (2*pi) / (r + s)
    where r = 2 * p + s

angleToPosition :: Double -> Double -> (Double, Double)
angleToPosition rho theta = (rho * cos theta
                            ,rho * sin theta)

-- we may end up with tooth counts which do not allow symmetric planet placement
-- we instead place the planets at the nearest location to symmetric where the
-- teeth line up properly
positionPlanets :: Int -> Int -> Int -> [Double]
positionPlanets p s numPlanets = fmap (\x -> fromIntegral x * alpha) (init [0,step `div` numPlanets..step])
    where
      step = floor ((2*pi) / alpha)
      alpha = angleBetweenPlanets (fromIntegral p) (fromIntegral s)
