{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import HSlew
import Toml (TomlCodec, (.=))
import qualified Data.Text.IO as TIO
import qualified Toml
import System.Environment (getArgs)
import Data.List (sortOn)

configCodec :: TomlCodec DesignConstraints
configCodec = DesignConstraints
    <$> Toml.double "minSunDiam" .= minSunDiam
    <*> Toml.int "maxNumTeeth" .= maxNumTeeth
    <*> Toml.int "minNumTeeth" .= minNumTeeth
    <*> Toml.double "minModule" .= minModule
    <*> Toml.double "maxModule" .= maxModule
    <*> Toml.double "minPlanetDiam" .= minPlanetDiam
    <*> Toml.double "maxDiam" .= maxDiam
    <*> Toml.double "minRatio" .= minRatio
    <*> Toml.double "maxRatio" .= maxRatio
    <*> Toml.double "minDeltaRing" .= minDeltaRing

main :: IO ()
main = do
    args <- getArgs
    case args of
      []     -> putStrLn "no configuration file given"
      (fp:_) -> do
        tomlRes <- Toml.decodeFileEither configCodec fp
    -- tomlRes <- Toml.decodeFileEither configCodec "./test/config.toml"
        case tomlRes of
          Left errs      -> TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
          Right settings ->
            putStr . unlines . fmap toScad . sortOn c2cDist $ design settings
    
