{-# OPTIONS -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import HSlew

import Control.Applicative ((<|>))
import Data.Text (Text)
import Toml (TomlCodec, (.=))

import qualified Data.Text.IO as TIO
import qualified Toml

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
    tomlRes <- Toml.decodeFileEither configCodec "./test/config.toml"
    case tomlRes of
        Left errs      -> TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
        Right settings ->
          putStr . unlines . fmap show $ design settings
    
