{-# LANGUAGE OverloadedStrings #-}

module Olec.Config (
	configKeyTimeout
) where

import Control.Monad
import Control.Exception
import Control.Applicative

import Data.Aeson
import Data.Maybe

import qualified Data.ByteString.Lazy as BL

import System.IO.Unsafe

-- | Configuration capsulation
data Config = Config  {
	-- | Time out duration for key combinations
	_configKeyTimeout :: Int
}

instance FromJSON Config where
	parseJSON (Object v) =
		Config <$> (v .: "key_timeout" <|> pure (_configKeyTimeout defaultConfig))
	parseJSON _ = mzero

-- | Default configuration
defaultConfig :: Config
defaultConfig =
	Config {
		_configKeyTimeout = 2000000
	}

configKeyTimeout :: Int

Config configKeyTimeout =
	unsafePerformIO $
		handle (\ (SomeException _ ) -> pure defaultConfig)
		       (fromMaybe defaultConfig . decode <$> BL.readFile ".olec")
