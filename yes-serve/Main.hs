{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Csv
import Data.Vector

import GHC.Generics

import Pipes as P hiding (Proxy)

import Servant
import Servant.API.ContentTypes
import Servant.CSV.Cassava
import Servant.Docs
import Servant.Pipes.Csv

import System.Environment

import Network.Wai.Handler.Warp (run)

data YesNo = Yes | No
	deriving (Show, Generic)

instance ToRecord YesNo where
	toRecord Yes = singleton "yes"
	toRecord No = singleton "no"

instance ToSample YesNo

type YesNoApi = YesApi :<|> NoApi

data DefaultCsvOpts

instance EncodeOpts DefaultCsvOpts where
	encodeOpts _ = encodeOpts (Proxy :: Proxy DefaultEncodeOpts)

instance DecodeOpts DefaultCsvOpts where
	decodeOpts _ = decodeOpts (Proxy :: Proxy DefaultDecodeOpts)

type YesApi = "yes" :> GetStream '[(CSV', DefaultCsvOpts)] YesNo

type NoApi = "no" :> GetStream '[(CSV', DefaultCsvOpts)] YesNo

yesNoApi :: Proxy YesNoApi
yesNoApi = Proxy

yesNoService :: Server YesNoApi
yesNoService = yesService :<|> noService

	where

		yesService = return $ P.each (repeat Yes)

		noService = return $ P.each (repeat No)

main :: IO ()
main = do
	
	args <- getArgs

	case args of
		[] -> do
			putStrLn "Running on port 8054"
			run 8054 $ serve yesNoApi yesNoService
		["--help"] -> do
			putStrLn . markdown . docs $ yesNoApi
