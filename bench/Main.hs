{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Criterion.IO
import Criterion.Main

import Control.Concurrent.Async

import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P

import Servant
import Servant.Client
import Servant.CSV.Cassava
import Servant.Pipes
import Servant.Pipes.Csv

import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
	t <- async runServer
	link t
	defaultMain
		[ bench "100 rows" $ nfIO (runBench 100)
		, bench "10,000 rows" $ nfIO (runBench 10000)
		, bench "1,000,000 rows" $ nfIO (runBench 1000000)
		]

runBench :: Int -> IO ()
runBench = undefined

data DefaultCsvOpts

instance EncodeOpts DefaultCsvOpts where
	encodeOpts _ = encodeOpts (Proxy :: Proxy DefaultEncodeOpts)

instance DecodeOpts DefaultCsvOpts where
	decodeOpts _ = decodeOpts (Proxy :: Proxy DefaultDecodeOpts)

type TestApi
	= Capture "count" Int
	:> GetStream '[(CSV', DefaultCsvOpts)] (Int, Int)

testApi :: Proxy TestApi
testApi = Proxy

testApiServer :: Server TestApi
testApiServer count =
	return $ P.each [ (x,y) | x <- [0..], y <- [0..9] ] >-> P.take count

runServer :: IO ()
runServer = do
	run 57232 $ serve testApi testApiServer


