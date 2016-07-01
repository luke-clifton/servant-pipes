{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Csv
import Data.Vector

import Pipes as P hiding (Proxy)

import Servant
import Servant.Pipes.Csv

import Network.Wai.Handler.Warp (run)

data Yes = Yes
data No = No

instance ToRecord Yes where
	toRecord _ = singleton "yes"

instance ToRecord No where
	toRecord _ = singleton "no"

type YesNoApi = YesApi :<|> NoApi

type YesApi = "yes" :> GetStream '[(CSV', DefaultEncodeOpts)] Yes

type NoApi = "no" :> GetStream '[(CSV', DefaultEncodeOpts)] No

yesNoApi :: Proxy YesNoApi
yesNoApi = Proxy

yesNoService :: Server YesNoApi
yesNoService = yesService :<|> noService

	where

		yesService = return $ P.each (repeat Yes)

		noService = return $ P.each (repeat No)

main :: IO ()
main = do
	
	putStrLn "Running on port 8054"

	run 8054 $ serve yesNoApi yesNoService
