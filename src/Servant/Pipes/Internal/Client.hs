{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Pipes.Internal.Client where

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import qualified Data.ByteString.Lazy as BL
import Data.ByteString as BS (ByteString, null)
import Data.ByteString.Lazy (fromStrict)
import Data.Monoid ((<>))

import qualified Network.HTTP.Client as HC
import Network.HTTP.Client (Manager, responseOpen, responseClose, brRead)
import Network.HTTP.Types (Method, Header, statusCode)
import Network.HTTP.Media (MediaType, (//), parseAccept)

import qualified Pipes.Prelude as P
import Pipes (Producer, (>->), liftIO, yield)
import Pipes.ByteString (toLazyM)
import Pipes.Safe (SafeT, runSafeT, MonadSafe(..))

import Servant.Client
import Servant.Common.Req

---------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------

-- | Body to be shown in some error conditions. We don't want to dump
-- everything, because the output might be very large (or even infinite).
truncateBody :: Producer ByteString (SafeT IO) () -> IO BL.ByteString
truncateBody p = do
	sample <- runSafeT . toLazyM $ p >-> P.take 2000
	return $ sample <> "\n<< output truncated >>\n"


---------------------------------------------------------------------
-- Streaming Requests
---------------------------------------------------------------------

performStreamRequest
	:: Method
	-> Req
	-> Manager
	-> BaseUrl
	-> ClientM (MediaType, [Header], Producer ByteString (SafeT IO) ())
performStreamRequest method req manager host = do
	partialRequest <- liftIO $ reqToRequest req host

	let
		request = partialRequest
			{ HC.method = method
			, HC.checkStatus = \_ _ _ -> Nothing
			}

	res <- liftIO $ responseOpen request manager

	let
		br = HC.responseBody res
		stream = do
			f <- register (responseClose res)
			let
				p = do
					bs <- liftBase $ brRead br
					if BS.null bs then release f else yield bs >> p
			p

		status = HC.responseStatus res
		status_code = statusCode status
		hdrs = HC.responseHeaders res

	ct <- case lookup "Content-Type" hdrs of
		Nothing -> return $ "application"//"octet-stream"
		Just t -> case parseAccept t of
			Nothing -> do
				body <- liftIO $ truncateBody stream
				throwError $ InvalidContentTypeHeader (fromStrict t) body
			Just t' -> pure t'

	unless (status_code >= 200 && status_code < 300)
		$ do
			body <- liftIO . runSafeT $ toLazyM stream
			throwError (FailureResponse status ct body)

	return (ct, hdrs, stream)
