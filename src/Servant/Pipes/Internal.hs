{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Pipes.Internal where

import Servant.Pipes.Internal.Client

import Control.Lens ((&), (.~),)

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (byteString)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.TypeLits (Nat, KnownNat, natVal)

import Network.HTTP.Client (Manager)
import Network.HTTP.Types (hAccept, hContentType, Method, Header)
import Network.HTTP.Media (MediaType, mapAcceptMedia, renderHeader, matches)
import Network.Wai (requestHeaders, responseStream)

import Pipes (Producer, (>->), runEffect, for, liftIO, each)
import Pipes.ByteString (toLazy)
import Pipes.Csv (ToRecord, FromRecord, encodeWith, decodeWith, HasHeader(..))
import Pipes.Safe (SafeT, runSafeT)

import Servant (StdMethod(..), Proxy(..), HasServer(..), err406, ReflectMethod(..))
import Servant.API.ContentTypes (AllMime, Accept, contentType, AcceptHeader(..), allMime)
import Servant.Client
import Servant.Common.Req
import Servant.CSV.Cassava (EncodeOpts(..), CSV', DecodeOpts(..))
import Servant.Docs.Internal (HasDocs(..), DocOptions(..), ToSample(..), response, respStatus, respTypes, respBody, API(..), method)
import Servant.Server.Internal (leafRouter, ct_wildcard, RouteResult(..), runAction)

---------------------------------------------------------------------
-- Core Combinators
---------------------------------------------------------------------

-- | Used to control the data stream.
data Flush
	= NoFlush -- ^ Don't flush
	| Flush   -- ^ Flush after each element in the stream

class ReflectFlush a where
	reflectFlush :: Proxy a -> Flush

instance ReflectFlush 'Flush where
	reflectFlush Proxy = Flush

instance ReflectFlush 'NoFlush where
	reflectFlush Proxy = NoFlush

-- | The combinator to use when you want a streaming interface for your
-- servant API. See also `GetStream`, `PutStream` and `PostStream`.
data Stream
	(method :: k)
	(status :: Nat)
	(flush :: Flush)
	(ctypes :: [*])
	(a :: *)

-- | A useful abreviation of `Stream`
type GetStream = Stream 'GET 200 'NoFlush

---------------------------------------------------------------------
-- Serialisation and Deserialisation Typeclasses
---------------------------------------------------------------------

-- | Streaming version of `MimeRender`
class StreamMimeRender (ctype :: k) a where

	-- | Convert a `Producer` of `a`s into a `Producer` of `ByteString`s
	-- as required by the content type.
	mimeStreamRender
		:: Monad m
		=> Proxy ctype
		-> Producer a m ()
		-> Producer ByteString m ()

-- | Streaming version of `MimeUnrender`
class StreamMimeUnrender (ctype :: k) a where

	-- | The error type used by the deserialiser.
	type DecodeErr ctype a

	-- | Convert a `Producer` of `ByteString`s into a `Producer` of
	-- `Either (DecodeErr ctype a) a`.
	mimeStreamUnrender
		:: Monad m
		=> Proxy ctype
		-> Producer ByteString m ()
		-> Producer (Either (DecodeErr ctype a) a) m ()

---------------------------------------------------------------------
-- StreamMimeRender Instances
---------------------------------------------------------------------

instance
	( EncodeOpts opt
	, ToRecord a
	) => StreamMimeRender (CSV', opt) a where

	mimeStreamRender Proxy p = p >-> encodeWith (encodeOpts opt)
		where
			opt = Proxy :: Proxy opt

instance
	( DecodeOpts opt
	, FromRecord a
	) => StreamMimeUnrender (CSV', opt) a where

	type DecodeErr (CSV', opt) a = String

	mimeStreamUnrender Proxy p = decodeWith (decodeOpts (Proxy :: Proxy opt)) NoHeader p

---------------------------------------------------------------------
-- HasClient Instance for Stream
---------------------------------------------------------------------

-- | Perform a streaming request, checking the `Content-Type` header
-- matches the expected value.
performStreamRequestCT
	:: (StreamMimeUnrender ct result, Accept ct)
	=> Proxy ct
	-> Method
	-> Req
	-> Manager
	-> BaseUrl
	-> ClientM
		( [Header]
		, Producer (Either (DecodeErr ct result) result) (SafeT IO) ()
		)

performStreamRequestCT ct meth req manager host = do
	let
		acceptCt = contentType ct

	(respCt, hdrs, p) <-
		performStreamRequest meth req{reqAccept = [acceptCt]} manager host

	unless (matches respCt acceptCt) $ do
		body <- liftIO $ truncateBody p
		throwError (UnsupportedContentType respCt body)

	let
		stream = mimeStreamUnrender ct p

	return (hdrs, stream)
instance
	( StreamMimeUnrender ct a
	, Accept ct
	, ReflectMethod method
	) => HasClient (Stream method status flush (ct ': cts) a) where

	type Client (Stream method status flush (ct ': cts) a)
		= Manager -> BaseUrl -> ClientM (Producer (Either (DecodeErr ct a) a) (SafeT IO) ())

	clientWithRoute Proxy req manager baseurl = do
		let
			meth = reflectMethod (Proxy :: Proxy method)

		(_, stream) <-
			performStreamRequestCT (Proxy :: Proxy ct) meth req manager baseurl

		return stream

---------------------------------------------------------------------
-- HasServer Instance for Stream
---------------------------------------------------------------------

instance
	( KnownNat status
	, ReflectFlush flush
	, AllStreamCTRender ctypes a
	) => HasServer (Stream 'GET status flush ctypes a) context where

	type ServerT (Stream 'GET status flush ctypes a) m = m (Producer a (SafeT IO) ())

	route Proxy _ action = leafRouter route'
		where
			route' env request respond =
				let
					accHdr = lookup hAccept $ requestHeaders request
					accH = AcceptHeader $ fromMaybe ct_wildcard accHdr
				in runAction action env request respond $ \output -> do
					let
						handleA = handleStreamAcceptH ctypes accH output

					case handleA of
						Nothing -> FailFatal err406
						Just (contentT, body) ->
							Route $ responseStream status hdrs bdy
							where
								hdrs = [(hContentType, contentT)]
								bdy write flush = do
									let
										flushIt = case flushVal of
											Flush -> flush
											NoFlush -> return ()
									runSafeT . runEffect $ for body $ \bs -> do
										liftIO (write $ byteString bs)
										liftIO flushIt


			ctypes = Proxy :: Proxy ctypes
			status = toEnum . fromInteger . natVal $ (Proxy :: Proxy status)
			flushVal = reflectFlush (Proxy :: Proxy flush)

---------------------------------------------------------------------
-- AllStreamMimeRender (Streaming Version of AllMimeRender)
---------------------------------------------------------------------

class (AllMime list) => AllStreamMimeRender (list :: [*]) a where

	allStreamMimeRender
		:: Monad m
		=> Proxy list
		-> Producer a m ()
		-> [(MediaType, Producer ByteString m ())]

instance {-# OVERLAPPABLE #-}
	( Accept ctype
	, StreamMimeRender ctype a
	) => AllStreamMimeRender '[ctype] a where

	allStreamMimeRender Proxy a =
		let
			ctype = Proxy :: Proxy ctype
		in
			[(contentType ctype, mimeStreamRender ctype a)]

instance {-# OVERLAPPABLE #-}
	( StreamMimeRender ctype a
	, AllStreamMimeRender (ctype' ': ctypes) a
	, Accept ctype
	) => AllStreamMimeRender (ctype ': ctype' ': ctypes) a where

	allStreamMimeRender Proxy a =
		let
			ctype = Proxy :: Proxy ctype
			ctypes = Proxy :: Proxy (ctype' ': ctypes)
		in
			(contentType ctype, mimeStreamRender ctype a)
			: allStreamMimeRender ctypes a

---------------------------------------------------------------------
-- AllStreamCTRender (Streaming Version of AllCTRender
---------------------------------------------------------------------

class AllMime list => AllStreamCTRender (list :: [*]) a where

	handleStreamAcceptH
		:: Monad m
		=> Proxy list
		-> AcceptHeader
		-> Producer a m ()
		-> Maybe (ByteString, Producer ByteString m ())


instance {-# OVERLAPPABLE #-}
	( Accept ctype
	, AllMime ctypes
	, AllStreamMimeRender (ctype ': ctypes) a
	) => AllStreamCTRender (ctype ': ctypes) a where

	handleStreamAcceptH Proxy (AcceptHeader accept) p =
		let
			ctypes = Proxy :: Proxy (ctype ': ctypes)
			asmr = allStreamMimeRender ctypes p
			lkup = fmap (\(a,b) -> (a, (renderHeader a, b))) asmr
		in
			mapAcceptMedia lkup accept

---------------------------------------------------------------------
-- HasDocs
---------------------------------------------------------------------

toSampleByteString
	:: (ToSample a , AllStreamMimeRender ct a)
	=> Proxy ct -> Proxy a -> [(Text, MediaType, LBS.ByteString)]
toSampleByteString c p =
	let
		pr = each . fmap snd $ toSamples p
		ar = allStreamMimeRender c pr
		bs = map (\(m, producers) -> (Text.empty, m, toLazy producers)) ar
	in
		bs

instance
	( KnownNat status
	, ReflectMethod method
	, ToSample a
	, AllStreamMimeRender (ct ': cts) a
	) => HasDocs (Stream method status flush (ct ': cts) a) where
    
	docsFor Proxy (endpoint, action) DocOptions{..} = single endpoint' action'
		where
			single e a = API mempty (HashMap.singleton e a)

			endpoint' = endpoint & method .~ method'

			action' =
				action
					& response . respBody .~ take _maxSamples (toSampleByteString t p)
					& response . respTypes .~ allMime t
					& response . respStatus .~ status

			t = Proxy :: Proxy (ct ': cts)

			method' = reflectMethod (Proxy :: Proxy method)

			status = fromInteger . natVal $ (Proxy :: Proxy status)

			p = Proxy :: Proxy a
