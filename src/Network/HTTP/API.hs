{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Utility code intended to simplify writing bindings to JSON APIs.
module Network.HTTP.API where
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text.Encoding
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit

-- | A catch-all error type for common request failures.
data APIError = InvalidJSON | ExceptionalStatusCodeError String
  deriving (Show)

-- | RequestMiddleware allows requests to be customized before being sent.
--
-- This is particularly useful for dealing with APIs that require some form
-- of custom authenticaton or for attaching additional headers to requests.
type RequestMiddleware = Request (ResourceT IO) -> Request (ResourceT IO)

-- | Execute the API client
runAPIClient :: String -> RequestMiddleware -> APIClient a -> IO (Either APIError a)
runAPIClient base middleware m = withManager $ \man -> do
  r <- parseUrl base
  runEitherT $ runReaderT (fromAPIClient m) $ ClientSettings r man middleware

-- | Attempt to decode a response into the expected JSON type or fail with
-- an "InvalidJSON" response.
jsonize :: (FromJSON a) => Response L.ByteString -> APIClient (Response a)
jsonize r = APIClient $ case decode $ responseBody r of
    Nothing -> lift $ left InvalidJSON
    Just jsonResp -> return $ r { responseBody = jsonResp }

-- | The basic settings for the client to run each request with.
data ClientSettings = ClientSettings
  { baseRequest :: Request (ResourceT IO) -- ^ A request with the base URL and any other constants set.
  , clientManager :: Manager -- ^ Manager to share across all client requests
  , requestMiddleware :: RequestMiddleware -- ^ Any additional transformations that should be applied prior to sending a request.
  }

-- | The Base API client type. Intended to be wrapped in a newtype for libraries that use it.
newtype APIClient a = APIClient { fromAPIClient :: ReaderT ClientSettings (EitherT APIError (ResourceT IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Perform a GET on the given path, decoding the response from JSON.
get :: FromJSON a => ByteString -> APIClient (Response a)
get p = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { path = p }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

-- | Perform a PUT on the given path, encoding the request into JSON and decoding the response from JSON.
put :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
put p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "PUT", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

-- | Perform a POST on the given path, encoding the request into JSON and decoding the response from JSON.
post :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
post p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "POST", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

-- | Perform a PATCH on the given path, encoding the request into JSON and decoding the response from JSON.
patch :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
patch p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "PATCH", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

-- | Perform a DELETE on the given path, encoding the request into JSON and decoding the response from JSON.
--
-- An input value is often unnecessary for DELETEs, but most APIs seem to accept () as an input.
-- Future versions of this library will likely do away with the input value and make a second function
-- for deleting with an body value.
delete :: (ToJSON a, FromJSON b) => ByteString -> a -> APIClient (Response b)
delete p v = APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { method = "DELETE", path = p, requestBody = RequestBodyLBS $ encode v }
  resp <- lift $ lift $ httpLbs r man
  fromAPIClient $ jsonize resp

