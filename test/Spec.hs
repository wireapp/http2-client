{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP2.Client
import Network.HTTP2.Client.Helpers
import Network.HTTP2
import qualified OpenSSL.Session as SSL
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "FrameConnection" $ do
        it "can make a FrameConnection" $ do
            sslContext <- makeSSLContext defaultOpenSSLSettings
            eitherFrameConn <- runClientIO $ newHttp2FrameConnection "wire.com" 443 (Just sslContext)
            frameConn <- extractFrameConn eitherFrameConn
            runClientIO $ runHttp2Client frameConn 8192 8192 [(SettingsInitialWindowSize,10000000)] defaultGoAwayHandler ignoreFallbackHandler $ \conn -> do
                let fc = _incomingFlowControl conn
                lift $ _addCredit fc 10000000
                _ <- _updateWindow fc
                let requestHeaders = [ (":method", "GET")
                                     , (":scheme", "https")
                                     , (":path", "/en/about/")
                                     , (":authority", "wire.com")
                                     ]
                withHttp2Stream conn $ \stream ->
                  let
                    initStream = headers stream requestHeaders (setEndHeader . setEndStream)
                    resetPushPromises _ pps _ _ _ = _rst pps RefusedStream
                    handler sfc _ = do
                        streamRes <- waitStream conn stream sfc resetPushPromises
                        lift $ print $ fromStreamResult streamRes
                  in
                    StreamDefinition initStream handler
                _goaway conn NoError "test complete"
            putStrLn "OK"
          where
            extractFrameConn :: Either ClientError Http2FrameConnection -> IO Http2FrameConnection
            extractFrameConn (Right conn) = return conn
            extractFrameConn (Left clientErr) = fail $ show clientErr
