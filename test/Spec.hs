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
            -- This set a specific cipher for debugging, however, certain TLS1.3 ciphersuites
            -- cannot be disabled
            --let osslSettings = defaultOpenSSLSettings
            --sslContext <- makeSSLContext osslSettings { osslSettingsCiphers = "ECDHE-RSA-AES128-GCM-SHA256" }

            sslContext <- makeSSLContext defaultOpenSSLSettings
            -- Load a specific private key to allow monitoring the connection in Wireshark
            -- (but it still seems unable to decode)
            --SSL.contextSetPrivateKeyFile sslContext "osslkey.pem"
            eitherFrameConn <- runClientIO $ newHttp2FrameConnection "www.siefkes.net" 443 (Just sslContext)
            frameConn <- extractFrameConn eitherFrameConn
            runClientIO $ runHttp2Client frameConn 8192 8192 [(SettingsInitialWindowSize,10000000)] defaultGoAwayHandler ignoreFallbackHandler $ \conn -> do
                let fc = _incomingFlowControl conn
                lift $ _addCredit fc 10000000
                _ <- _updateWindow fc
                let requestHeaders = [ (":method", "GET")
                                     , (":scheme", "https")
                                     , (":path", "/contact/")
                                     , (":authority", "www.siefkes.net")
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
