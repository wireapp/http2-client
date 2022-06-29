{-# LANGUAGE OverloadedStrings #-}

import Data.Default.Class (def)
import Network.HTTP2.Client
import Network.HTTP2.Client.Helpers
import Network.HTTP2
import Test.Hspec
import Network.TLS as TLS
import Network.TLS.Extra.Cipher as TLS

main :: IO ()
main = hspec $ do
    describe "FrameConnection" $ do
        it "can make a FrameConnection" $ do
            eitherFrameConn <- runClientIO $ newHttp2FrameConnection "www.siefkes.net" 443 (Just tlsParams)
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
            tlsParams :: ClientParams
            tlsParams = TLS.ClientParams {
                TLS.clientWantSessionResume    = Nothing
              , TLS.clientUseMaxFragmentLength = Nothing
              , TLS.clientServerIdentification = ("127.0.0.1", "")
              , TLS.clientUseServerNameIndication = True
              , TLS.clientShared               = def
              , TLS.clientHooks                = def { TLS.onServerCertificate = \_ _ _ _ -> return [] }
              , TLS.clientSupported            = def { TLS.supportedCiphers = TLS.ciphersuite_default }
              , TLS.clientDebug                = def
              }
