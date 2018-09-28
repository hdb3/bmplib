{-# LANGUAGE RecordWildCards #-}
module BMPMessage where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Base16
import Data.IP
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Monoid((<>))
import Control.Monad(when,unless)
import Control.Applicative((<|>))
import Data.Attoparsec.ByteString -- from package attoparsec
import qualified Data.Attoparsec.ByteString as DAB
import Data.Attoparsec.Binary -- from package attoparsec-binary

{-
3.1.  BMP Messages

   The following are the messages provided by BMP:

   o  Route Monitoring (RM): Used to provide an initial dump of all
      routes received from a peer, as well as an ongoing mechanism that
      sends the incremental routes advertised and withdrawn by a peer to
      the monitoring station.

   o  Peer Down Notification: A message sent to indicate that a peering
      session has gone down with information indicating the reason for
      the session disconnect.

   o  Stats Reports (SR): An ongoing dump of statistics that can be used
      by the monitoring station as a high-level indication of the
      activity going on in the router.

   o  Peer Up Notification: A message sent to indicate that a peering
      session has come up.  The message includes information regarding
      the data exchanged between the peers in their OPEN messages, as
      well as information about the peering TCP session itself.  In
      addition to being sent whenever a peer transitions to the
      Established state, a Peer Up Notification is sent for each peer in
      the Established state when the BMP session itself comes up.

   o  Initiation: A means for the monitored router to inform the
      monitoring station of its vendor, software version, and so on.

   o  Termination: A means for the monitored router to inform the
      monitoring station of why it is closing a BMP session.

   o  Route Mirroring: A means for the monitored router to send verbatim
      duplicates of messages as received.  Can be used to exactly mirror
      a monitored BGP session.  Can also be used to report malformed BGP
      Protocol Data Units (PDUs)
-}

data BMPMsg = BMPRouteMonitoring PerPeerHeader | BMPPeerDown | BMPStatsReport | BMPPeerUP | BMPInitiation PerPeerHeader | BMPTermination | BMPRouteMirroring deriving Show

data PerPeerHeader = PerPeerHeader {
                                     pphType :: Word8
                                   , pphFlags :: Word8
                                   , pphDistinguisher :: BS.ByteString
                                   , pphAddress :: IP
                                   , pphAS :: Word32
                                   , pphBGPID :: IPv4
                                   , pphTimeStampSecs :: Word32
                                   , pphTimeStampMicroSecs :: Word32
                                   , vFlag, lFlag, aFlag :: Bool
                                   } deriving Show
getPerPeerHeader = do
    pphType <- anyWord8
    pphFlags <- anyWord8
    let vFlag = testBit pphFlags 7  
        lFlag = testBit pphFlags 6  
        aFlag = testBit pphFlags 5  
    pphDistinguisher <- DAB.take 8
    pphAddress <- if vFlag then ipIPv6 else DAB.take 12 >> ipIPv4
    pphAS <- anyWord32be
    pphBGPID <- zIPv4
    pphTimeStampSecs <- anyWord32be
    pphTimeStampMicroSecs <- anyWord32be
    return PerPeerHeader{..}


atto p s = ( p' <|> return Nothing ) <?> s where
    p' = do tmp <- p
            return (Just tmp)
     
bmpMessageParser :: Parser (Maybe BMPMsg)
bmpMessageParser = atto bmpMessageParser' "BMP payload parser"
-- bmpMessageParser = ( bmpMessageParser' <|> return Nothing ) <?> "BMP wire format parser"

bmpMessageParser' :: Parser BMPMsg
bmpMessageParser' = do
    msgType <- anyWord8
    when (msgType > 6 ) ( fail "invalid message type")
    case msgType of 
        0 -> getBMPRouteMonitoring
        1 -> return BMPPeerDown
        2 -> return BMPStatsReport
        3 -> return BMPPeerUP
        4 -> getBMPInitiation
        5 -> return BMPTermination
        6 -> return BMPRouteMirroring

getBMPRouteMonitoring = do
    perPeerHeader <- getPerPeerHeader
    return $ BMPRouteMonitoring perPeerHeader

getBMPInitiation = do
    perPeerHeader <- getPerPeerHeader
    return $ BMPInitiation perPeerHeader

bmpParser :: Parser (Maybe BMPMsg)
bmpParser = ( do msg <- bsParser
                 return $ maybeResult $ parse bmpMessageParser' msg
            ) <|> return Nothing


bsParser :: Parser BS.ByteString
bsParser = do
    word8 0x03
    msgLen <- anyWord32be
    when (msgLen < 5 || msgLen > 0xffff) ( fail "invalid message length")
    DAB.take (fromIntegral msgLen - 5)

{-
4.  BMP Message Format

4.1.  Common Header

   The following common header appears in all BMP messages.  The rest of
   the data in a BMP message is dependent on the Message Type field in
   the common header.

      0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     +-+-+-+-+-+-+-+-+
     |    Version    |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |                        Message Length                         |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |   Msg. Type   |
     +---------------+

   o  Version (1 byte): Indicates the BMP version.  This is set to '3'
      for all messages defined in this specification. ('1' and '2' were
      used by draft versions of this document.)  Version 0 is reserved
      and MUST NOT be sent.

   o  Message Length (4 bytes): Length of the message in bytes
      (including headers, data, and encapsulated messages, if any).

   o  Message Type (1 byte): This identifies the type of the BMP
      message.  A BMP implementation MUST ignore unrecognized message
      types upon receipt.

      *  Type = 0: Route Monitoring
      *  Type = 1: Statistics Report
      *  Type = 2: Peer Down Notification
      *  Type = 3: Peer Up Notification
      *  Type = 4: Initiation Message
      *  Type = 5: Termination Message
      *  Type = 6: Route Mirroring Message
-}

{-
data BMPMessageRaw = BMPMessageRaw { msgType :: Word8 , payload :: HexByteString } deriving Show
instance Binary BMPMessageRaw where
    get = label "BMPMessageRaw" $ do
        version <- getWord8
        unless (version == 0x03) (fail $ "BMPMessageRaw: incorrect version - expected 3 got " ++ show version) -- version hardcoded
        msgLength <- getWord32be             
        msgType <- getWord8
        payload <- getHexByteString (fromIntegral msgLength -6)
        return BMPMessageRaw {..}
    put BMPMessageRaw {..} = putWord8 0x03 <> putWord32be (fromIntegral $ 6 + hbLength payload) <> putWord8 msgType <> put payload 

-- HexByteString exists in order to implement a useful instance of 'show'
newtype HexByteString = HexByteString BS.ByteString deriving (Eq,Read)
instance Show HexByteString where
    show (HexByteString bs) = let toHex = Data.ByteString.Char8.unpack . Data.ByteString.Base16.encode
                              in toHex bs

hbLength (HexByteString hb) = BS.length hb

-- getHexByteString n = fmap HexByteString . getByteString n 
getHexByteString n = do bs <- getByteString n
                        return ( HexByteString bs)

instance Binary HexByteString where
    get = undefined
    put (HexByteString bs) = putByteString bs
-}

-- AttoParsec parsers for IOStream adapters

newtype BMPMessageRaw = BMPMessageRaw BS.ByteString
extract (BMPMessageRaw bs) = bs

instance Binary BMPMessageRaw where
    get = label "BMPMessageRaw" $ do
        version <- getWord8
        unless (version == 0x03) (fail $ "BMPMessageRaw: incorrect version - expected 3 got " ++ show version) -- version hardcoded
        msgLength <- getWord32be             
        payload <- getByteString (fromIntegral msgLength -5)
        return $ BMPMessageRaw payload
    put (BMPMessageRaw payload) = putWord8 0x03 <> putWord32be (fromIntegral $ 5 + BS.length payload) <> put payload 

rawBMPMessageParser :: Parser (Maybe BMPMessageRaw)
rawBMPMessageParser = atto rawBMPMessageParser' "BMP wire format parser"
-- rawBMPMessageParser = ( rawBMPMessageParser' <|> return Nothing ) <?> "BMP wire format parser"

rawBMPMessageParser' :: Parser BMPMessageRaw
rawBMPMessageParser' = do
-- TODO cal back to bsParse instead
    word8 0x03
    msgLen <- anyWord32be
    when (msgLen < 5 || msgLen > 0xffff) ( fail "invalid message length")
    payload <- DAB.take (fromIntegral msgLen - 5)
    return $ BMPMessageRaw payload


-- copied from zserv - maybe need some common library for this stuff?

zIPv4 = zIPv4Parser
zIPv4Parser :: Parser IPv4
zIPv4Parser = do
    v4address <- anyWord32le
    return $ fromHostAddress v4address

ipIPv4 :: Parser IP
ipIPv4 = fmap IPv4 zIPv4

ipIPv6 :: Parser IP
ipIPv6 = fmap IPv6 ipIPv6Parser
ipIPv6Parser = do
    v6address <- DAB.take 16
    return $ (toIPv6b . map fromIntegral . BS.unpack) v6address

