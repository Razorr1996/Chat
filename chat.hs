{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import           Data.Map (Map)
import qualified Data.Map as Map
import GHC.IO.Encoding
import Network
import System.Environment
import System.IO
import Text.Printf

type ClientName = String

data Client 	= Client
				{ clientName 		:: ClientName
				, clientHandle 		:: Handle
				, clientKicked 		:: TVar (Maybe String)
				, clientSendChan 	:: TChan Message
				}

data Server 	= Server
				{ clients :: TVar (Map ClientName Client)
				}

data Message 	= Notice String
				| Tell ClientName String
				| Broadcast ClientName String
				| Command String


main :: IO ()
main = do
	args <- getArgs
	port <- return $ fromInteger 60000
	server <- newServer
	sock <- listenOn (PortNumber port)
	printf "Listening on port %s\n" $ show port
	forever $ do
		(handle, host, port) <- accept sock
		hSetEncoding handle utf8
		printf "Accepted connection from %s: %s\n" host (show port)
		forkFinally (talk handle server) (\_ -> hClose handle)

getServ :: Num a => [String] -> a
getServ [arg] = fromInteger $ read arg
getServ _ = fromInteger 60000

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
	c <- newTChan
	k <- newTVar Nothing
	return Client	{ clientName = name
					, clientHandle = handle
					, clientSendChan = c
					, clientKicked = k
					}

newServer :: IO Server
newServer = do
	c <- newTVarIO Map.empty
	return Server { clients = c }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} msg = writeTChan clientSendChan msg

broadcast :: Server ->  Message -> STM ()
broadcast server msg = broadcast' server Nothing msg

broadcast' :: Server -> Maybe (Either ClientName ClientName) -> Message -> STM ()
broadcast' Server{..} clt msg = do
	clientmap <- readTVar clients
	mapM_ (\client -> sendMessage client msg) (Map.elems $ someFilt clt clientmap)
 	where
		someFilt Nothing b = b
		someFilt (Just (Left a)) b = Map.filterWithKey (\k _ -> k /= a) b
		someFilt (Just (Right a)) b = Map.filterWithKey (\k _ -> k == a) b

broadcastWithout :: Server -> ClientName -> Message -> STM ()
broadcastWithout server name msg = broadcast' server (Just (Left name)) msg

broadcastTo :: Server -> ClientName -> Message -> STM ()
broadcastTo server name msg = broadcast' server (Just (Right name)) msg

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
	clientmap <- readTVar clients
	if Map.member name clientmap
		then return Nothing
		else do client <- newClient name handle
			writeTVar clients $ Map.insert name client clientmap
			broadcast server $ Notice (name ++ " has connected")
			return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
	modifyTVar' clients $ Map.delete name
	broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk handle server@Server{..} = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
  readName
 where
	readName = do
		hPutStrLn handle "What is your name?"
		name <- hGetLine handle
		if null name
			then readName
			else mask $ \restore -> do
				ok <- checkAddClient server name handle
				case ok of
					Nothing -> restore $ do --
						hPrintf handle
							"The name %s is in use, please choose another\n" name
						readName
					Just client ->
						restore (runClient server client) --
						`finally` removeClient server name

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
	race server receive
	return ()
 where
	receive = forever $ do
		msg <- hGetLine clientHandle
		atomically $ sendMessage client (Command msg)

	server = join $ atomically $ do
		k <- readTVar clientKicked
		case k of
			Just reason -> return $
				hPutStrLn clientHandle $ "You have been kicked: " ++ reason
			Nothing -> do
				msg <- readTChan clientSendChan
				return $ do
					continue <- handleMessage serv client msg
					when continue $ server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client{..} message =
	case message of
		Notice msg -> output $ "# " ++ msg
		Tell name msg -> output $ "*" ++ name ++ "*: " ++ msg
		Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
		Command msg ->
			case words msg of
				{-["/kick", who] -> do
					atomically $ kick server who clientName
					return True-}
				"/tell" : who : what -> do
					atomically $ broadcastTo server who $ Tell clientName (unwords what)
					return True
				["/quit"] ->
					return False
				('/':_):_ -> do
					hPutStrLn clientHandle $ "Unrecognized command: " ++ msg
					return True
				_ -> do
					atomically $ broadcast server $ Broadcast clientName msg
					return True
 where
	output s = do hPutStrLn clientHandle s; return True