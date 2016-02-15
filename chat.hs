{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Data.Char (toLower)
import           Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception
import Control.Monad
import Network (accept, listenOn, PortID(..))
import System.Environment (getArgs)
import System.IO
import Text.Printf

type ClientName = String

data Client     = Client
								{ clientName            :: ClientName
								, clientHandle          :: Handle
								, clientKicked          :: TVar (Maybe String)
								, clientSendChan        :: TChan Message
								}

data Server     = Server
								{ clients :: TVar (Map ClientName Client)
								}

data Message    = Notice String
								| Tell ClientName String
								| Broadcast ClientName String
								| Command String

main :: IO ()
main = do
		args <- getArgs
		let port = getServ args
		server <- newServer
		sock <- listenOn (PortNumber port)
		printf "Listening on port %s\n" $ show port
		forever $ do
				(hdl, host, _) <- accept sock
				hSetEncoding hdl utf8
				printf "Accepted connection from %s: %s\n" host (show port)
				forkFinally (talk hdl server) (\_ -> hClose hdl)

getServ :: Num a => [String] -> a
getServ [arg] = fromInteger $ read arg
getServ _ = 60000

newClient :: ClientName -> Handle -> STM Client
newClient name hdl = do
		c <- newTChan
		k <- newTVar Nothing
		return Client   { clientName = name
						, clientHandle = hdl
						, clientSendChan = c
						, clientKicked = k
						}

newServer :: IO Server
newServer = do
		c <- newTVarIO Map.empty
		return Server { clients = c }

sendMessage :: Client -> Message -> STM ()
sendMessage Client{..} = writeTChan clientSendChan

broadcast :: Server ->  Message -> STM ()
broadcast server = broadcast' server Nothing

broadcast' :: Server -> Maybe (Either ClientName ClientName) -> Message -> STM ()
broadcast' Server{..} clt msg = do
		clientmap <- readTVar clients
		mapM_ (`sendMessage` msg) (Map.elems $ someFilt clt clientmap)
		where
				someFilt Nothing b = b
				someFilt (Just (Left a)) b = Map.filterWithKey (\k _ -> k /= map toLower a) b
				someFilt (Just (Right a)) b = Map.filterWithKey (\k _ -> k == map toLower a) b

broadcastWithout :: Server -> ClientName -> Message -> STM ()
broadcastWithout server name = broadcast' server (Just (Left name))

broadcastTo :: Server -> ClientName -> Message -> STM ()
broadcastTo server name = broadcast' server (Just (Right name))

checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name hdl = atomically $ do
		clientmap <- readTVar clients
		if Map.member (map toLower name) clientmap
				then return Nothing
				else do
					client <- newClient name hdl
					writeTVar clients $ Map.insert (map toLower name) client clientmap
					broadcast server $ Notice (name ++ " has connected")
					return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
		modifyTVar' clients $ Map.delete name
		broadcast server $ Notice (name ++ " has disconnected")

talk :: Handle -> Server -> IO ()
talk hdl server@Server{..} = do
	hSetNewlineMode hdl universalNewlineMode
	hSetBuffering hdl LineBuffering
	readName
	where
	readName = do
		hPrintf hdl "What is your name?\n"
		name <- hGetLine hdl
		if null name
			then readName
			else mask $ \restore -> do
				ok <- checkAddClient server name hdl
				case ok of
					Nothing -> restore $ do --
						hPrintf hdl
								"The name %s is in use, please choose another\n" name
						readName
					Just client ->
						restore (runClient server client) --
						`finally` removeClient server name

runClient :: Server -> Client -> IO ()
runClient serv@Server{..} client@Client{..} = do
	hPrintf clientHandle helpMsg
	_ <- race server receive
	return ()
 where
	receive = forever $ do
		msg <- hGetLine clientHandle
		atomically $ sendMessage client (Command msg)

	server = join $ atomically $ do
		k <- readTVar clientKicked
		case k of
			Just reason -> return $
				hPrintf clientHandle "You have been kicked: %s\n" reason
			Nothing -> do
				msg <- readTChan clientSendChan
				return $ do
					continue <- handleMessage serv client msg
					when continue server

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server Client{..} message =
	case message of
		Notice msg -> output $ "# " ++ msg
		Tell name msg -> output $ "*" ++ name ++ "*: " ++ msg
		Broadcast name msg -> output $ "<" ++ name ++ ">: " ++ msg
		Command msg ->
			case words msg of
				{-["/kick", who] -> do
				atomically $ kick server who clientName
				return True-}
				[] ->
					return True
				["/help"] -> do
					hPrintf clientHandle helpMsg
					return True
				"/tell" : who : what -> do
					atomically $ broadcastTo server who $ Tell clientName (unwords what)
					return True
				["/quit"] ->
					return False
				('/':_):_ -> do
					hPrintf clientHandle "Unrecognized command: %s\n" msg
					return True
				_ -> do
					atomically $ broadcast server $ Broadcast clientName msg
					return True
 where
	output s = do hPutStrLn clientHandle s; return True

helpMsg :: String
helpMsg =  unlines
			[ l1
			, l2
			, "\"/tell nick some message\" - send private message to nick (user name)"
			, "    example: /tell razorr Hello!"
			, "\"/help\" - show this help"
			, "\"/quit\" - to exit"
			, l2
			,l1
			]
			where
				l1 = "#---------------------"
				l2 = ""
