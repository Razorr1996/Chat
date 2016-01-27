module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Fix (fix)
import GHC.IO.Encoding
import Network.Socket
import System.Environment
import System.IO

type Msg = (Int, String)

main :: IO ()
main = withSocketsDo $ do
	args <- getArgs
	port <- return $ getServ args
	chan <- atomically $ newTChan
	sock <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr 1
	bindSocket sock (SockAddrInet port iNADDR_ANY)
	listen sock 2
	forkIO $ fix $ \loop -> do
		(_, msg) <- atomically $ readTChan chan
		loop
	mainLoop sock chan 0
	
mainLoop :: Socket -> TChan Msg -> Int -> IO ()
mainLoop sock chan nr = do
	conn <- accept sock
	forkIO (runConn conn chan nr)
	mainLoop sock chan $! nr+1

runConn :: (Socket, SockAddr) -> TChan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
	let broadcast msg = atomically $ writeTChan chan (nr, msg)
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl NoBuffering
	hPutStrLn hdl "Hi, what's your name?"
	name <- liftM init (hGetLine hdl)
	broadcast ("--> " ++ name ++ " entered.")
	hPutStrLn hdl ("Welcome, " ++ name ++ "!")
	chan' <- atomically $ dupTChan chan
	reader <- forkIO $ fix $ \loop -> do
		(nr', line) <- atomically $ readTChan chan'
		when (nr /= nr') $ hPutStrLn hdl line
		loop
	handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
		line <- liftM init (hGetLine hdl)
		case line of
			":q"	-> hPutStrLn hdl "Bye!"
			_		-> do
				broadcast (name ++ ": " ++ line)
				loop
	killThread reader
	broadcast ("<-- " ++ name ++ " left.")
	hClose hdl

getServ (arg : []) = fromInteger $ read arg
getServ _ = fromInteger 60000
