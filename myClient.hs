module Main where

import Control.Concurrent.Async (race)
import Control.Monad.Trans (liftIO)
import Network (PortID(..), connectTo)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
	args <- getArgs
	(adr, port) <- return $ getServ args
	hdl <- connectTo adr $ PortNumber port
	hSetEncoding hdl utf8
	talk hdl `finally` hClose hdl

talk :: Handle -> IO ()
talk hdl = do
	hSetNewlineMode hdl universalNewlineMode
	hSetBuffering hdl NoBuffering
	_ <- race fromServer (runInputT defaultSettings toServer)
	return ()
	where 
		toServer ::  InputT IO ()
		toServer = do
			minput <- getInputLine ""
			case minput of
				Just "" -> toServer
				Just input -> do
					liftIO $ hPutStrLn hdl input
					toServer
				_ -> toServer
		fromServer :: IO ()
		fromServer = do
			eof <- hIsEOF hdl
			if eof
				then do
					putStrLn "Connection closed by Chat server."
					return ()
				else do
					line <- hGetLine hdl
					putStrLn line
					fromServer

getServ :: Num a => [String] -> (String, a)
getServ (arg1 : [arg2]) = (arg1, fromInteger $ read arg2)
getServ [arg1] = ("127.0.0.1", fromInteger $ read arg1)
getServ _ = ("127.0.0.1", 60000)
