module Main where

import Control.Concurrent.Async (race)
import Control.Exception
import Control.Monad (forever)
import GHC.IO.Encoding
import Network (withSocketsDo, PortID(..), connectTo)
import System.Environment
import System.IO

main :: IO ()
main = withSocketsDo $ do
	args <- getArgs
	case (length args) of
		0 -> do (adr, port) <- return ("127.0.0.1", fromIntegral 60001)
		1 -> do (adr, port) <- return ("127.0.0.1", fromIntegral $ read $ args !! 0)
		_ -> do (adr, port) <- return (args !! 0, fromIntegral $ read $ args !! 0)
	hdl <- connectTo adr port		
	talk hdl `finally` hClose hdl

talk :: Handle -> IO ()
talk hdl = do
	hSetNewlineMode hdl universalNewlineMode
	hSetBuffering hdl NoBuffering
	_ <- race fromServer toServer
	return ()
	where
		fromServer = forever $ do
			line <- hGetLine hdl
			case line of
				"Bye!"	-> do return ()
				_		-> do putStrLn line
		toServer = forever $ do
			line <- (getLine >>= return . (++ [';']))
			hPutStrLn hdl line
