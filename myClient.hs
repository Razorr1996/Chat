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
	(adr, port) <- return $ getServ args
	hdl <- connectTo adr $ PortNumber port
	talk hdl `finally` hClose hdl

talk :: Handle -> IO ()
talk hdl = do
	hSetNewlineMode hdl universalNewlineMode
	hSetBuffering hdl LineBuffering
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

getServ (arg1 : arg2 : []) = (arg1, fromInteger $ read arg2)
getServ (arg1 : []) = ("127.0.0.1", fromInteger $ read arg1)
getServ _ = ("127.0.0.1", fromInteger 60000)
