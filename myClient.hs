module Main where

import Control.Concurrent.Async (race)
import Control.Exception
import Control.Monad (forever)
import Data.String.Unicode
import GHC.IO.Encoding
import Network (withSocketsDo, PortID(..), connectTo)
import System.Environment
import System.IO

main :: IO ()
main = withSocketsDo $ do
	args <- getArgs
	(adr, port) <- return $ getServ args
	hdl <- connectTo adr $ PortNumber port
	hSetEncoding hdl utf8
	talk hdl `finally` hClose hdl

talk :: Handle -> IO ()
talk hdl = do
	hSetNewlineMode hdl universalNewlineMode
	hSetBuffering hdl LineBuffering
	race fromServer toServer
	return ()
	where
		fromServer = do
			eof <- hIsEOF hdl
			if eof
				then do
					putStrLn "Connection closed by foreign host."
					return ()
				else do
					line <- hGetLine hdl
					putStrLn line
					fromServer
		toServer = forever $ do
			line <- getLine
			hPutStrLn hdl line

getServ :: Num a => [String] -> (String, a)
getServ (arg1 : [arg2]) = (arg1, fromInteger $ read arg2)
getServ [arg1] = ("127.0.0.1", fromInteger $ read arg1)
getServ _ = ("127.0.0.1", fromInteger 60000)
