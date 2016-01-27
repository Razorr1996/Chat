all: server client clean
server:
	haskell-compiler myServer.hs
client:
	haskell-compiler myClient.hs
clean:
	rm -rf *.o *.hi
