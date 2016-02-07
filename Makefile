all: server client chat clean
server:
	haskell-compiler myServer.hs
client:
	haskell-compiler myClient.hs
chat:
	haskell-compiler chat.hs
clean:
	rm -rf *.o *.hi
