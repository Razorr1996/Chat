all: server client chat1 clean
server:
	haskell-compiler myServer.hs
client:
	haskell-compiler myClient.hs
chat1:
	haskell-compiler chat.hs
clean:
	rm -rf *.o *.hi
