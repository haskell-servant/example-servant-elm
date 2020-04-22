../assets/index.html: src/Api.elm src/Main.elm
	elm make src/Main.elm
	mv index.html ../assets/

src/Api.elm: GenerateElm.hs ../server/src/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver/src client/GenerateElm.hs)

run-tests: src/Api.elm
	elm-test

