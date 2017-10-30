../assets/index.html: Api.elm Main.elm
	elm make --yes Main.elm
	mv index.html ../assets/

Api.elm: GenerateElm.hs ../server/src/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver/src client/GenerateElm.hs)

run-tests: Api.elm
	elm-test
