../assets/index.html: Api.elm Main.elm
	elm make Main.elm
	mv index.html ../assets/

Api.elm: GenerateElm.hs ../server/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver client/GenerateElm.hs > client/Api.elm)
