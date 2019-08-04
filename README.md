# Haskell Servant and Elm Example

This example project illustrates how to set up a project that:

- Uses haskell and `servant` as a backend exposing a JSON api
  (and serving some files).
- Uses elm to write the frontend.
- Uses `servant-elm` to generate client functions in elm for the JSON api.
  This means that mismatches regarding the JSON api will be detected statically.
- Allows a very fast development cycle: You can type-check the server and
  client code in a very short amount of time.

## Makefile

There's a `Makefile` included with the following targets:

- `setup` -- Set up everything: install ghc and dependencies. (Needs `stack`, `elm`
  and `elm-test`.)
- `build` -- Build the server and the client.
- `server-start` -- Start the server here: <http://localhost:3000/>. Requests sent
  to this server will trigger a recompilation of the client code (not the server
  code).
- `test` -- Recompiles the client and server code. And runs the test-suite.

## Caveats

- This project uses <https://travis-ci.org/soenkehahn/wai-make-assets>, which is
  experimental. E.g. there's no support for serving the assets in a production setting.
