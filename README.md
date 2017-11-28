# backend

`stack build`
`stack exec backend-exe -- database port`

- port is the port of the listening service, working on the plaintext database file.

# frontend

## JSaddle work (fast iterations)

`nix-shell -A shells.ghc`
`cd ./frontend`
`cabal new-repl`
`:set -XOverloadedStrings`
`run 8080 http://localhost:9000`

- 9000 is the port of the running backend
- Open your browser to "http://localhost:8080"

## Android application

`nix-build -o res -A android.frontend --argstr uri "http://localhost:9000"`

Change the uri to your backend uri and get your APK inside `res`.

# screenshot

![frontent](screenshot.png)
