This is a small calendar application to count my wife (half-)day of work.

Each day can contain up to four half-day of work (yes, that's weird).

Persistance of the calendar is ensured through webstorage, the file can be exported.

## JSaddle work (fast iterations)

```shell
nix-shell -A shells.ghc
cd ./frontend

cabal new-repl
:l GHCLauncher
run 8080
```

- Open your browser to "http://localhost:8080"

## Android application

```shell
nix-build -o res -A android.frontend
```

You will get your APK inside `res`.

# screenshot

![frontent](screenshot.png)
