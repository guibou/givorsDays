{uri ? "http://localhost:8000"}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    # common = ./common;
    # backend = ./backend;
    frontend = ./frontend;
  };

  overrides = self: super: {
    frontend = pkgs.haskell.lib.overrideCabal super.frontend (old : {
      configureFlags = old.configureFlags ++ ["--ghc-option=-DAPP_URI=\"${uri}\""];
      });
    };

  shells = {
    ghc = ["frontend"]; # ["common" "backend" "frontend"];
    ghcjs = ["frontend"]; # ["common" "frontend"];
  };

  android.frontend = {
    executableName = "frontend-exe";
    applicationId = "org.example.frontend";
    displayName = "Givors Days";
  };
})
