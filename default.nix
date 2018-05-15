(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    # common = ./common;
    # backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["frontend"]; # ["common" "backend" "frontend"];
    ghcjs = ["frontend"]; # ["common" "frontend"];
  };

  android.frontend = {
    executableName = "frontend-exe";
    applicationId = "org.givorsdays.android";
    displayName = "Givors Days";
  };
})
