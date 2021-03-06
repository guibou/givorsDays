(import ./reflex-platform {}).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    # common = ./common;
    # backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["frontend"]; # ["common" "backend" "frontend"];
    ghc8_2_1 = ["frontend"]; # ["common" "backend" "frontend"];
    ghcjs = ["frontend"]; # ["common" "frontend"];
  };

  android.frontend = {
    executableName = "frontend-exe";
    applicationId = "org.givorsdays.android";
    displayName = "Givors Days";
  };
})
