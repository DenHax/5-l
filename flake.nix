{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;

          config = {
            allowUnfree = false;
          };
        };
        R-with-my-packages = pkgs.rWrapper.override {
          packages = with pkgs.rPackages; [
            ggplot2
            dplyr
            xts
            shiny
            DT
            jsonlite
            igraph
          ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.yazi
            pkgs.lynx
            pkgs.tmux
            pkgs.htop

            pkgs.git
            pkgs.curl
            pkgs.wget
            pkgs.jq

            pkgs.neovim
            pkgs.ripgrep
            pkgs.fd
            pkgs.gat
            pkgs.fzf

            pkgs.disko

            R-with-my-packages
          ];
        };
      }
    );
}
