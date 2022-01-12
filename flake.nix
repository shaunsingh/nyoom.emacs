{
  description = "Shuarya Singh's Doom-emacs config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, emacs, doom-emacs }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;
      name = "doom-emacs";
      preOverlays = [
        emacs.overlay
        (final: prev: { doomEmacsRevision = doom-emacs.rev; })
      ];
      shell = ./shell.nix;
      systems = [ "aarch64-darwin" ];
    };
}
