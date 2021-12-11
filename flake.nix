# [[file:config.org::*Notes for the unwary adventurer][Notes for the unwary adventurer:1]]
{
  description = "Shuarya Singh's Doom-emacs config";

  nixConfig.extra-substituters = "https://nix-community.cachix.org";
  nixConfig.extra-trusted-public-keys =
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "unstable";
    };
    emacs = {
      url = "github:shaunsingh/emacs";
      inputs.nixpkgs.follows = "unstable";
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
# Notes for the unwary adventurer:1 ends here
