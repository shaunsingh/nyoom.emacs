# [[file:config.org::*Notes for the unwary adventurer][Notes for the unwary adventurer:2]]
{ pkgs ? import <nixpkgs> {
  overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
} }:
with pkgs;
mkShell {
  buildInputs = [
    # use emacs29
    emacsGit
    # we need ripgrep build with pcre lookheads
    (ripgrep.override { withPCRE2 = true; })
    # new emacs needs sqlite
    sqlite
    # General org-mode config deps
    gnuplot
    pandoc
    sdcv
    languagetool
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    tectonic
    # used for formatting nix files
    nixfmt
    # used for compiling the css files
    sassc
    # required by +jupyter
    (python39.withPackages(ps: with ps; [jupyter]))
    # mu4e
    mu
    isync
  ];
  shellHook = ''
    if [ ! -d $HOME/.config/emacs/.git ]; then
      mkdir -p $HOME/.config/emacs
      git -C $HOME/.config/emacs init
    fi
    if [ $(git -C $HOME/.config/emacs rev-parse HEAD) != ${pkgs.doomEmacsRevision} ]; then
      git -C $HOME/.config/emacs fetch https://github.com/hlissner/doom-emacs.git || true
      git -C $HOME/.config/emacs checkout ${pkgs.doomEmacsRevision} || true
    fi
  '';
}
# Notes for the unwary adventurer:2 ends here
