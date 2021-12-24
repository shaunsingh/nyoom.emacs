{ pkgs ? import <nixpkgs> {
  overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/shaunsingh/emacs/archive/master.tar.gz";
    }))
  ];
} }:
with pkgs;
mkShell {
  buildInputs = [
    # Emacs deps
    emacs
    (ripgrep.override { withPCRE2 = true; })
    sqlite
    gnuplot
    # pandoc
    # sdcv
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    tectonic
    # languagetool
    nixfmt
    fd
    sassc
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
