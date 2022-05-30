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
    # :completion vertico
    (ripgrep.override { withPCRE2 = true; })
    sqlite
    # :lang org
    tectonic
    ## +gnuplot
    gnuplot
    ## +pandoc
    pandoc
    ## +jupyter
    (python39.withPackages(ps: with ps; [jupyter]))
    # :checkers spell
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    sdcv
    # :checkers grammar
    languagetool
    # :app mu4e
    ## mu
    ## isync
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
