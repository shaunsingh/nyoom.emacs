{ pkgs ? import <nixpkgs> {
  overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
} }:
with pkgs;
mkShell {
  buildInputs = [
    # Emacs deps
    ((emacsPackagesNgGen emacsGcc).emacsWithPackages (epkgs: [ epkgs.vterm ]))
    (ripgrep.override { withPCRE2 = true; })
    sqlite
    gnuplot
    # pandoc
    # sdcv
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    (texlive.combine {
      inherit (texlive)
        scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox
        amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra
        cleveref latexmk tcolorbox environ arev amsfonts simplekv alegreya
        sourcecodepro newpx svg catchfile transparent hanging biblatex
        biblatex-mla;
    })
    # languagetool
    nixfmt
    fd
    # sassc
  ];
  shellHook = ''
    if [ ! -d $HOME/.config/emacs/.git ]; then
      mkdir -p $HOME/.config/emacs
      git -C $HOME/.config/emacs init
      $HOME/.config/emacs/bin/doom sync || true
      YES=1 FORCE=1 $HOME/.config/emacs/bin/doom sync -u
    fi
    if [ $(git -C $HOME/.config/emacs rev-parse HEAD) != ${pkgs.doomEmacsRevision} ]; then
      git -C $HOME/.config/emacs fetch https://github.com/hlissner/doom-emacs.git || true
      git -C $HOME/.config/emacs checkout ${pkgs.doomEmacsRevision} || true
    fi
  '';
}
