;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! doct)
(package! citar)
(package! citeproc)
(package! org-appear)
(package! org-roam-ui)
(package! org-latex-impatient)
(package! org-latex-impatient
  :recipe (:host github
           :repo "shaunsingh/org-latex-impatient"))
(package! org-cite-csl-activate
  :recipe (:host github
           :repo "andras-simonyi/org-cite-csl-activate"))
(package! org-pandoc-import ;https://github.com/melpa/melpa/pull/7326
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

;;latex
(package! aas)
(package! laas)
(package! org-fragtog)
(package! engrave-faces)

;;markdown and html
(package! ox-gfm)
(package! websocket)
;;(package! webkit
;;          :recipe (:host github
;;                   :repo "akirakyle/emacs-webkit"
;;                   :branch "main"
;;                   :files (:defaults "*.js" "*.css" "*.so" "*.nix")
;;                   :pre-build (("nix-shell" "shell.nix" "--command make"))))

;;looks
(package! solaire-mode :disable t)
(package! dimmer :recipe (:host github :repo "gonewest818/dimmer.el"))
(package! nano-theme :recipe (:host github :repo "rougier/nano-theme"))
(package! nano-agenda :recipe (:host github :repo "rougier/nano-agenda"))
(package! svg-tag-mode :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))
(package! nano-modeline :recipe (:host github :repo "rougier/nano-modeline"))

;;emacs additions
(package! lexic)
;;(unpin! tree-sitter)
;;(unpin! tree-sitter-langs)

;;lsp
(unpin! lsp-ui)
(unpin! lsp-mode)

;;fun
(package! nov)
(package! xkcd)
(package! monkeytype)
(package! selectric-mode :recipe (:local-repo "lisp/selectric-mode"))
