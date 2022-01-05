;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! doct)
(package! citar)
(package! citeproc)
(package! org-appear)
(package! org-roam-ui)
(package! org-ol-tree
  :recipe (:host github 
           :repo "Townk/org-ol-tree"))
(package! notebook-mode
  :recipe (:host github 
           :repo "rougier/notebook-mode"))
;; (package! org-bib-mode
;;   :recipe (:host github 
;;            :repo "rougier/org-bib-mode"))
(package! org-pretty-table
  :recipe (:host github 
           :repo "Fuco1/org-pretty-table"))
(package! org-cite-csl-activate
  :recipe (:host github 
           :repo "andras-simonyi/org-cite-csl-activate"))
(package! org-pandoc-import 
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

;;latex
(package! aas)
(package! laas)
(package! engrave-faces)

;;markdown and html
(package! ox-gfm)
(package! websocket)

;;looks
(package! focus)
(package! dimmer)
(package! nano-theme)
(package! nano-agenda)
(package! svg-tag-mode)
(package! nano-modeline)
(package! solaire-mode :disable t)
(package! nano-splash :recipe (:local-repo "lisp/nano-splash"))
(package! ox-chameleon :recipe (:host github 
                                :repo "tecosaur/ox-chameleon"))

;;emacs additions
;; (package! meow)
(package! esup)
(package! lexic)

;;fun
(package! nov)
(package! xkcd)
(package! monkeytype)
(package! selectric-mode :recipe (:local-repo "lisp/selectric-mode"))
