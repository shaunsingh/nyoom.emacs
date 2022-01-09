;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(unpin! org)
(package! doct)
(package! citar)
(package! citeproc)
(package! websocket)
(package! org-appear)
(package! org-roam-ui)
(package! org-preview-html)
(package! org-num :recipe (:local-repo "lisp/org-num"))
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

;;looks
(package! focus)
(package! dimmer)
(package! nano-theme)
(package! nano-agenda)
(package! info-colors)
(package! svg-tag-mode)
(package! nano-modeline)
(package! solaire-mode :disable t)
(package! nano-splash :recipe (:local-repo "lisp/nano-splash"))
(package! ox-chameleon :recipe (:host github
                                :repo "tecosaur/ox-chameleon")
  :pin "5a1928b9c33cbeb0463cf794afe8cff4ab512ce7")

;;emacs additions
;; (package! meow)
(package! esup)
(package! lexic)
(package! etrace :recipe (:host github
                          :repo "aspiers/etrace"))

;;fun
(package! nov)
(package! xkcd)
(package! monkeytype)
(package! selectric-mode :recipe (:local-repo "lisp/selectric-mode"))

(use-package! etrace
  :after elp)
