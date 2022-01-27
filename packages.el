;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; live life on the edge
(unpin! t)

;;org
(package! doct)
(package! websocket)
(package! org-appear)
(package! org-roam-ui)
(package! org-preview-html)
(package! org-num :recipe (:local-repo "lisp/org-num"))
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))
(package! org-bib-mode :recipe (:host github :repo "rougier/org-bib-mode"))
(package! notebook-mode :recipe (:host github :repo "rougier/notebook-mode"))
(package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-pandoc-import :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))

;;latex
(package! aas)
(package! laas)
(package! engrave-faces)

;;looks
(package! focus)
(package! dimmer)
(package! info-colors)
(package! svg-tag-mode)
(package! solaire-mode :disable t)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon") :pin "5a1928b9c33cbeb0463cf794afe8cff4ab512ce7")

;;carbon
(package! carbon-themes :recipe (:local-repo "lisp/carbon-themes"))
(package! carbon-modeline :recipe (:local-repo "lisp/carbon-modeline"))

;;emacs additions
;; (package! meow)
(package! lexic)
(package! etrace :recipe (:host github :repo "aspiers/etrace"))
(package! flight-attendant.el :recipe (:local-repo "lisp/flight-attendant.el" :files ("*.el" "copilot")))

;;fun
(package! nov)
(package! xkcd)
(package! md4rd)
(package! elcord)
(package! monkeytype)
(package! selectric-mode :recipe (:local-repo "lisp/selectric-mode"))

(use-package! etrace
  :after elp)
