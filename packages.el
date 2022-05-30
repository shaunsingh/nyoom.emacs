;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! doct)
(package! websocket)
(package! org-appear)
(package! org-roam-ui)
(package! org-preview-html)

;;latex
(package! aas)
(package! laas)
(package! engrave-faces)
(package! ox-chameleon
  :recipe (:host github :repo "tecosaur/ox-chameleon"))

;;looks
(package! focus)
(package! dimmer)
(package! minions)
(package! mini-frame)
(package! solaire-mode :disable t)

;; nano stuff
(package! nano-theme)
(package! svg-tag-mode)
;; (package! nano-modeline)

;;emacs additions
(package! nov)
(package! lexic)
(package! info-colors)
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))

;;fun
(package! xkcd)
(package! md4rd)
(package! smudge)
(package! elcord)
(package! monkeytype)
