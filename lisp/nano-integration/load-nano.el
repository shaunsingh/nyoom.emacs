;;; load-nano.el -*- lexical-binding: t; -*-
;; This file loads nano. Enable by putting (require 'load-nano) in config.el

;; necessary for proper appearance of nano
(setq doom-theme 'nil)

(require 'nano-layout)
(require 'nano-modeline)
(require 'nano-theme)
(require 'nano-colors)
(require 'nano-theme-light)
(require 'nano-faces)
(nano-faces)
(nano-theme)
(require 'nano-face-override)
(provide 'load-nano)
