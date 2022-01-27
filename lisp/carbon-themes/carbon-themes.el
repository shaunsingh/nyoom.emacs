;;; carbon-themes.el --- A set of minimal and medium contrast light/dark themes from IBM
;; Copyright (C) 2020 Colin McLear, Shaurya Singh
;; -------------------------------------------------------------------
;; Authors: Colin McLear, modified Shaurya Singh
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with this
;; program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;; Commentary: This theme offers a set of light/dark bespoke themes and custom mode
;; line for the discerning yak shaver. There is also an optional mode line
;; configuration, which may be used either as a header line or a foot. Options and
;; useful function below. See README for further info
;; -------------------------------------------------------------------
;;; Code:


;;;; Theme Options
(defcustom carbon-set-theme 'light
  "Choose which theme variant, light or dark, to use."
  :group 'carbon-themes
  :type 'symbol)

;; Cursors
(defcustom carbon-set-evil-cursors t
  "If t then use carbon evil cursor colors."
  :group 'carbon-themes
  :type 'boolean)

;; Font options
(defcustom carbon-set-italic-comments t
  "If t then use italics for comments."
  :group 'carbon-themes
  :type 'boolean)

(defcustom carbon-set-italic-keywords t
  "If t then use italics for keywords."
  :group 'carbon-themes
  :type 'boolean)

(defcustom carbon-set-variable-pitch t
  "If t then use variable-pitch for headings."
  :group 'carbon-themes
  :type 'boolean)

;;;; After Load Theme Hook
(defvar carbon-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'carbon-after-load-theme-hook))

;;;; Disable Theme Function
(defun carbon--disable-all-themes ()
  "Disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

;;;; Theme Toggle
;;;###autoload
(defun carbon/toggle-theme ()
  "Toggle between dark and light variants"
  (interactive)
  (if (eq carbon-set-theme 'light)
      (progn
        (carbon--disable-all-themes)
        (setq carbon-set-theme 'dark)
        (load-theme 'carbon t))
    (progn
      (carbon--disable-all-themes)
      (setq carbon-set-theme 'light)
      (load-theme 'carbon t))))

;;;; Call Theme Functions
;;;###autoload
(defun carbon/light-theme ()
  "Set light variant of carbon-theme"
  (interactive)
  (carbon--disable-all-themes)
  (setq carbon-set-theme 'light)
  (load-theme 'carbon t))

;;;###autoload
(defun carbon/dark-theme ()
  "Set dark variant of carbon-theme"
  (interactive)
  (carbon--disable-all-themes)
  (setq carbon-set-theme 'dark)
  (load-theme 'carbon t))

;;; Provide Theme
(provide 'carbon-themes)
;;; End carbon-themes.el
