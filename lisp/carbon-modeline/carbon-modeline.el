;;; carbon-modeline.el -- custom mode line for carbon themes  ;; -*- lexical-binding: t -*-
;; Copyright (C) 2020 Colin McLear
;; -------------------------------------------------------------------
;; Authors: Colin McLear, Modified Shaurya Singh
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
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;; Commentary:
;; A carbon modeline.
;; This mode line originated as a fork of the nano-emacs modeline.
;; See https://github.com/rougier/nano-emacs
;; https://github.com/rougier/nano-modeline
;; -------------------------------------------------------------------
;;
;; Bespoke mode line format:
;;
;; [ status | name (primary)               secondary | item1 | item2 ]
;;
;; -------------------------------------------------------------------
;;
;; It can be displayed at the bottom (mode-line) or at the top (header-line)
;; depending on carbon-modeline-position custom setting.
;;
;; There are two sets of faces (for active and inactive modelines) that
;; can be customized (M-x: customize-group + carbon-modeline)
;;
;; - carbon-modeline-active-name      / carbon-modeline-inactive-name
;; - carbon-modeline-active-primary   / carbon-modeline-inactive-primary
;; - carbon-modeline-active-secondary / carbon-modeline-inactive-secondary
;; - carbon-modeline-active-status-RO / carbon-modeline-inactive-status-RO
;; - carbon-modeline-active-status-RW / carbon-modeline-inactive-status-RW
;; - carbon-modeline-active-status-** / carbon-modeline-inactive-status-**
;;
;; Usage example:
;;
;; M-x: carbon-modeline-mode
;;
;; You may also toggle the modeline to the top or bottom using M-x: carbon-modeline-toggle

;;; Code:

;;;; Group
(defgroup carbon nil
  "Bespoke group"
  :group 'convenience)

(defgroup carbon-modeline nil
  "Bespoke group modeline"
  :group 'carbon)

(defgroup carbon-modeline-active nil
  "Active modeline faces.

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'carbon-modeline)

(defgroup carbon-modeline-inactive nil
  "Inactive modeline faces

Modeline is composed as:
[ status | name (primary)                        secondary ]"
  :group 'carbon-modeline)

;;;; Faces

(defface carbon-modeline-active
  '((t (:inherit mode-line)))
  "Modeline face for active modeline"
  :group 'carbon-modeline-active)

(defface carbon-modeline-active-name
  '((t (:inherit (mode-line))))
  "Modeline face for active name element"
  :group 'carbon-modeline-active)

(defface carbon-modeline-active-primary
  '((t (:inherit fringe)))
  "Modeline face for active primary element"
  :group 'carbon-modeline-active)

(defface carbon-modeline-active-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element"
  :group 'carbon-modeline-active)

(defface carbon-modeline-active-status-RO
  '((t (:inherit mode-line)))
  "Modeline face for active READ-ONLY element"
  :group 'carbon-modeline-active)

(defface carbon-modeline-active-status-RW
  '((t (:inherit mode-line)))
  "Modeline face for active READ-WRITE element"
  :group 'carbon-modeline-active)

(defface carbon-modeline-active-status-**
  '((t (:inherit mode-line)))
  "Modeline face for active MODIFIED element"
  :group 'carbon-modeline-active)

(defface carbon-modeline-inactive
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive window"
  :group 'carbon-modeline-inactive)

(defface carbon-modeline-inactive-name
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive name element"
  :group 'carbon-modeline-inactive)

(defface carbon-modeline-inactive-primary
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive primary element"
  :group 'carbon-modeline-inactive)

(defface carbon-modeline-inactive-secondary
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive primary element"
  :group 'carbon-modeline-inactive)

(defface carbon-modeline-inactive-status-RO
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive READ-ONLY element"
  :group 'carbon-modeline-inactive)

(defface carbon-modeline-inactive-status-RW
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive READ-WRITE element"
  :group 'carbon-modeline-inactive)

(defface carbon-modeline-inactive-status-**
  '((t (:inherit mode-line-inactive)))
  "Modeline face for inactive MODIFIED element"
  :group 'carbon-modeline-inactive)

;;; Modeline Options
(defcustom carbon-modeline-position 'top
  "Default modeline position (top or bottom)"
  :type '(choice
          (const :tag "Nil" nil)
          (const :tag "Top"    top)
          (const :tag "Bottom" bottom))
  :group 'carbon-modeline)

(defcustom carbon-modeline-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'carbon-modeline)

(defcustom carbon-modeline-size 3
  "Set the size of the modeline as an integer
Initial value is 3."
  :group 'carbon-modeline
  :type 'integer)

(defcustom carbon-modeline-cleaner nil
  "If t then show abbreviated mode symbol in modeline. Default is
nil. To change the values of the major-mode symbols see the value
of carbon-modeline-cleaner-alist"
  :group 'carbon-modeline
  :type 'boolean)

(defcustom carbon-modeline-git-diff-mode-line t
  "If t then show diff lines in modeline."
  :group 'carbon-modeline
  :type 'boolean)

(defcustom carbon-modeline-vc-symbol "  "
  "Symbol to use in buffers visiting files under version control"
  :group 'carbon-modeline
  :type 'string)

;; Visual Bell
(defcustom carbon-modeline-visual-bell t
  "If t then use carbon-modeline-visual-bell."
  :group 'carbon-themes
  :type 'boolean)

;; Mode line symbols
(defcustom carbon-modeline-gui-ro-symbol " ⨂ "
  "Modeline gui read-only symbol."
  :group 'carbon-modeline
  :type 'string)

(defcustom carbon-modeline-gui-mod-symbol " ** "
  "Modeline gui modified symbol."
  :group 'carbon-modeline
  :type 'string)

(defcustom carbon-modeline-gui-rw-symbol " ⨀ "
  "Modeline gui read-write symbol."
  :group 'carbon-modeline
  :type 'string)

(defcustom carbon-modeline-tty-ro-symbol " RO "
  "Modeline tty read-only symbol."
  :group 'carbon-modeline
  :type 'string)

(defcustom carbon-modeline-tty-mod-symbol " ** "
  "Modeline tty modified symbol."
  :group 'carbon-modeline
  :type 'string)

(defcustom carbon-modeline-tty-rw-symbol " RW "
  "Modeline tty read-write symbol."
  :group 'carbon-modeline
  :type 'string)

(defcustom carbon-modeline-truncate-value 30
  "Value of modeline truncate-length function."
  :group 'carbon-modeline
  :type 'integer)

(defcustom carbon-modeline-space-top +0.20
  "Space adjustment for top of modeline
 Possitive is upwards"
  :type 'float
  :group 'carbon-modeline)

(defcustom carbon-modeline-space-bottom -0.25
  "Space adjustment for bottom of modeline
 Negative is downwards."
  :type 'float
  :group 'carbon-modeline)

;;; Optional Functions

;;;; Visual bell for mode line

;; See https://github.com/hlissner/emacs-doom-themes for the idea

(require 'face-remap)

(defface carbon-modeline-visual-bell '((t (:underline "red3")))
  "Face to use for the mode-line when `carbon-modeline-visual-bell-config' is used."
  :group 'carbon-modeline)

;;;###autoload
(defun carbon-modeline-visual-bell-fn ()
  "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((carbon-modeline--bell-cookie (face-remap-add-relative 'mode-line 'carbon-modeline-visual-bell)))
    (force-mode-line-update)
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update)))
                    carbon-modeline--bell-cookie
                    (current-buffer))))

;;;###autoload
(defun carbon-modeline-visual-bell-config ()
  "Enable flashing the mode-line on error."
  (setq ring-bell-function #'carbon-modeline-visual-bell-fn
        visible-bell t))

(when carbon-modeline-visual-bell
  (carbon-modeline-visual-bell-config))


;;;; Clean mode line
;; Source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(require 'cl-lib)

(defvar carbon-modeline-cleaner-alist
  `((dired-mode . "Dir")
    (emacs-lisp-mode . "EL")
    (fundamental-mode . "FL")
    (helpful-mode . "")
    (help-mode . "")
    (lisp-interaction-mode . "λ")
    (markdown-mode . "MD")
    (magit-mode . "MG")
    (nxhtml-mode . "NX")
    (prog-mode . "PR")
    (python-mode . "PY")
    (text-mode . "TX")
    )
  "Alist for `carbon-modeline/clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *as substitute for* the original.")

(defun carbon-modeline/clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in carbon-modeline-cleaner-alist
           do (let* ((mode (car cleaner))
                     (mode-str (cdr cleaner))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))

(when carbon-modeline-cleaner
  (add-hook 'after-change-major-mode-hook #'carbon-modeline/clean-mode-line))



;;; Modeline Functions
;;;; Base Functions
(defun carbon-modeline-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  carbon-modeline-user-mode)

(defun carbon-modeline-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

(defun carbon-modeline-mode-name ()
  "Return current major mode name"
  (format-mode-line mode-name))

;;;; Branch display
;; -------------------------------------------------------------------
(defun carbon-project-name ()
  "return name of project without path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

(defun carbon-modeline-vc-project-branch ()
  "If buffer is visiting a file under version control, show project and branch name for file. Otherwise show '-'"
  (let ((backend (vc-backend buffer-file-name)))
    (concat
     (if buffer-file-name
         (if vc-mode
             (let ((project-name (carbon-project-name)))
               ;; Project name
               (unless (string= "-" project-name)
                 (concat
                  ;; Divider
                  (propertize " •" 'face `(:inherit fringe))
                  (format " %s" project-name)
                  )))))
     ;; Show branch
     (if vc-mode
         (concat
          carbon-modeline-vc-symbol (substring-no-properties vc-mode ;    
                                                              (+ (if (eq backend 'Hg) 2 3) 2)))  nil))))
;;;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
(when carbon-modeline-git-diff-mode-line
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
    "Show the information of git diff on modeline."
    (setq ad-return-value
        (concat ad-return-value
              (let ((plus-minus (vc-git--run-command-string
                             file "diff" "--numstat" "--")))
                (if (and plus-minus
                         (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
                    (concat
                         " "
                   (format "+%s" (match-string 1 plus-minus))
                   (format "-%s" (match-string 2 plus-minus)))
                  (propertize "" 'face '(:weight bold))))))))

;;;; Dir display


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun carbon-modeline-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;;;; Mode line status
;; ---------------------------------------------------------------------
(defun carbon-modeline-status ()
  "Return buffer status: default symbols are read-only (⨂)/(RO),
modified (⨀)/(**), or read-write (◯)/(RW)"
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    ;; Use status letters for TTY display
    (cond
     (modified
      (if (display-graphic-p)
          carbon-modeline-gui-mod-symbol
        carbon-modeline-tty-mod-symbol))
     (read-only
      (if (display-graphic-p)
          carbon-modeline-gui-ro-symbol
        carbon-modeline-tty-ro-symbol))
     (t (if (display-graphic-p)
            carbon-modeline-gui-rw-symbol
          carbon-modeline-tty-rw-symbol)))))

;;;; Compose mode line
;; -------------------------------------------------------------------

(defun carbon-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (active        (eq window carbon-modeline--selected-window))
         (prefix (if (display-graphic-p)
                     (cond ((string= status carbon-modeline-gui-ro-symbol)
                            (propertize (if (window-dedicated-p)" –– " carbon-modeline-gui-ro-symbol)
                                        'face (if active
                                                  'carbon-modeline-active-status-RO
                                                'carbon-modeline-inactive-status-RO)))
                           ((string= status carbon-modeline-gui-mod-symbol)
                            (propertize (if (window-dedicated-p)" –– " carbon-modeline-gui-mod-symbol)
                                        'face (if active
                                                  'carbon-modeline-active-status-**
                                                'carbon-modeline-inactive-status-**)))
                           ((string= status carbon-modeline-gui-rw-symbol)
                            (propertize (if (window-dedicated-p) " –– " carbon-modeline-gui-rw-symbol)
                                        'face (if active 'carbon-modeline-active-status-RW
                                                'carbon-modeline-inactive-status-RW)))
                           (t (propertize status
                                          'face (if active 'carbon-modeline-active-status-**
                                                  'carbon-modeline-inactive-status-**))))
                   ;; TTY displays
                   (cond ((string= status carbon-modeline-tty-ro-symbol)
                          (propertize
                           (if (window-dedicated-p) " -- " carbon-modeline-tty-ro-symbol)
                           'face (if active
                                     'carbon-modeline-active-status-RO
                                   'carbon-modeline-inactive-status-RO)))
                         ((string= status carbon-modeline-tty-mod-symbol)
                          (propertize
                           (if (window-dedicated-p) " -- " carbon-modeline-tt-mod-symbol)
                           'face (if active
                                     'carbon-modeline-active-status-**
                                   'carbon-modeline-inactive-status-**)))
                         ((string= status carbon-modeline-tty-rw-symbol)
                          (propertize
                           (if (window-dedicated-p) " -- " carbon-modeline-tty-rw-symbol)
                           'face (if active 'carbon-modeline-active-status-RW
                                   'carbon-modeline-inactive-status-RW)))
                         (t (propertize status
                                        'face (if active 'carbon-modeline-active-status-**
                                                'carbon-modeline-inactive-status-**))))))

         (left (concat
                (propertize " "  'face (if active 'carbon-modeline-active
                                         'carbon-modeline-inactive)
                            'display `(raise ,carbon-modeline-space-top))
                (propertize name 'face (if active 'carbon-modeline-active-name
                                         'carbon-modeline-inactive-name))
                (propertize " "  'face (if active 'carbon-modeline-active
                                         'carbon-modeline-inactive)
                            'display `(raise ,carbon-modeline-space-bottom))
                (propertize primary 'face (if active 'carbon-modeline-active-primary
                                            'carbon-modeline-inactive-primary))))
         (right (concat secondary " "))

         (available-width (- (window-total-width)
                             (length prefix) (length left) (length right)
                             (/ (window-right-divider-width) char-width)))
         (available-width (max 1 available-width)))
    (concat prefix
            left
            (propertize (make-string available-width ?\ )
                        'face (if active 'carbon-modeline-active
                                'carbon-modeline-inactive))
            (propertize right 'face (if active 'carbon-modeline-active-secondary
                                      'carbon-modeline-inactive-secondary)))))

;;;; Default display

(defun carbon-modeline-default-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (carbon-modeline-mode-name))
        (branch      (carbon-modeline-vc-project-branch))
        (position    (format-mode-line "%l:%c ")))
    (carbon-modeline-compose (carbon-modeline-status)
                              (carbon-modeline-truncate buffer-name carbon-modeline-truncate-value)
                              (concat "(" mode-name
                                      (when branch
                                        branch)
                                      ")")
                              (concat
                               ;; Narrowed buffer
                               (when (buffer-narrowed-p)
                                 (propertize "⇥ "  'face `(:inherit fringe)))
                               position))))

;;;; Prog & Text Modes
;; ---------------------------------------------------------------------
(defun carbon-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun carbon-modeline-elisp-mode-p ()
  (derived-mode-p 'lisp-data-mode))

(defun carbon-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

;;;; Info Display
;; ---------------------------------------------------------------------
(setq Info-use-header-line nil)
(defun carbon-modeline-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
      (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
      line)
    (while  (> depth 0)
      (setq node (nth 1 (assoc node nodes)))
      (if node (push node crumbs))
      (setq depth (1- depth)))
    (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                           crumbs (cons nil crumbs))))
    (forward-line 1)
    (dolist (node crumbs)
      (let ((text
           (if (not (equal node "Top")) node
             (format "%s"
                   (if (stringp Info-current-file)
                     (file-name-sans-extension
                      (file-name-nondirectory Info-current-file))
                   Info-current-file)))))
      (setq line (concat line (if (null line) "" " > ")
                           (if (null node) "..." text)))))
    (if (and cnode (not (equal cnode "Top")))
        (setq line (concat line (if (null line) "" " > ") cnode)))
    line))

(defun carbon-modeline-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun carbon-modeline-info-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Info"
                            (concat "("
                                    (carbon-modeline-info-breadcrumbs)
                                    ")")
                            ""))

;;;; Term & Vterm
;; ---------------------------------------------------------------------
(defun carbon-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun carbon-modeline-term-mode ()
  (carbon-modeline-compose " >_ "
                            "Terminal"
                            (concat "(" shell-file-name ")")
                            (carbon-modeline-shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
;; vterm
(defun carbon-modeline-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun carbon-modeline-get-ssh-host (_str)
  (let ((split-defdir (split-string default-directory)))
    (if (equal (length split-defdir) 1)
        (car (split-string (shell-command-to-string "hostname") "\n"))
      (cadr split-defdir))))

(defun carbon-modeline-vterm-mode ()
  (carbon-modeline-compose " >_ "
                            "Terminal"
                            (concat "(" (carbon-modeline-get-ssh-host default-directory) ")")
                            (carbon-modelibe-shorten-directory (car (last (split-string default-directory ":"))) 32)))

;;;; Message Mode
;; ---------------------------------------------------------------------
(defun carbon-modeline-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun carbon-modeline-message-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Message" "(draft)" ""))

;;;; Docview Mode
;;---------------------------------------------------------------------
(defun carbon-modeline-docview-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun carbon-modeline-docview-mode ()
  (let ((buffer-name (format-mode-line "%b"))
      (mode-name   (carbon-modeline-mode-name))
      (branch      (carbon-modeline-vc-project-branch))
      (page-number (concat
                  (number-to-string (doc-view-current-page)) "/"
                  (or (ignore-errors
                      (number-to-string (doc-view-last-page-number)))
                    "???"))))
    (carbon-modeline-compose
     (carbon-modeline-status)
     buffer-name
     (concat "(" mode-name
             branch
           ")" )
     page-number)))

;;;; PDF View Mode
;; ---------------------------------------------------------------------
(defun carbon-modeline-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (require 'pdf-view))

(defun carbon-modeline-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
      (mode-name   (carbon-modeline-mode-name))
      (branch      (carbon-modeline-vc-project-branch))
      (page-number (concat
                  (number-to-string (eval `(pdf-view-current-page))) "/"
                  (or (ignore-errors
                      (number-to-string (pdf-cache-number-of-pages)))
                    "???"))))
    (carbon-modeline-compose
     (carbon-modeline-status)
     buffer-name
     (concat "(" mode-name
             branch
           ")" )
     (concat page-number " "))))

;;;; MenuMode

(defun carbon-modeline-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun carbon-modeline-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (carbon-modeline-mode-name))
        (position    (format-mode-line "%l:%c ")))

    (carbon-modeline-compose (carbon-modeline-status)
                              buffer-name "" position)))


;;;; Completion
;; ---------------------------------------------------------------------
(defun carbon-modeline-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun carbon-modeline-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (carbon-modeline-mode-name))
        (position    (format-mode-line "%l:%c ")))

    (carbon-modeline-compose (carbon-modeline-status)
                              buffer-name "" position)))

;;;; Deft Mode

(with-eval-after-load 'deft
  (defun deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun carbon-modeline-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun carbon-modeline-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Search:")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (carbon-modeline-compose prefix primary filter matches)))

;;;; Calendar Mode
;; ---------------------------------------------------------------------
(defun carbon-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun carbon-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun carbon-modeline-calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default)))))
(add-hook 'calendar-initial-window-hook #'carbon-modeline-calendar-setup-header)

;;;; Org Capture
;; ---------------------------------------------------------------------
(defun carbon-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun carbon-modeline-org-capture-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Capture"
                            "(Org)"
                            ""))

(with-eval-after-load 'org-capture
  (defun carbon-modeline--org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'carbon-modeline--org-capture-turn-off-header-line))

;;;; Org Agenda
;; ---------------------------------------------------------------------
(defun carbon-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun carbon-modeline-org-agenda-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Agenda"
                            ""
                            (concat (propertize "◴"
                                                'face 'default
                                                'display '(raise 0.06))
                                    (format-time-string "%H:%M "))))

;;;; Org Clock
;; ---------------------------------------------------------------------
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook #'carbon-modeline-org-clock-out))

(defun carbon-modeline-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun carbon-modeline-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (not org-mode-line-string)))

(defun carbon-modeline-org-clock-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (carbon-modeline-mode-name))
        (branch      (carbon-modeline-vc-project-branch)))
    (carbon-modeline-compose (carbon-modeline-status)
                              buffer-name
                              (concat "(" mode-name
                                      (when branch
                                        branch)
                                      ")")
                              (concat
                               ;; Narrowed buffer
                               (when (buffer-narrowed-p)
                                 (propertize "⇥ "  'face `(:inherit fringe)))
                               org-mode-line-string))))


;;;; Elfeed
(defun carbon-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun carbon-modeline-elfeed-search-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Elfeed"
                            (concat "(" (elfeed-search--header)  ")")
                            ""))

(defun carbon-modeline-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

(defun carbon-modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun carbon-modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (carbon-modeline-compose (carbon-modeline-status)
                              (carbon-modeline-truncate title 40)
                              (concat "(" tags-str ")")
                              feed-title)))

;;;; Mu4e

(defun carbon-modeline-mu4e-last-query ()
  "Get the most recent mu4e query or nil if there is none."
  (if (fboundp 'mu4e-last-query)
      (mu4e-last-query)
    mu4e~headers-last-query))

(defun carbon-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))

(defun carbon-modeline-mu4e-server-props ()
  "Encapsulates the call to the variable mu4e-/~server-props
depending on the version of mu4e."
  (if (string> mu4e-mu-version "1.6.5")
      mu4e--server-props
    mu4e~server-props))

;; ---------------------------------------------------------------------
(defun carbon-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun carbon-modeline-mu4e-dashboard-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Mail"
                            (carbon-modeline-mu4e-context)
                            (format "%d messages" (plist-get (carbon-modeline-mu4e-server-props) :doccount))))

;; ---------------------------------------------------------------------
(defun carbon-modeline-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))

(defun carbon-modeline-mu4e-loading-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Mail"
                            (carbon-modeline-mu4e-context)
                            (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun carbon-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun carbon-modeline-mu4e-main-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "Mail"
                            (carbon-modeline-mu4e-context)
                            (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun carbon-modeline-mu4e-quote (str)
  (if (string> mu4e-mu-version "1.6.5")
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun carbon-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun carbon-modeline-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (carbon-modeline-compose (carbon-modeline-status)
                              (carbon-modeline-mu4e-quote (carbon-modeline-mu4e-last-query))
                              ""
                              (format "[%s]"
                                      (carbon-modeline-mu4e-quote
                                       (mu4e-context-name (mu4e-context-current)))))))

;; ---------------------------------------------------------------------
(defun carbon-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun carbon-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (carbon-modeline-compose (carbon-modeline-status)
                              (carbon-modeline-truncate subject 60)
                              ""
                              from)))

(defun carbon-modeline-mu4e-view-hook ()
  (setq header-line-format "%-")
  (face-remap-add-relative 'header-line
                           '(:background "#ffffff"
                             :underline nil
                             :box nil
                             :height 1.0)))
;; (add-hook 'mu4e-view-mode-hook #'carbon-modeline-mu4e-view-hook)

;;;; Help

(defun carbon-modeline-carbon-help-mode-p ()
  (derived-mode-p 'carbon-help-mode))

(defun carbon-modeline-carbon-help-mode ()
  (carbon-modeline-compose (carbon-modeline-status)
                            "GNU Emacs"
                            "(help)"
                            ""))

;;;; Ein

(defun carbon-modeline-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (carbon-modeline-compose (if (ein:notebook-modified-p) "**" "RW")
                              buffer-name
                              ""
                              (ein:header-line))))

;;; Set Mode line

;;;; Clear Modeline Faces

(defun carbon-modeline-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))


;;;; Define Selected Window

(defvar carbon-modeline--saved-mode-line-format nil)
(defvar carbon-modeline--saved-header-line-format nil)
(defvar carbon-modeline--selected-window nil)

(defun carbon-modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq carbon-modeline--selected-window (selected-window)))

;;;; Set content for mode/header line

(defun carbon-modeline ()
  "Build and set the modeline."
  
  (let* ((format
          '((:eval
             (cond
              ((carbon-modeline-user-mode-p)            (funcall ,carbon-modeline-user-mode))
              ((carbon-modeline-prog-mode-p)            (carbon-modeline-default-mode))
              ((carbon-modeline-message-mode-p)         (carbon-modeline-message-mode))
              ((carbon-modeline-elfeed-search-mode-p)   (carbon-modeline-elfeed-search-mode))
              ((carbon-modeline-elfeed-show-mode-p)     (carbon-modeline-elfeed-show-mode))
              ((carbon-modeline-deft-mode-p)            (carbon-modeline-deft-mode))
              ((carbon-modeline-info-mode-p)            (carbon-modeline-info-mode))
              ((carbon-modeline-calendar-mode-p)        (carbon-modeline-calendar-mode))
              ((carbon-modeline-org-capture-mode-p)     (carbon-modeline-org-capture-mode))
              ((carbon-modeline-org-agenda-mode-p)      (carbon-modeline-org-agenda-mode))
              ((carbon-modeline-org-clock-mode-p)       (carbon-modeline-org-clock-mode))
              ((carbon-modeline-term-mode-p)            (carbon-modeline-term-mode))
              ((carbon-modeline-vterm-mode-p)           (carbon-modeline-term-mode))
              ((carbon-modeline-mu4e-dashboard-mode-p)  (carbon-modeline-mu4e-dashboard-mode))
              ((carbon-modeline-mu4e-main-mode-p)       (carbon-modeline-mu4e-main-mode))
              ((carbon-modeline-mu4e-loading-mode-p)    (carbon-modeline-mu4e-loading-mode))
              ((carbon-modeline-mu4e-headers-mode-p)    (carbon-modeline-mu4e-headers-mode))
              ((carbon-modeline-mu4e-view-mode-p)       (carbon-modeline-mu4e-view-mode))
              ((carbon-modeline-text-mode-p)            (carbon-modeline-default-mode))
              ((carbon-modeline-pdf-view-mode-p)        (carbon-modeline-pdf-view-mode))
              ((carbon-modeline-docview-mode-p)         (carbon-modeline-docview-mode))
              ((carbon-modeline-buffer-menu-mode-p)     (carbon-modeline-buffer-menu-mode))
              ((carbon-modeline-completion-list-mode-p) (carbon-modeline-completion-list-mode))
              ((carbon-modeline-carbon-help-mode-p)       (carbon-modeline-carbon-help-mode))
              (t                                      (carbon-modeline-default-mode)))))))
    
    (if (eq carbon-modeline-position 'top)
        (progn
          (setq-default mode-line-format (list (propertize "%_" 'face `(:inherit fringe))))
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))


(defun carbon-modeline-mode--activate ()
  "Activate carbon modeline"

  ;; Save current mode-line and header-line
  (unless carbon-modeline--saved-mode-line-format
    (setq carbon-modeline--saved-mode-line-format mode-line-format)
    (setq carbon-modeline--saved-header-line-format header-line-format))

  ;; since the EIN library itself is constantly re-rendering the notebook, and thus
  ;; re-setting the header-line-format, we cannot use the carbon-modeline function to set
  ;; the header format in a notebook buffer. Fortunately, EIN exposes the
  ;; ein:header-line-format variable for just this purpose.
  (with-eval-after-load 'ein
    (if (eq carbon-modeline-position 'top)
        (setq ein:header-line-format '((:eval (carbon-modeline-ein-notebook-mode))))))

  ;; Elfeed uses header-line, we need to tell it to use our own format
  (with-eval-after-load 'elfeed
    (if (eq carbon-modeline-position 'top)
        (setq elfeed-search-header-function #'carbon-modeline-elfeed-setup-header)))
  
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'carbon-modeline-calendar-setup-header))

  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'carbon-modeline-org-clock-out))
  
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'carbon-modeline--org-capture-turn-off-header-line))

  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil))

  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'carbon-modeline))

  (if (eq carbon-modeline-position 'top)
      (setq Info-use-header-line nil))

  (if (eq carbon-modeline-position 'top)
      (setq Buffer-menu-use-header-line nil))

  ;; Update selected window
  (carbon-modeline--update-selected-window)

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (carbon-modeline)
  
  ;; This hook is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'carbon-modeline--update-selected-window)
  (force-mode-line-update t))


(defun carbon-modeline-mode--deactivate ()
  "Deactivate carbon mode-line and restore default mode-line"
  
  (custom-reevaluate-setting 'Info-use-header-line)
  (custom-reevaluate-setting 'Buffer-menu-use-header-line)
  (custom-reevaluate-setting 'eshell-status-in-mode-line)

  (if (boundp 'ein:header-line-format)
      (setq ein:header-line-format '(:eval (ein:header-line))))
  (if (boundp 'elfeed-search-header-function)
      (setq elfeed-search-header-function #'elfeed-search--header))
  
  (remove-hook 'calendar-initial-window-hook
               #'carbon-modeline-calendar-setup-header)
  (remove-hook 'org-capture-mode-hook
               #'carbon-modeline--org-capture-turn-off-header-line)
  (remove-hook 'org-clock-out-hook
               #'carbon-modeline-org-clock-out)
  (remove-hook 'post-command-hook
               #'carbon-modeline--update-selected-window)
  (advice-remove 'mu4e~header-line-format #'carbon-modeline)

  (setq         mode-line-format carbon-modeline--saved-mode-line-format)
  (setq-default mode-line-format carbon-modeline--saved-mode-line-format)
  (setq         header-line-format carbon-modeline--saved-header-line-format)
  (setq-default header-line-format carbon-modeline--saved-header-line-format))


;;;###autoload
(define-minor-mode carbon-modeline-mode
  "Toggle carbon-modeline minor mode"
  :group 'carbon-modeline
  :global t
  :init-value nil

  (if carbon-modeline-mode
      (carbon-modeline-mode--activate)
    (carbon-modeline-mode--deactivate))

  ;; Run any registered hooks
  (run-hooks 'carbon-modeline-mode-hook))

;; Toggle functions
(defun carbon-modeline-get-current-theme ()
  "Determines the currently enabled theme."
  (or (car custom-enabled-themes) '*default*))

;;;###autoload
(defun carbon-modeline-reload-current-theme ()
  "Reloads the currently activated theme."
  (carbon-modeline-enable-theme (carbon-modeline-get-current-theme)))

;;;###autoload
(defun carbon-modeline-enable-theme (theme)
  "Enables the specified color-theme.
Pass `*default*' to select Emacs defaults."
  (cl-flet* ((disable-all-themes ()
                                 (mapcar 'disable-theme
                                       custom-enabled-themes)))
    (disable-all-themes)
    (condition-case nil
        (when (not (eq theme '*default*))
          (load-theme theme t))
      (error nil))))

(defun carbon-modeline-toggle ()
  "Toggle between a modeline in header or one at footer. Note that
this function reloads the theme to properly set colors and that
you may need to revert buffers to see the modeline properly."
  (interactive)
  (if (eq carbon-modeline-position 'top)
      (progn
        (carbon-modeline-face-clear 'mode-line)
        (setq carbon-modeline-position 'bottom)
        (carbon-modeline-mode--deactivate)
        (carbon-modeline-mode--activate)
        (carbon-modeline-reload-current-theme)
        (run-hooks 'carbon-modeline-mode-hook))
    (progn
      (setq carbon-modeline-position 'top)
      (carbon-modeline-mode--deactivate)
      (setq-default mode-line-format (list (propertize "%_" 'face `(:inherit fringe))))
      (carbon-modeline-mode--activate)
      (carbon-modeline-reload-current-theme)
      (run-hooks 'carbon-modeline-mode-hook))))

(provide 'carbon-modeline)
;;; carbon-modeline.el ends here
