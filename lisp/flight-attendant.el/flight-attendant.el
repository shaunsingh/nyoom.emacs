;;; flight-attendant.el --- Major mode to provide Github Copilot support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author: cryptobadger <https://github.com/cryptobadger>
;; Maintainer: cryptobadger
;; Created: November 28, 2021
;; Modified: November 28, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/wintermute/flight-attendant
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Major mode to provide Github Copilot support
;;
;;; Code:
;;;

(add-to-list 'load-path ".")
(message "flight-attendant.el loaded")
(require 'json)
;; (add-load-path! ".")
(require 'flight-attendant-rpc)
(require 'flight-attendant-ui)
(require 'cl-lib)   ; for `cl-every', `cl-copy-list', `cl-delete-if-not'
(require 'subr-x)
(defvar fa-enabled-p nil
  "Is Fa enabled or not.")
(defconst fa-version "0.05.0"
  "The version of the Fa Lisp code.")

(defgroup fa nil
  "Emacs copilot."
  :prefix "fa-"
  :group 'languages)

(defcustom fa-mode-hook nil
  "Hook run when `fa-mode' is enabled.

This can be used to enable minor modes for Python development."
  :type 'hook
  :options '(subword-mode hl-line-mode)
  :group 'fa)

;;;###autoload
(defun fa-enable (&optional _ignored)
  "Enable Fa in all future Python buffers."
  (interactive)
  (and fa-enabled-p
       (message "Fa already enabled."))
  (unless fa-enabled-p
    (when (< emacs-major-version 24)
      (error "Fa requires Emacs 24 or newer"))
    (when _ignored
      (warn "The argument to `fa-enable' is deprecated, customize `fa-modules' instead"))
    (let ((filename (find-lisp-object-file-name 'python-mode
                                                'symbol-function)))
      (when (and filename
                 (string-match "/python-mode\\.el\\'"
                               filename))
        (error (concat "You are using python-mode.el. "
                       "Fa only works with python.el from "
                       "Emacs 24 and above"))))
    ;; (fa-modules-global-init)
    ;; (define-key inferior-python-mode-map (kbd "C-c C-z") 'fa-shell-switch-to-buffer)
    (add-hook 'python-mode-hook 'fa-mode)
    ;; (add-hook 'pyvenv-post-activate-hooks 'fa-rpc--disconnect)
    ;; (add-hook 'pyvenv-post-deactivate-hooks 'fa-rpc--disconnect)
    ;; (add-hook 'inferior-python-mode-hook 'fa-shell--enable-output-filter)
    ;; (add-hook 'python-shell-first-prompt-hook 'fa-shell--send-setup-code t)
    ;; Add codecell boundaries highligting
    ;; (font-lock-add-keywords
    ;;  'python-mode
    ;;  `((,(replace-regexp-in-string "\\\\" "\\\\"
    ;;                                fa-shell-cell-boundary-regexp)
    ;;     0 'fa-codecell-boundary prepend)))
    ;; ;; Enable Fa-mode in the opened python buffer
    (setq fa-enabled-p t)
    (dolist (buffer (buffer-list))
      (and (not (string-match "^ ?\\*" (buffer-name buffer)))
           (with-current-buffer buffer
             (when (string= major-mode 'python-mode)
               (python-mode)  ;; update codecell fontification
               (fa-mode t)))))))


(defun fa-disable ()
  "Disable Fa in all future Python buffers."
  (interactive)
  ;; (fa-modules-global-stop)
  ;; (define-key inferior-python-mode-map (kbd "C-c C-z") nil)
  ;; (remove-hook 'python-mode-hook 'fa-mode)
  ;; Remove codecell boundaries highligting
  (fa--remove-hooks)
  (remove-hook 'python-mode-hook 'fa-mode)
  (setq fa-enabled-p nil))

(defun fa--remove-hooks ()
  (remove-hook 'evil-insert-state-entry-hook 'fa--enter-insert)
  (remove-hook 'evil-insert-state-exit-hook 'fa-clear-overlay)
  (remove-hook 'post-self-insert-hook 'fa--on-insert))
(define-minor-mode fa-mode
  "Minor mode in Python buffers for the Emacs Lisp Python Environment.
\\{fa-mode-map}"
  :lighter " Fa"
  (unless (derived-mode-p 'python-mode)
    (error "Fa only works with `python-mode'"))
  (unless fa-enabled-p
    (error "Please enable Fa with `(fa-enable)` before using it"))
  ;; (when (boundp 'xref-backend-functions)
  ;;   (add-hook 'xref-backend-functions #'fa--xref-backend nil t)))
  ;; Set this for `fa-check' command
  ;; (setq-local python-check-command fa-syntax-check-command)
  (cond
   (fa-mode
    (message "Activating FA-Mode")
    (add-to-list 'mode-line-misc-info
                 `(fa-mode (" [COPILOT] ")))
    ;; enable
    ;; (fa-modules-buffer-init)
    (fa--add-hooks)
    (when (not gh-token)
      (fa-rpc-get-token)))
   ((not fa-mode)
    ;; disable
    (fa--remove-hooks))))
;; (fa-modules-buffer-stop)))

(defun fa--add-hooks ()
  (add-hook 'evil-insert-state-entry-hook 'fa--enter-insert)
  (add-hook 'evil-insert-state-exit-hook 'fa-clear-overlay)
  (add-hook 'post-self-insert-hook 'fa--on-insert))

(defun fa--enter-insert ()
  (when fa-mode
    (fa-clear-overlay)
    (fa-rpc-request-completion
     (lambda (completion)
       (message (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
       (let ((display-text (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
             (pos-line (cdr (car (cdr (assq 'position (car (cdr (assq 'completions completion))))))))
             (pos-char (cdr (assq 'character (cdr (assq 'position (car (cdr (assq 'completions completion)))))))))
         (and
          (message display-text)
          (fa-display-overlay-str display-text pos-line pos-char)))))))

(defun fa--on-insert ()
  (when fa-mode
    (fa-clear-overlay)
    (fa-rpc-request-completion
     (lambda (completion)
       (let ((display-text (cdr (assq 'displayText (car (cdr (assq 'completions completion))))))
             (pos-line (cdr (car (cdr (assq 'position (car (cdr (assq 'completions completion))))))))
             (pos-char (cdr (assq 'character (cdr (assq 'position (car (cdr (assq 'completions completion)))))))))
         (and
          (message display-text)
          (fa-display-overlay-str display-text pos-line pos-char)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module: Sane Defaults

(defun fa-module-sane-defaults (command &rest _args)
  "Module for sane Emacs default for python."
  (pcase command
    (`buffer-init
     ;; Set `forward-sexp-function' to nil in python-mode. See
     ;; http://debbugs.gnu.org/db/13/13642.html
     (set (make-local-variable 'forward-sexp-function) nil)
     ;; PEP8 recommends two spaces in front of inline comments.
     (set (make-local-variable 'comment-inline-offset) 2))
    (`buffer-stop
     (kill-local-variable 'forward-sexp-function)
     (kill-local-variable 'comment-inline-offset))))


;;;;;;;;;;;
;;; Modules

(defvar fa-modules-initialized-p nil
  "Boolean, set to true if modules were run with `global-init'.")

(defun fa-modules-run (command &rest args)
  "Run COMMAND with ARGS for all modules in `fa-modules'."
  (dolist (module fa-modules)
    (apply module command args)))

(defun fa-modules-global-init ()
  "Run the global-init method of Fa modules.

Make sure this only happens once."
  (unless fa-modules-initialized-p
    (fa-modules-run 'global-init)
    (setq fa-modules-initialized-p t)))

(defun fa-modules-global-stop ()
  "Run the global-stop method of Fa modules.

Make sure this only happens once per global-init call."
  (when fa-modules-initialized-p
    (fa-modules-run 'global-stop)
    (setq fa-modules-initialized-p nil)))

(defun fa-modules-buffer-init ()
  "Run the buffer-init method of Fa modules.

Make sure global-init is called first."
  (fa-modules-global-init)
  (fa-modules-run 'buffer-init))

(defun fa-modules-buffer-stop ()
  "Run the buffer-stop method of Fa modules."
  (fa-modules-run 'buffer-stop))


(provide 'flight-attendant)
;;; flight-attendant.el ends here
