;; [[file:config.org::*Intro][Intro:1]]
;;; config.el -*- lexical-binding: t; -*-
;; This file has been generated from config.org file. DO NOT EDIT.
;; Sources are available from https://github.com/shaunsingh/nyoom.emacs

;; Copyright (C) 2022 Shaurya Singh

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;; Intro:1 ends here

;; [[file:config.org::*Customizations][Customizations:1]]
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; Customizations:1 ends here

;; [[file:config.org::*Personal information][Personal information:1]]
(setq user-full-name "Shaurya Singh"
      user-mail-address "shaunsingh0207@gmail.com")
;; Personal information:1 ends here

;; [[file:config.org::*Window management][Window management:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Window management:1 ends here

;; [[file:config.org::*Window management][Window management:2]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
;; Window management:2 ends here

;; [[file:config.org::*Shell][Shell:1]]
(setq vterm-always-compile-module t)
;; Shell:1 ends here

;; [[file:config.org::*Shell][Shell:2]]
(setq vterm-kill-buffer-on-exit t)
;; Shell:2 ends here

;; [[file:config.org::*Shell][Shell:3]]
(after! vterm
  (setf (alist-get "magit-status" vterm-eval-cmds nil nil #'equal)
        '((lambda (path)
            (magit-status path)))))
;; Shell:3 ends here

;; [[file:config.org::*Shell][Shell:4]]
(setq +ligatures-in-modes t)
;; Shell:4 ends here

;; [[file:config.org::*Fonts][Fonts:1]]
;;fonts
(setq doom-font (font-spec :family "Liga SFMono Nerd Font" :size 15)
      doom-big-font (font-spec :family "Liga SFMono Nerd Font" :size 20)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 16)
      doom-unicode-font (font-spec :family "Liga SFMono Nerd Font")
      doom-serif-font (font-spec :family "IBM Plex Sans" :size 16 :weight 'medium))
;; Fonts:1 ends here

;; [[file:config.org::*LSP][LSP:1]]
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil))

(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K
;; LSP:1 ends here

;; [[file:config.org::*Company][Company:1]]
(after! company
  (setq company-idle-delay 0.1
        company-selection-wrap-around t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-dabbrev-other-buffers nil
        company-tooltip-limit 5
        company-tooltip-minimum-width 40)
  (set-company-backend!
    '(text-mode
      markdown-mode
      gfm-mode)
    '(:seperate
      company-files)))
;; Company:1 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:1]]
(setq scroll-margin 2
      auto-save-default t
      display-line-numbers-type nil
      delete-by-moving-to-trash t
      truncate-string-ellipsis "…"
      browse-url-browser-function 'xwidget-webkit-browse-url)

(fringe-mode 0)
(global-subword-mode 1)
;; Better Defaults:1 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:2]]
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; Better Defaults:2 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:3]]
(cond
 ((string-equal system-type "darwin")
  (setq frame-resize-pixelwise  t
        window-resize-pixelwise t)))
;; Better Defaults:3 ends here

;; [[file:config.org::*Evil][Evil:1]]
(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring
;; Evil:1 ends here

;; [[file:config.org::*Evil][Evil:2]]
(setq which-key-allow-multiple-replacements t
      which-key-idle-delay 0.5) ;; I need the help, I really do
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . " \\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . " \\1"))))
;; Evil:2 ends here

;; [[file:config.org::*Mu4e][Mu4e:1]]
(after! mu4e
  (setq mu4e-index-cleanup nil
        mu4e-index-lazy-check t
        mu4e-update-interval 300)
  (set-email-account! "shaunsingh0207"
                      '((mu4e-sent-folder       . "/Sent Mail")
                        (mu4e-drafts-folder     . "/Drafts")
                        (mu4e-trash-folder      . "/Trash")
                        (mu4e-refile-folder     . "/All Mail")
                        (smtpmail-smtp-user     . "shaunsingh0207@gmail.com"))))
;; Mu4e:1 ends here

;; [[file:config.org::*Mu4e][Mu4e:2]]
(after! mu4e
  (setq sendmail-program "msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))
;; Mu4e:2 ends here

;; [[file:config.org::*Magit][Magit:1]]
(after! magit
  (magit-delta-mode +1))
;; Magit:1 ends here

;; [[file:config.org::*Monkeytype][Monkeytype:1]]
(use-package! monkeytype
  :commands (monkeytype-region monkeytype-buffer monkeytype-region-as-words)
  :config
  (setq monkeytype-directory "~/.config/monkeytype"
        monkeytype-file-name "%a-%d-%b-%Y-%H-%M-%S"
        monkeytype-randomize t
        monkeytype-delete-trailing-whitespace t
        monkeytype-excluded-chars-regexp "[^[:alnum:]']"))
;; Monkeytype:1 ends here

;; [[file:config.org::*Smudge][Smudge:1]]
(use-package! smudge
  :commands global-smudge-remote-mode
  :config
  (setq smudge-transport 'connect
        smudge-oauth2-client-secret "8f5525c076544cd6b25588c868b9b3d7"
        smudge-oauth2-client-id "4b2b46899e604b6884714cd7ca47e0e3")
  (map! :map smudge-mode-map "M-p" #'smudge-command-map))
;; Smudge:1 ends here

;; [[file:config.org::*Dashboard][Dashboard:1]]
(setq fancy-splash-image (expand-file-name "misc/splash-images/kaori.png" doom-private-dir) ;; ibm, kaori, fennel
      +doom-dashboard-banner-padding '(0 . 0))

(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-private-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splase-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splase-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splase-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splase-phrase--cache)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))

;; remove useless dashboard info
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
;; Dashboard:1 ends here

;; [[file:config.org::*Info Colors][Info Colors:1]]
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)
;; Info Colors:1 ends here

;; [[file:config.org::*Minibuffer][Minibuffer:1]]
(setq minibuffer-prompt-properties '(read-only t
                                     cursor-intangible t
                                     face minibuffer-prompt)
      enable-recursive-minibuffers t)

(defun my/minibuffer-header ()
  "Minibuffer header"
  (let ((depth (minibuffer-depth)))
    (concat
     (propertize (concat "  " (if (> depth 1)
                                   (format "Minibuffer (%d)" depth)
                                 "Minibuffer ")
                         "\n")
                 'face `(:inherit (nano-subtle nano-strong)
                         :box (:line-width (1 . 3)
                               :color ,(face-background 'nano-subtle)
                               :style flat)
                         :extend t)))))

(defun my/mini-frame-reset (frame)
  "Reset FRAME size and position.

  Move frame at the top of parent frame and resize it
  horizontally to fit the width of current selected window."
  (interactive)
  (let* ((border (frame-parameter frame 'internal-border-width))
         (height (frame-parameter frame 'height)))
    (with-selected-frame (frame-parent frame)
      (let* ((edges (window-pixel-edges))
             (body-edges (window-body-pixel-edges))
             (top (nth 1 edges))
             (bottom (nth 3 body-edges))
             (left (- (nth 0 edges) (or left-fringe-width 0)))
             (right (+ (nth 2 edges) (or right-fringe-width 0)))
             (width (- right left))
             (y (- top border)))
        (set-frame-width frame width nil t)
        (set-frame-height frame height)
        (set-frame-position frame (- left border) y)))))

(defun my/mini-frame-shrink (frame &optional delta)
  "Make the FRAME DELTA lines smaller.

  If no argument is given, make the frame one line smaller. If
  DELTA is negative, enlarge frame by -DELTA lines."
  (interactive)
  (let ((delta (or delta -1)))
    (when (and (framep frame)
               (frame-live-p frame)
               (frame-visible-p frame))
      (set-frame-parameter frame 'height
                           (+ (frame-parameter frame 'height) delta)))))

(defun my/minibuffer-setup ()
  "Install a header line in the minibuffer via an overlay (and a hook)"
  (set-window-margins nil 0 0)
  (set-fringe-style '(0 . 0))
  (cursor-intangible-mode t)
  (face-remap-add-relative 'default
                           :inherit 'highlight)
 (let* ((overlay (make-overlay (+ (point-min) 0) (+ (point-min) 0)))
        (inhibit-read-only t))

    (save-excursion
      (goto-char (point-min))
      (insert (propertize
               (concat (my/minibuffer-header)
                       (propertize "\n" 'face `(:height 0.33))
                       (propertize " "))
               'cursor-intangible t
               'read-only t
               'field t
               'rear-nonsticky t
               'front-sticky t)))))


(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup)
;; Minibuffer:1 ends here

;; [[file:config.org::*Mini-Frame][Mini-Frame:1]]
(use-package! mini-frame
  :hook (after-init . mini-frame-mode)
  :config
  (defcustom my/minibuffer-position 'top
    "Minibuffer position, one of 'top or 'bottom"
    :type '(choice (const :tag "Top"    top)
                   (const :tag "Bottom" bottom))
    :group 'nano-minibuffer)
  
  (defun my/minibuffer--frame-parameters ()
    "Compute minibuffer frame size and position."

    ;; Quite precise computation to align the minibuffer and the
    ;; modeline when they are both at top position
    (let* ((edges (window-pixel-edges)) ;; (left top right bottom)
           (body-edges (window-body-pixel-edges)) ;; (left top right bottom)
           (left (nth 0 edges)) ;; Take margins into account
           (top (nth 1 edges)) ;; Drop header line
           (right (nth 2 edges)) ;; Take margins into account
           (bottom (nth 3 body-edges)) ;; Drop header line
           (left (if (eq left-fringe-width 0)
                     left
                   (- left (frame-parameter nil 'left-fringe))))
           (right (nth 2 edges))
           (right (if (eq right-fringe-width 0)
                      right
                    (+ right (frame-parameter nil 'right-fringe))))
           (border 1)
           (width (- right left (* 0 border)))

           ;; Window divider mode
           (width (- width (if (and (bound-and-true-p window-divider-mode)
                                    (or (eq window-divider-default-places 'right-only)
                                        (eq window-divider-default-places t))
                                    (window-in-direction 'right (selected-window)))
                               window-divider-default-right-width
                             0)))
           (y (- top border)))

      (append `((left-fringe . 0)
                (right-fringe . 0)
                (user-position . t)
                (foreground-color . ,(face-foreground 'highlight nil 'default))
                (background-color . ,(face-background 'highlight nil 'default)))
              (cond ((and (eq my/minibuffer-position 'bottom))
                     `((top . -1)
                       (left . 0)
                       (width . 1.0)
                       (child-frame-border-width . 0)
                       (internal-border-width . 0)))
                    (t
                     `((left . ,(- left border))
                       (top . ,y)
                       (width . (text-pixels . ,width))
                       (child-frame-border-width . ,border)
                       (internal-border-width . ,border)))))))

    (set-face-background 'child-frame-border (face-foreground 'nano-faded))
    (setq mini-frame-default-height 3)
    (setq mini-frame-create-lazy t)
    (setq mini-frame-show-parameters 'my/minibuffer--frame-parameters)
    (setq mini-frame-ignore-commands
          '("edebug-eval-expression" debugger-eval-expression))
    (setq mini-frame-internal-border-color (face-foreground 'nano-faded))
    (setq mini-frame-resize-min-height 3)
    (setq mini-frame-resize t)
    
  (defun my/mini-frame (&optional height foreground background border)
    "Create a child frame positionned over the header line whose
  width corresponds to the width of the current selected window.

  The HEIGHT in lines can be specified, as well as the BACKGROUND
  color of the frame. BORDER width (pixels) and color (FOREGROUND)
  can be also specified."
    (interactive)
    (let* ((foreground (or foreground
                           (face-foreground 'font-lock-comment-face nil t)))
           (background (or background (face-background 'highlight nil t)))
           (border (or border 1))
           (height (round (* (or height 8) (window-font-height))))
           (edges (window-pixel-edges))
           (body-edges (window-body-pixel-edges))
           (top (nth 1 edges))
           (bottom (nth 3 body-edges))
           (left (- (nth 0 edges) (or left-fringe-width 0)))
           (right (+ (nth 2 edges) (or right-fringe-width 0)))
           (width (- right left))

           ;; Window divider mode
           (width (- width (if (and (bound-and-true-p window-divider-mode)
                                    (or (eq window-divider-default-places 'right-only)
                                        (eq window-divider-default-places t))
                                  (window-in-direction 'right (selected-window)))
                               window-divider-default-right-width
                             0)))
           (y (- top border))
           (child-frame-border (face-attribute 'child-frame-border :background)))
      (set-face-attribute 'child-frame-border t :background foreground)
      (let ((frame (make-frame
                    `((parent-frame . ,(window-frame))
                      (delete-before . ,(window-frame))
                      (minibuffer . nil)
                      (modeline . nil)
                      (left . ,(- left border))
                      (top . ,y)
                      (width . (text-pixels . ,width))
                      (height . (text-pixels . ,height))
                      ;; (height . ,height)
                      (child-frame-border-width . ,border)
                      (internal-border-width . ,border)
                      (background-color . ,background)
                      (horizontal-scroll-bars . nil)
                      (menu-bar-lines . 0)
                      (tool-bar-lines . 0)
                      (desktop-dont-save . t)
                      (unsplittable . nil)
                      (no-other-frame . t)
                      (undecorated . t)
                      (pixelwise . t)
                      (visibility . t)))))
        (set-face-attribute 'child-frame-border t :background child-frame-border)
        frame))))
;; Mini-Frame:1 ends here

;; [[file:config.org::*Vertico][Vertico:1]]
(after! vertico
  ;; settings
  (setq vertico-resize nil        ; How to resize the Vertico minibuffer window.
        vertico-count 10          ; Maximal number of candidates to show.
        vertico-count-format nil) ; No prefix with number of entries

  ;; looks
  (setq vertico-grid-separator
        #("  |  " 2 3 (display (space :width (1))
                               face (:background "#ECEFF1")))
        vertico-group-format
        (concat #(" " 0 1 (face vertico-group-title))
                #(" " 0 1 (face vertico-group-separator))
                #(" %s " 0 4 (face vertico-group-title))
                #(" " 0 1 (face vertico-group-separator
                            display (space :align-to (- right (-1 . right-margin) (- +1)))))))
  (set-face-attribute 'vertico-group-separator nil
                      :strike-through t)
  (set-face-attribute 'vertico-current nil
                      :inherit '(nano-strong nano-subtle))
  (set-face-attribute 'completions-first-difference nil
                      :inherit '(nano-default))

  ;; minibuffer tweaks
  (defun my/vertico--resize-window (height)
    "Resize active minibuffer window to HEIGHT."
      (setq-local truncate-lines t
                  resize-mini-windows 'grow-only
                  max-mini-window-height 1.0)
    (unless (frame-root-window-p (active-minibuffer-window))
      (unless vertico-resize
        (setq height (max height vertico-count)))
      (let* ((window-resize-pixelwise t)
             (dp (- (max (cdr (window-text-pixel-size))
                         (* (default-line-height) (1+ height)))
                    (window-pixel-height))))
        (when (or (and (> dp 0) (/= height 0))
                  (and (< dp 0) (eq vertico-resize t)))
          (window-resize nil dp nil nil 'pixelwise)))))

  (advice-add #'vertico--resize-window :override #'my/vertico--resize-window)

  ;; completion at point
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (defun minibuffer-format-candidate (orig cand prefix suffix index _start)
    (let ((prefix (if (= vertico--index index)
                      "  "
                    "   ")))
      (funcall orig cand prefix suffix index _start)))
  (advice-add #'vertico--format-candidate
             :around #'minibuffer-format-candidate)
  (defun vertico--prompt-selection ()
    "Highlight the prompt"

    (let ((inhibit-modification-hooks t))
      (set-text-properties (minibuffer-prompt-end) (point-max)
                           '(face (nano-strong nano-salient)))))
  (defun minibuffer-vertico-setup ()
    (setq truncate-lines t)
    (setq completion-in-region-function
          (if vertico-mode
              #'consult-completion-in-region
            #'completion--in-region)))

  (add-hook 'vertico-mode-hook #'minibuffer-vertico-setup)
  (add-hook 'minibuffer-setup-hook #'minibuffer-vertico-setup))
;; Vertico:1 ends here

;; [[file:config.org::*Marginalia][Marginalia:1]]
(after! marginalia
  (setq marginalia--ellipsis "…"    ; Nicer ellipsis
        marginalia-align 'right     ; right alignment
        marginalia-align-offset -1)) ; one space on the right
;; Marginalia:1 ends here

;; [[file:config.org::*Elcord][Elcord:1]]
(defun shaunsingh/elcord-buffer-details-format ()
  "Return the buffer details string shown on discord."
  (format "Text is a Magical Thing"))

(use-package! elcord
  :commands elcord-mode
  :config
  (setq elcord-mode-icon-alist '((dashboard-mode . "elisp-mode_icon")
                                 (fundamental-mode . "elisp-mode_icon")
                                 (c-mode . "c-mode_icon")
                                 (c++-mode . "c_-mode_icon")
                                 (crystal-mode . "crystal-mode_icon")
                                 (clojure-mode . "clojure-mode_icon")
                                 (css-mode . "css-mode_icon")
                                 (emacs-lisp-mode . "elisp-mode_icon")
                                 (eshell-mode . "elisp-mode_icon")
                                 (haskell-mode . "haskell-mode_icon")
                                 (haxe-mode . "haxe-mode_icon")
                                 (haskell-interactive-mode . "haskell-mode_icon")
                                 (js-mode . "javascript-mode_icon")
                                 (magit-mode . "magit-mode_icon")
                                 (markdown-mode . "markdown-mode_icon")
                                 (nixos-mode . "nixos-mode_icon")
                                 (latex-mode . "latex-mode_icon")
                                 (text-mode . "elisp-mode_icon")
                                 (org-mode . "org-mode_icon")
                                 ("^slime-.*" . "lisp-mode_icon")
                                 ("^sly-.*$" . "lisp-mode_icon")
                                 (typescript-mode . "typescript-mode_icon")
                                 (writer-mode . "org-mode_icon")
                                 (term-mode . "x-mode_icon")
                                 (shell-mode . "x-mode_icon")
                                 (vterm-mode . "x-mode_icon")))
  (setq elcord-client-id "930927487867834408") ;; You can set your own check elcord's readme
  (setq elcord-quiet t
        elcord-editor-icon "elisp-mode_icon"
        elcord-buffer-details-format-function 'shaunsingh/elcord-buffer-details-format
        elcord-display-buffer-details t
        elcord-display-elapsed nil
        elcord-show-small-icon nil
        elcord-use-major-mode-as-main-icon t
        elcord-refresh-rate 0.25))
;; Elcord:1 ends here

;; [[file:config.org::*Pixel-scroll][Pixel-scroll:1]]
(if (boundp 'mac-mouse-wheel-smooth-scroll)
    (setq  mac-mouse-wheel-smooth-scroll t))

(if (> emacs-major-version 28)
    (pixel-scroll-precision-mode))
;; Pixel-scroll:1 ends here

;; [[file:config.org::*Nano][Nano:1]]
(setq-default line-spacing 0.24)
;; Nano:1 ends here

;; [[file:config.org::*Window Padding][Window Padding:1]]
;; Vertical window divider
(setq-default window-divider-default-right-width 24
              window-divider-default-places 'right-only
              left-margin-width 0
              right-margin-width 0
              window-combination-resize nil) ; Do not resize windows proportionally

(window-divider-mode 1)
;; Window Padding:1 ends here

;; [[file:config.org::*Window Padding][Window Padding:2]]
;; Default frame settings
(setq default-frame-alist '((min-height . 1)  '(height . 45)
                            (min-width  . 1)  '(width  . 81)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 24)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))

(setq initial-frame-alist default-frame-alist)
;; Window Padding:2 ends here

;; [[file:config.org::*Colorscheme][Colorscheme:1]]
(defun shaunsingh/apply-nano-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (nano-light))
    ('dark (nano-dark))))
;; Colorscheme:1 ends here

;; [[file:config.org::*Colorscheme][Colorscheme:2]]
(use-package nano-theme
  :hook (after-init . nano-light)
  :config
  ;; If emacs has been built with system appearance detection
  ;; add a hook to change the theme to match the system
  ;; (if (boundp 'ns-system-appearance-change-functions)
  ;;     (add-hook 'ns-system-appearance-change-functions #'shaunsingh/apply-nano-theme))
  ;; Now to add some missing faces
  (custom-set-faces
   `(flyspell-incorrect ((t (:underline (:color ,nano-light-salient :style line)))))
   `(flyspell-duplicate ((t (:underline (:color ,nano-light-salient :style line)))))

   `(git-gutter:modified ((t (:foreground ,nano-light-salient))))
   `(git-gutter-fr:added ((t (:foreground ,nano-light-popout))))
   `(git-gutter-fr:modified ((t (:foreground ,nano-light-salient))))

   `(lsp-ui-doc-url:added ((t (:background ,nano-light-highlight))))
   `(lsp-ui-doc-background:modified ((t (:background ,nano-light-highlight))))

   `(vterm-color-red ((t (:foreground ,nano-light-critical))))
   `(vterm-color-blue ((t (:foreground ,nano-light-salient))))
   `(vterm-color-green ((t (:foreground ,nano-light-popout))))
   `(vterm-color-yellow ((t (:foreground ,nano-light-popout))))
   `(vterm-color-magenta ((t (:foreground ,nano-light-salient))))

   `(scroll-bar ((t (:background ,nano-light-background))))
   `(child-frame-border ((t (:foreground ,nano-light-faded))))

   `(avy-lead-face-1 ((t (:foreground ,nano-light-subtle))))
   `(avy-lead-face ((t (:foreground ,nano-light-popout :weight bold))))
   `(avy-lead-face-0 ((t (:foreground ,nano-light-salient :weight bold))))))
;; Colorscheme:2 ends here

;; [[file:config.org::*Colorscheme][Colorscheme:3]]
;; (use-package! nano-modeline
;;   :hook (after-init . nano-modeline-mode)
;;   :config
;;   (setq nano-modeline-prefix 'status
;;         nano-modeline-prefix-padding 1
;;         nano-modeline-position 'bottom))

(use-package! minions
  :hook (after-init . minions-mode))

;; Add a zero-width tall character to add padding to modeline
(setq-default mode-line-format
              (cons (propertize "\u200b" 'display '((raise -0.35) (height 1.4))) mode-line-format))
;; Colorscheme:3 ends here

;; [[file:config.org::*SVG-tag-mode][SVG-tag-mode:1]]
(use-package svg-tag-mode
  :commands svg-tag-mode
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                  nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority
                                              :beg 2 :end -1 :margin 0))))

          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


          ;; Citation of the form [cite:@Knuth:1984]
          ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                            (svg-tag-make tag
                                                          :inverse t
                                                          :beg 7 :end -1
                                                          :crop-right t))))
          ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                  (svg-tag-make tag
                                                                :end -1
                                                                :crop-left t))))


          ;; Active date (with or without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s \\)%s>" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s \\(%s>\\)" date-re day-time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (with or without day name, with or without time)
           (,(format "\\(\\[%s\\]\\)" date-re) .
            ((lambda (tag)
               (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
           (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
            ((lambda (tag)
               (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
           (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
            ((lambda (tag)
               (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))
;; SVG-tag-mode:1 ends here

;; [[file:config.org::*Dimming][Dimming:1]]
;; Dim inactive windows
(use-package! dimmer
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.5
        dimmer-adjustment-mode :foreground
        dimmer-use-colorspace :rgb
        dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe))
;; Dimming:1 ends here

;; [[file:config.org::*Dimming][Dimming:2]]
(defun add-list-to-list (dst src)
  "Similar to `add-to-list', but accepts a list as 2nd argument"
  (set dst
       (append (eval dst) src)))

(use-package! focus
  :commands focus-mode
  :config
  ;; add whatever lsp servers you use to this list
  (add-list-to-list 'focus-mode-to-thing
                    '((lua-mode . lsp-folding-range)
                      (rust-mode . lsp-folding-range)
                      (latex-mode . lsp-folding-range)
                      (python-mode . lsp-folding-range))))
;; Dimming:2 ends here

;; [[file:config.org::*Writeroom][Writeroom:1]]
(setq +zen-text-scale 0.8)
;; Writeroom:1 ends here

;; [[file:config.org::*RSS][RSS:1]]
(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)

(after! elfeed-search
  (set-evil-initial-state! 'elfeed-search-mode 'normal))
(after! elfeed-show-mode
  (set-evil-initial-state! 'elfeed-show-mode   'normal))

(after! evil-snipe
  (push 'elfeed-show-mode   evil-snipe-disabled-modes)
  (push 'elfeed-search-mode evil-snipe-disabled-modes))

(after! elfeed
  (elfeed-org)
  (use-package! elfeed-link)
  (setq rmh-elfeed-org-files '("~/org/elfeed.org"))

  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)

  (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground 'nil
                      :weight 'light)

  (defadvice! +rss-elfeed-wrap-h-nicer ()
    "Enhances an elfeed entry's readability by wrapping it to a width of
`fill-column' and centering it with `visual-fill-column-mode'."
    :override #'+rss-elfeed-wrap-h
    (setq-local truncate-lines nil
                shr-width 120
                visual-fill-column-center-text t
                default-text-properties '(line-height 1.1))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (visual-fill-column-mode)
      ;; (setq-local shr-current-font '(:family "Merriweather" :height 1.2))
      (set-buffer-modified-p nil)))

  (defun +rss/elfeed-search-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((elfeed-goodies/tag-column-width 40)
           (elfeed-goodies/feed-source-column-width 30)
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat (mapconcat 'identity tags ",")))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))

           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
      (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))))

  (defun +rss/elfeed-show-refresh--better-style ()
    "Update the buffer to match the selected entry, using a mail-style."
    (interactive)
    (let* ((inhibit-read-only t)
           (title (elfeed-entry-title elfeed-show-entry))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (author (elfeed-meta elfeed-show-entry :author))
           (link (elfeed-entry-link elfeed-show-entry))
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tagsstr (mapconcat #'symbol-name tags ", "))
           (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
           (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
           (type (elfeed-entry-content-type elfeed-show-entry))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (feed-title (elfeed-feed-title feed))
           (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
      (erase-buffer)
      (insert "\n")
      (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
      (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
      (when (and author elfeed-show-entry-author)
        (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
      (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
      (when tags
        (insert (format "%s\n"
                        (propertize tagsstr 'face 'elfeed-search-tag-face))))
      ;; (insert (propertize "Link: " 'face 'message-header-name))
      ;; (elfeed-insert-link link link)
      ;; (insert "\n")
      (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
               do (insert (propertize "Enclosure: " 'face 'message-header-name))
               do (elfeed-insert-link (car enclosure))
               do (insert "\n"))
      (insert "\n")
      (if content
          (if (eq type 'html)
              (elfeed-insert-html content base)
            (insert content))
        (insert (propertize "(empty)\n" 'face 'italic)))
      (goto-char (point-min)))))

(after! elfeed-show
  (require 'url)

  (defvar elfeed-pdf-dir
    (expand-file-name "pdfs/"
                      (file-name-directory (directory-file-name elfeed-enclosure-default-dir))))

  (defvar elfeed-link-pdfs
    '(("https://www.jstatsoft.org/index.php/jss/article/view/v0\\([^/]+\\)" . "https://www.jstatsoft.org/index.php/jss/article/view/v0\\1/v\\1.pdf")
      ("http://arxiv.org/abs/\\([^/]+\\)" . "https://arxiv.org/pdf/\\1.pdf"))
    "List of alists of the form (REGEX-FOR-LINK . FORM-FOR-PDF)")

  (defun elfeed-show-pdf (entry)
    (interactive
     (list (or elfeed-show-entry (elfeed-search-selected :ignore-region))))
    (let ((link (elfeed-entry-link entry))
          (feed-name (plist-get (elfeed-feed-meta (elfeed-entry-feed entry)) :title))
          (title (elfeed-entry-title entry))
          (file-view-function
           (lambda (f)
             (when elfeed-show-entry
               (elfeed-kill-buffer))
             (pop-to-buffer (find-file-noselect f))))
          pdf)

      (let ((file (expand-file-name
                   (concat (subst-char-in-string ?/ ?, title) ".pdf")
                   (expand-file-name (subst-char-in-string ?/ ?, feed-name)
                                     elfeed-pdf-dir))))
        (if (file-exists-p file)
            (funcall file-view-function file)
          (dolist (link-pdf elfeed-link-pdfs)
            (when (and (string-match-p (car link-pdf) link)
                       (not pdf))
              (setq pdf (replace-regexp-in-string (car link-pdf) (cdr link-pdf) link))))
          (if (not pdf)
              (message "No associated PDF for entry")
            (message "Fetching %s" pdf)
            (unless (file-exists-p (file-name-directory file))
              (make-directory (file-name-directory file) t))
            (url-copy-file pdf file)
            (funcall file-view-function file)))))))
;; RSS:1 ends here

;; [[file:config.org::*Ebooks][Ebooks:1]]
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (advice-add 'nov-render-title :override #'ignore)
  (defun +nov-mode-setup ()
    (face-remap-add-relative 'default :height 1.3)
    (setq-local next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 81
                nov-text-width 80)
    (visual-fill-column-mode 1)
    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)
    (add-hook 'nov-mode-hook #'+nov-mode-setup)))
;; Ebooks:1 ends here

;; [[file:config.org::*Org-Mode][Org-Mode:1]]
(after! org
  (setq org-directory "~/org"                     ; let's put files here
        org-ellipsis "  ﬋"                        ; cute icon for folded org blocks
        org-list-allow-alphabetical t             ; have a. A. a) A) list bullets
        org-use-property-inheritance t            ; it's convenient to have properties inherited
        org-catch-invisible-edits 'smart          ; try not to accidently do weird stuff in invisible regions
        org-log-done 'time                        ; having the time a item is done sounds convenient
        org-roam-directory "~/org/roam/"))        ; same thing, for roam
;; Org-Mode:1 ends here

;; [[file:config.org::*Org-Mode][Org-Mode:2]]
(after! org
  (setq org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-inline-src-prettify-results '("⟨" . "⟩")
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t))
;; Org-Mode:2 ends here

;; [[file:config.org::*Org-Mode][Org-Mode:3]]
(after! org
  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:comments . "link"))))
;; Org-Mode:3 ends here

;; [[file:config.org::*Org-Mode][Org-Mode:4]]
(after! org
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))))
;; Org-Mode:4 ends here

;; [[file:config.org::*Org-Mode][Org-Mode:5]]
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))
;; Org-Mode:5 ends here

;; [[file:config.org::*Org-Mode][Org-Mode:6]]
(after! ox
  (org-link-set-parameters "yt" :export #'+org-export-yt)
  (defun +org-export-yt (path desc backend _com)
    (cond ((org-export-derived-backend-p backend 'html)
           (format "<iframe width='440' \
height='335' \
src='https://www.youtube.com/embed/%s' \
frameborder='0' \
allowfullscreen>%s</iframe>" path (or "" desc)))
          ((org-export-derived-backend-p backend 'latex)
           (format "\\href{https://youtu.be/%s}{%s}" path (or desc "youtube")))
          (t (format "https://youtu.be/%s" path)))))
;; Org-Mode:6 ends here

;; [[file:config.org::*HTML export][HTML export:1]]
(defun org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle (expand-file-name "misc/org-css/style.css" doom-private-dir) path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(defun org-inline-js-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.js"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle (expand-file-name "misc/org-css/style.js" doom-private-dir) path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                          "<script type=\"text/javascript\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</script>\n")))))

(defun org-inline-html-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.html"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle (expand-file-name "misc/org-css/style.html" doom-private-dir) path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "\n")))))

(add-hook 'org-export-before-processing-hook 'org-inline-css-hook)
(add-hook 'org-export-before-processing-hook 'org-inline-js-hook)
(add-hook 'org-export-before-processing-hook 'org-inline-html-hook)
;; HTML export:1 ends here

;; [[file:config.org::*HTML export][HTML export:2]]
(after! ox-html
  (setq org-html-mathjax-options
        '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js" )
          (scale "1")
          (autonumber "ams")
          (multlinewidth "85%")
          (tagindent ".8em")
          (tagside "right")))

  (setq org-html-mathjax-template
        "<script>
     MathJax = {
       chtml: {
         scale: %SCALE
       },
       svg: {
         scale: %SCALE,
         fontCache: \"global\"
       },
       tex: {
         tags: \"%AUTONUMBER\",
         multlineWidth: \"%MULTLINEWIDTH\",
         tagSide: \"%TAGSIDE\",
         tagIndent: \"%TAGINDENT\"
       }
     };
     </script>
     <script id=\"MathJax-script\" async
             src=\"%PATH\"></script>"))
;; HTML export:2 ends here

;; [[file:config.org::*HTML export][HTML export:3]]
(use-package! org-preview-html
  :commands org-preview-html-mode
  :config
  (setq org-preview-html-refresh-configuration 'save
        org-preview-html-viewer 'xwidget))
;; HTML export:3 ends here

;; [[file:config.org::*HTML export][HTML export:4]]
(setq org-startup-with-inline-images t)
;; HTML export:4 ends here

;; [[file:config.org::*Org-Roam][Org-Roam:1]]
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
;; Org-Roam:1 ends here

;; [[file:config.org::*Org-Roam][Org-Roam:2]]
(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil))
;; Org-Roam:2 ends here

;; [[file:config.org::*Org-Agenda][Org-Agenda:1]]
(after! org-agenda
  (setq org-agenda-files (list "~/org/school.org"
                               "~/org/todo.org"))
  (setq org-agenda-window-setup 'current-window
        org-agenda-restore-windows-after-quit t
        org-agenda-show-all-dates nil
        org-agenda-time-in-grid t
        org-agenda-show-current-time-in-grid t
        org-agenda-start-on-weekday 1
        org-agenda-span 7
        org-agenda-tags-column 0
        org-agenda-block-separator nil
        org-agenda-category-icon-alist nil
        org-agenda-sticky t)
  (setq org-agenda-prefix-format
        '((agenda . "%i %?-12t%s")
          (todo .   "%i")
          (tags .   "%i")
          (search . "%i")))
  (setq org-agenda-sorting-strategy
        '((agenda deadline-down scheduled-down todo-state-up time-up
                  habit-down priority-down category-keep)
          (todo   priority-down category-keep)
          (tags   timestamp-up priority-down category-keep)
          (search category-keep))))
;; Org-Agenda:1 ends here

;; [[file:config.org::*Font Display][Font Display:1]]
(after! org
  (setq org-agenda-deadline-faces
        '((1.0 . error)
          (1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline))))
;; Font Display:1 ends here

;; [[file:config.org::*Font Display][Font Display:2]]
(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t))
;; Font Display:2 ends here

;; [[file:config.org::*(sub|super)script characters][(sub|super)script characters:1]]
(setq org-export-with-sub-superscripts '{})
;; (sub|super)script characters:1 ends here

;; [[file:config.org::*Make verbatim different to code][Make verbatim different to code:1]]
(after! org
  (setq org-latex-text-markup-alist
        '((bold . "\\textbf{%s}")
          (code . protectedtexttt)
          (italic . "\\emph{%s}")
          (strike-through . "\\sout{%s}")
          (underline . "\\uline{%s}")
          (verbatim . verb))))
;; Make verbatim different to code:1 ends here

;; [[file:config.org::*XKCD][XKCD:1]]
(use-package! xkcd
  :commands (xkcd-get-json
             xkcd-download xkcd-get
             ;; now for funcs from my extension of this pkg
             +xkcd-find-and-copy +xkcd-find-and-view
             +xkcd-fetch-info +xkcd-select)
  :config
  (setq xkcd-cache-dir (expand-file-name "xkcd/" doom-cache-dir)
        xkcd-cache-latest (concat xkcd-cache-dir "latest"))
  (unless (file-exists-p xkcd-cache-dir)
    (make-directory xkcd-cache-dir))
  :general (:states 'normal
            :keymaps 'xkcd-mode-map
            "<right>" #'xkcd-next
            "n"       #'xkcd-next
            "<left>"  #'xkcd-prev
            "N"       #'xkcd-prev
            "r"       #'xkcd-rand
            "a"       #'xkcd-rand ; because image-rotate can interfere
            "t"       #'xkcd-alt-text
            "q"       #'xkcd-kill-buffer
            "o"       #'xkcd-open-browser
            "e"       #'xkcd-open-explanation-browser
            ;; extras
            "s"       #'+xkcd-find-and-view
            "/"       #'+xkcd-find-and-view
            "y"       #'+xkcd-copy))
;; XKCD:1 ends here

;; [[file:config.org::*XKCD][XKCD:2]]
(after! xkcd
  (require 'emacsql-sqlite)

  (defun +xkcd-select ()
    "Prompt the user for an xkcd using `completing-read' and `+xkcd-select-format'. Return the xkcd number or nil"
    (let* (prompt-lines
           (-dummy (maphash (lambda (key xkcd-info)
                              (push (+xkcd-select-format xkcd-info) prompt-lines))
                            +xkcd-stored-info))
           (num (completing-read (format "xkcd (%s): " xkcd-latest) prompt-lines)))
      (if (equal "" num) xkcd-latest
        (string-to-number (replace-regexp-in-string "\\([0-9]+\\).*" "\\1" num)))))

  (defun +xkcd-select-format (xkcd-info)
    "Creates each completing-read line from an xkcd info plist. Must start with the xkcd number"
    (format "%-4s  %-30s %s"
            (propertize (number-to-string (plist-get xkcd-info :num))
                        'face 'counsel-key-binding)
            (plist-get xkcd-info :title)
            (propertize (plist-get xkcd-info :alt)
                        'face '(variable-pitch font-lock-comment-face))))

  (defun +xkcd-fetch-info (&optional num)
    "Fetch the parsed json info for comic NUM. Fetches latest when omitted or 0"
    (require 'xkcd)
    (when (or (not num) (= num 0))
      (+xkcd-check-latest)
      (setq num xkcd-latest))
    (let ((res (or (gethash num +xkcd-stored-info)
                   (puthash num (+xkcd-db-read num) +xkcd-stored-info))))
      (unless res
        (+xkcd-db-write
         (let* ((url (format "https://xkcd.com/%d/info.0.json" num))
                (json-assoc
                 (if (gethash num +xkcd-stored-info)
                     (gethash num +xkcd-stored-info)
                   (json-read-from-string (xkcd-get-json url num)))))
           json-assoc))
        (setq res (+xkcd-db-read num)))
      res))

  ;; since we've done this, we may as well go one little step further
  (defun +xkcd-find-and-copy ()
    "Prompt for an xkcd using `+xkcd-select' and copy url to clipboard"
    (interactive)
    (+xkcd-copy (+xkcd-select)))

  (defun +xkcd-copy (&optional num)
    "Copy a url to xkcd NUM to the clipboard"
    (interactive "i")
    (let ((num (or num xkcd-cur)))
      (gui-select-text (format "https://xkcd.com/%d" num))
      (message "xkcd.com/%d copied to clipboard" num)))

  (defun +xkcd-find-and-view ()
    "Prompt for an xkcd using `+xkcd-select' and view it"
    (interactive)
    (xkcd-get (+xkcd-select))
    (switch-to-buffer "*xkcd*"))

  (defvar +xkcd-latest-max-age (* 60 60) ; 1 hour
    "Time after which xkcd-latest should be refreshed, in seconds")

  ;; initialise `xkcd-latest' and `+xkcd-stored-info' with latest xkcd
  (add-transient-hook! '+xkcd-select
    (require 'xkcd)
    (+xkcd-fetch-info xkcd-latest)
    (setq +xkcd-stored-info (+xkcd-db-read-all)))

  (add-transient-hook! '+xkcd-fetch-info
    (xkcd-update-latest))

  (defun +xkcd-check-latest ()
    "Use value in `xkcd-cache-latest' as long as it isn't older thabn `+xkcd-latest-max-age'"
    (unless (and (file-exists-p xkcd-cache-latest)
                 (< (- (time-to-seconds (current-time))
                       (time-to-seconds (file-attribute-modification-time (file-attributes xkcd-cache-latest))))
                    +xkcd-latest-max-age))
      (let* ((out (xkcd-get-json "http://xkcd.com/info.0.json" 0))
             (json-assoc (json-read-from-string out))
             (latest (cdr (assoc 'num json-assoc))))
        (when (/= xkcd-latest latest)
          (+xkcd-db-write json-assoc)
          (with-current-buffer (find-file xkcd-cache-latest)
            (setq xkcd-latest latest)
            (erase-buffer)
            (insert (number-to-string latest))
            (save-buffer)
            (kill-buffer (current-buffer)))))
      (shell-command (format "touch %s" xkcd-cache-latest))))

  (defvar +xkcd-stored-info (make-hash-table :test 'eql)
    "Basic info on downloaded xkcds, in the form of a hashtable")

  (defadvice! xkcd-get-json--and-cache (url &optional num)
    "Fetch the Json coming from URL.
If the file NUM.json exists, use it instead.
If NUM is 0, always download from URL.
The return value is a string."
    :override #'xkcd-get-json
    (let* ((file (format "%s%d.json" xkcd-cache-dir num))
           (cached (and (file-exists-p file) (not (eq num 0))))
           (out (with-current-buffer (if cached
                                         (find-file file)
                                       (url-retrieve-synchronously url))
                  (goto-char (point-min))
                  (unless cached (re-search-forward "^$"))
                  (prog1
                      (buffer-substring-no-properties (point) (point-max))
                    (kill-buffer (current-buffer))))))
      (unless (or cached (eq num 0))
        (xkcd-cache-json num out))
      out))

  (defadvice! +xkcd-get (num)
    "Get the xkcd number NUM."
    :override 'xkcd-get
    (interactive "nEnter comic number: ")
    (xkcd-update-latest)
    (get-buffer-create "*xkcd*")
    (switch-to-buffer "*xkcd*")
    (xkcd-mode)
    (let (buffer-read-only)
      (erase-buffer)
      (setq xkcd-cur num)
      (let* ((xkcd-data (+xkcd-fetch-info num))
             (num (plist-get xkcd-data :num))
             (img (plist-get xkcd-data :img))
             (safe-title (plist-get xkcd-data :safe-title))
             (alt (plist-get xkcd-data :alt))
             title file)
        (message "Getting comic...")
        (setq file (xkcd-download img num))
        (setq title (format "%d: %s" num safe-title))
        (insert (propertize title
                            'face 'outline-1))
        (center-line)
        (insert "\n")
        (xkcd-insert-image file num)
        (if (eq xkcd-cur 0)
            (setq xkcd-cur num))
        (setq xkcd-alt alt)
        (message "%s" title))))

  (defconst +xkcd-db--sqlite-available-p
    (with-demoted-errors "+org-xkcd initialization: %S"
      (emacsql-sqlite-ensure-binary)
      t))

  (defvar +xkcd-db--connection (make-hash-table :test #'equal)
    "Database connection to +org-xkcd database.")

  (defun +xkcd-db--get ()
    "Return the sqlite db file."
    (expand-file-name "xkcd.db" xkcd-cache-dir))

  (defun +xkcd-db--get-connection ()
    "Return the database connection, if any."
    (gethash (file-truename xkcd-cache-dir)
             +xkcd-db--connection))

  (defconst +xkcd-db--table-schema
    '((xkcds
       [(num integer :unique :primary-key)
        (year        :not-null)
        (month       :not-null)
        (link        :not-null)
        (news        :not-null)
        (safe_title  :not-null)
        (title       :not-null)
        (transcript  :not-null)
        (alt         :not-null)
        (img         :not-null)])))

  (defun +xkcd-db--init (db)
    "Initialize database DB with the correct schema and user version."
    (emacsql-with-transaction db
      (pcase-dolist (`(,table . ,schema) +xkcd-db--table-schema)
        (emacsql db [:create-table $i1 $S2] table schema))))

  (defun +xkcd-db ()
    "Entrypoint to the +org-xkcd sqlite database.
Initializes and stores the database, and the database connection.
Performs a database upgrade when required."
    (unless (and (+xkcd-db--get-connection)
                 (emacsql-live-p (+xkcd-db--get-connection)))
      (let* ((db-file (+xkcd-db--get))
             (init-db (not (file-exists-p db-file))))
        (make-directory (file-name-directory db-file) t)
        (let ((conn (emacsql-sqlite db-file)))
          (set-process-query-on-exit-flag (emacsql-process conn) nil)
          (puthash (file-truename xkcd-cache-dir)
                   conn
                   +xkcd-db--connection)
          (when init-db
            (+xkcd-db--init conn)))))
    (+xkcd-db--get-connection))

  (defun +xkcd-db-query (sql &rest args)
    "Run SQL query on +org-xkcd database with ARGS.
SQL can be either the emacsql vector representation, or a string."
    (if  (stringp sql)
        (emacsql (+xkcd-db) (apply #'format sql args))
      (apply #'emacsql (+xkcd-db) sql args)))

  (defun +xkcd-db-read (num)
    (when-let ((res
                (car (+xkcd-db-query [:select * :from xkcds
                                      :where (= num $s1)]
                                     num
                                     :limit 1))))
      (+xkcd-db-list-to-plist res)))

  (defun +xkcd-db-read-all ()
    (let ((xkcd-table (make-hash-table :test 'eql :size 4000)))
      (mapcar (lambda (xkcd-info-list)
                (puthash (car xkcd-info-list) (+xkcd-db-list-to-plist xkcd-info-list) xkcd-table))
              (+xkcd-db-query [:select * :from xkcds]))
      xkcd-table))

  (defun +xkcd-db-list-to-plist (xkcd-datalist)
    `(:num ,(nth 0 xkcd-datalist)
      :year ,(nth 1 xkcd-datalist)
      :month ,(nth 2 xkcd-datalist)
      :link ,(nth 3 xkcd-datalist)
      :news ,(nth 4 xkcd-datalist)
      :safe-title ,(nth 5 xkcd-datalist)
      :title ,(nth 6 xkcd-datalist)
      :transcript ,(nth 7 xkcd-datalist)
      :alt ,(nth 8 xkcd-datalist)
      :img ,(nth 9 xkcd-datalist)))

  (defun +xkcd-db-write (data)
    (+xkcd-db-query [:insert-into xkcds
                     :values $v1]
                    (list (vector
                           (cdr (assoc 'num        data))
                           (cdr (assoc 'year       data))
                           (cdr (assoc 'month      data))
                           (cdr (assoc 'link       data))
                           (cdr (assoc 'news       data))
                           (cdr (assoc 'safe_title data))
                           (cdr (assoc 'title      data))
                           (cdr (assoc 'transcript data))
                           (cdr (assoc 'alt        data))
                           (cdr (assoc 'img        data)))))))
;; XKCD:2 ends here

;; [[file:config.org::*XKCD][XKCD:3]]
(after! org
  (org-link-set-parameters "xkcd"
                           :image-data-fun #'+org-xkcd-image-fn
                           :follow #'+org-xkcd-open-fn
                           :export #'+org-xkcd-export
                           :complete #'+org-xkcd-complete)

  (defun +org-xkcd-open-fn (link)
    (+org-xkcd-image-fn nil link nil))

  (defun +org-xkcd-image-fn (protocol link description)
    "Get image data for xkcd num LINK"
    (let* ((xkcd-info (+xkcd-fetch-info (string-to-number link)))
           (img (plist-get xkcd-info :img))
           (alt (plist-get xkcd-info :alt)))
      (message alt)
      (+org-image-file-data-fn protocol (xkcd-download img (string-to-number link)) description)))

  (defun +org-xkcd-export (num desc backend _com)
    "Convert xkcd to html/LaTeX form"
    (let* ((xkcd-info (+xkcd-fetch-info (string-to-number num)))
           (img (plist-get xkcd-info :img))
           (alt (plist-get xkcd-info :alt))
           (title (plist-get xkcd-info :title))
           (file (xkcd-download img (string-to-number num))))
      (cond ((org-export-derived-backend-p backend 'html)
             (format "<img class='invertible' src='%s' title=\"%s\" alt='%s'>" img (subst-char-in-string ?\" ?“ alt) title))
            ((org-export-derived-backend-p backend 'latex)
             (format "\\begin{figure}[!htb]
    \\centering
    \\includegraphics[scale=0.4]{%s}%s
  \\end{figure}" file (if (equal desc (format "xkcd:%s" num)) ""
                        (format "\n  \\caption*{\\label{xkcd:%s} %s}"
                                num
                                (or desc
                                    (format "\\textbf{%s} %s" title alt))))))
            (t (format "https://xkcd.com/%s" num)))))

  (defun +org-xkcd-complete (&optional arg)
    "Complete xkcd using `+xkcd-stored-info'"
    (format "xkcd:%d" (+xkcd-select))))
;; XKCD:3 ends here

;; [[file:config.org::*Calc][Calc:1]]
(map! :map calc-mode-map
      :after calc
      :localleader
      :desc "Embedded calc (toggle)" "e" #'calc-embedded)
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Embedded calc (toggle)" "E" #'calc-embedded)
(map! :map latex-mode-map
      :after latex
      :localleader
      :desc "Embedded calc (toggle)" "e" #'calc-embedded)
;; Calc:1 ends here

;; [[file:config.org::*Calc][Calc:2]]
(defvar calc-embedded-trail-window nil)
(defvar calc-embedded-calculator-window nil)

(defadvice! calc-embedded-with-side-pannel (&rest _)
  :after #'calc-do-embedded
  (when calc-embedded-trail-window
    (ignore-errors
      (delete-window calc-embedded-trail-window))
    (setq calc-embedded-trail-window nil))
  (when calc-embedded-calculator-window
    (ignore-errors
      (delete-window calc-embedded-calculator-window))
    (setq calc-embedded-calculator-window nil))
  (when (and calc-embedded-info
             (> (* (window-width) (window-height)) 1200))
    (let ((main-window (selected-window))
          (vertical-p (> (window-width) 80)))
      (select-window
       (setq calc-embedded-trail-window
             (if vertical-p
                 (split-window-horizontally (- (max 30 (/ (window-width) 3))))
               (split-window-vertically (- (max 8 (/ (window-height) 4)))))))
      (switch-to-buffer "*Calc Trail*")
      (select-window
       (setq calc-embedded-calculator-window
             (if vertical-p
                 (split-window-vertically -6)
               (split-window-horizontally (- (/ (window-width) 2))))))
      (switch-to-buffer "*Calculator*")
      (select-window main-window))))
;; Calc:2 ends here

;; [[file:config.org::*Dictionaries][Dictionaries:1]]
(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))

(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))
;; Dictionaries:1 ends here

;; [[file:config.org::*PDF-Tools][PDF-Tools:1]]
(use-package! pdf-view
  :hook (pdf-tools-enabled . pdf-view-themed-minor-mode)
  :config
  (setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil
      pdf-view-display-size 'fit-page))
;; PDF-Tools:1 ends here

;; [[file:config.org::*View Exported File][View Exported File:1]]
(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-bufferpop-to-buffer (or (find-buffer-visiting output-file)
                                          (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")
;; View Exported File:1 ends here

;; [[file:config.org::*\(\LaTeX\) highlighting in Org-mode][\(\LaTeX\) highlighting in Org-mode:1]]
(after! org
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))
;; \(\LaTeX\) highlighting in Org-mode:1 ends here

;; [[file:config.org::*\(\LaTeX\) highlighting in Org-mode][\(\LaTeX\) highlighting in Org-mode:2]]
(defun +org-mode--fontlock-only-mode ()
  "Just apply org-mode's font-lock once."
  (let (org-mode-hook
        org-hide-leading-stars
        org-hide-emphasis-markers)
    (org-set-font-lock-defaults)
    (font-lock-ensure))
  (setq-local major-mode #'fundamental-mode))

(defun +org-export-babel-mask-org-config (_backend)
  "Use `+org-mode--fontlock-only-mode' instead of `org-mode'."
  (setq-local org-src-lang-modes
              (append org-src-lang-modes
                      (list (cons "org" #'+org-mode--fontlock-only)))))

(add-hook 'org-export-before-processing-hook #'+org-export-babel-mask-org-config)
;; \(\LaTeX\) highlighting in Org-mode:2 ends here

;; [[file:config.org::*Tectonic][Tectonic:1]]
(after! org
  (setq-default org-latex-pdf-process '("tectonic -Z shell-escape --outdir=%o %f")))
;; Tectonic:1 ends here

;; [[file:config.org::*Preambles][Preambles:1]]
(defvar org-latex-caption-preamble "
\\usepackage{subcaption}
\\usepackage[hypcap=true]{caption}
\\setkomafont{caption}{\\sffamily\\small}
\\setkomafont{captionlabel}{\\upshape\\bfseries}
\\captionsetup{justification=raggedright,singlelinecheck=true}
\\usepackage{capt-of} % required by Org
"
  "Preamble that improves captions.")

(defvar org-latex-checkbox-preamble "
\\newcommand{\\checkboxUnchecked}{$\\square$}
\\newcommand{\\checkboxTransitive}{\\rlap{\\raisebox{-0.1ex}{\\hspace{0.35ex}\\Large\\textbf -}}$\\square$}
\\newcommand{\\checkboxChecked}{\\rlap{\\raisebox{0.2ex}{\\hspace{0.35ex}\\scriptsize \\ding{52}}}$\\square$}
"
  "Preamble that improves checkboxes.")

(defvar org-latex-box-preamble "
% args = #1 Name, #2 Colour, #3 Ding, #4 Label
\\newcommand{\\defsimplebox}[4]{%
  \\definecolor{#1}{HTML}{#2}
  \\newenvironment{#1}[1][]
  {%
    \\par\\vspace{-0.7\\baselineskip}%
    \\textcolor{#1}{#3} \\textcolor{#1}{\\textbf{\\def\\temp{##1}\\ifx\\temp\\empty#4\\else##1\\fi}}%
    \\vspace{-0.8\\baselineskip}
    \\begin{addmargin}[1em]{1em}
  }{%
    \\end{addmargin}
    \\vspace{-0.5\\baselineskip}
  }%
}
"
  "Preamble that provides a macro for custom boxes.")
;; Preambles:1 ends here

;; [[file:config.org::*Conditional features][Conditional features:1]]
(defvar org-latex-italic-quotes t
  "Make \"quote\" environments italic.")
(defvar org-latex-par-sep t
  "Vertically seperate paragraphs, and remove indentation.")

(defvar org-latex-conditional-features
  '(("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]\\|\\\\\\]\\)+?\\.\\(?:eps\\|pdf\\|png\\|jpeg\\|jpg\\|jbig2\\)\\]\\]" . image)
    ("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]+?\\|\\\\\\]\\)\\.svg\\]\\]\\|\\\\includesvg" . svg)
    ("^[ \t]*|" . table)
    ("cref:\\|\\cref{\\|\\[\\[[^\\]]+\\]\\]" . cleveref)
    ("[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]" . acronym)
    ("\\+[^ ].*[^ ]\\+\\|_[^ ].*[^ ]_\\|\\\\uu?line\\|\\\\uwave\\|\\\\sout\\|\\\\xout\\|\\\\dashuline\\|\\dotuline\\|\\markoverwith" . underline)
    (":float wrap" . float-wrap)
    (":float sideways" . rotate)
    ("^[ \t]*#\\+caption:\\|\\\\caption" . caption)
    ("\\[\\[xkcd:" . (image caption))
    ((and org-latex-italic-quotes "^[ \t]*#\\+begin_quote\\|\\\\begin{quote}") . italic-quotes)
    (org-latex-par-sep . par-sep)
    ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\|[A-Za-z]+[.)]\\) \\[[ -X]\\]" . checkbox)
    ("^[ \t]*#\\+begin_warning\\|\\\\begin{warning}" . box-warning)
    ("^[ \t]*#\\+begin_info\\|\\\\begin{info}"       . box-info)
    ("^[ \t]*#\\+begin_success\\|\\\\begin{success}" . box-success)
    ("^[ \t]*#\\+begin_error\\|\\\\begin{error}"     . box-error))
  "Org feature tests and associated LaTeX feature flags.

Alist where the car is a test for the presense of the feature,
and the cdr is either a single feature symbol or list of feature symbols.

When a string, it is used as a regex search in the buffer.
The feature is registered as present when there is a match.

The car can also be a
- symbol, the value of which is fetched
- function, which is called with info as an argument
- list, which is `eval'uated

If the symbol, function, or list produces a string: that is used as a regex
search in the buffer. Otherwise any non-nil return value will indicate the
existance of the feature.")

(defvar org-latex-feature-implementations
  '((image         :snippet "\\usepackage{graphicx}" :order 2)
    (svg           :snippet "\\usepackage{svg}" :order 2)
    (table         :snippet "\\usepackage{longtable}\n\\usepackage{booktabs}" :order 2)
    (cleveref      :snippet "\\usepackage[capitalize]{cleveref}" :order 1)
    (underline     :snippet "\\usepackage[normalem]{ulem}" :order 0.5)
    (float-wrap    :snippet "\\usepackage{wrapfig}" :order 2)
    (rotate        :snippet "\\usepackage{rotating}" :order 2)
    (caption       :snippet org-latex-caption-preamble :order 2.1)
    (acronym       :snippet "\\newcommand{\\acr}[1]{\\protect\\textls*[110]{\\scshape #1}}\n\\newcommand{\\acrs}{\\protect\\scalebox{.91}[.84]{\\hspace{0.15ex}s}}" :order 0.4)
    (italic-quotes :snippet "\\renewcommand{\\quote}{\\list{}{\\rightmargin\\leftmargin}\\item\\relax\\em}\n" :order 0.5)
    (par-sep       :snippet "\\setlength{\\parskip}{\\baselineskip}\n\\setlength{\\parindent}{0pt}\n" :order 0.5)
    (.pifont       :snippet "\\usepackage{pifont}")
    (checkbox      :requires .pifont :order 3
                   :snippet (concat (unless (memq 'maths features)
                                      "\\usepackage{amssymb} % provides \\square")
                                    org-latex-checkbox-preamble))
    (.fancy-box    :requires .pifont    :snippet org-latex-box-preamble :order 3.9)
    (box-warning   :requires .fancy-box :snippet "\\defsimplebox{warning}{e66100}{\\ding{68}}{Warning}" :order 4)
    (box-info      :requires .fancy-box :snippet "\\defsimplebox{info}{3584e4}{\\ding{68}}{Information}" :order 4)
    (box-success   :requires .fancy-box :snippet "\\defsimplebox{success}{26a269}{\\ding{68}}{\\vspace{-\\baselineskip}}" :order 4)
    (box-error     :requires .fancy-box :snippet "\\defsimplebox{error}{c01c28}{\\ding{68}}{Important}" :order 4))
  "LaTeX features and details required to implement them.

List where the car is the feature symbol, and the rest forms a plist with the
following keys:
- :snippet, which may be either
  - a string which should be included in the preamble
  - a symbol, the value of which is included in the preamble
  - a function, which is evaluated with the list of feature flags as its
    single argument. The result of which is included in the preamble
  - a list, which is passed to `eval', with a list of feature flags available
    as \"features\"

- :requires, a feature or list of features that must be available
- :when, a feature or list of features that when all available should cause this
    to be automatically enabled.
- :prevents, a feature or list of features that should be masked
- :order, for when ordering is important. Lower values appear first.
    The default is 0.

Features that start with ! will be eagerly loaded, i.e. without being detected.")
;; Conditional features:1 ends here

;; [[file:config.org::*Conditional features][Conditional features:2]]
(defun org-latex-detect-features (&optional buffer info)
  "List features from `org-latex-conditional-features' detected in BUFFER."
  (let ((case-fold-search nil))
    (with-current-buffer (or buffer (current-buffer))
      (delete-dups
       (mapcan (lambda (construct-feature)
                 (when (let ((out (pcase (car construct-feature)
                                    ((pred stringp) (car construct-feature))
                                    ((pred functionp) (funcall (car construct-feature) info))
                                    ((pred listp) (eval (car construct-feature)))
                                    ((pred symbolp) (symbol-value (car construct-feature)))
                                    (_ (user-error "org-latex-conditional-features key %s unable to be used" (car construct-feature))))))
                         (if (stringp out)
                             (save-excursion
                               (goto-char (point-min))
                               (re-search-forward out nil t))
                           out))
                   (if (listp (cdr construct-feature)) (cdr construct-feature) (list (cdr construct-feature)))))
               org-latex-conditional-features)))))
;; Conditional features:2 ends here

;; [[file:config.org::*Conditional features][Conditional features:3]]
(defun org-latex-expand-features (features)
  "For each feature in FEATURES process :requires, :when, and :prevents keywords and sort according to :order."
  (dolist (feature features)
    (unless (assoc feature org-latex-feature-implementations)
      (error "Feature %s not provided in org-latex-feature-implementations" feature)))
  (setq current features)
  (while current
    (when-let ((requirements (plist-get (cdr (assq (car current) org-latex-feature-implementations)) :requires)))
      (setcdr current (if (listp requirements)
                          (append requirements (cdr current))
                        (cons requirements (cdr current)))))
    (setq current (cdr current)))
  (dolist (potential-feature
           (append features (delq nil (mapcar (lambda (feat)
                                                (when (plist-get (cdr feat) :eager)
                                                  (car feat)))
                                              org-latex-feature-implementations))))
    (when-let ((prerequisites (plist-get (cdr (assoc potential-feature org-latex-feature-implementations)) :when)))
      (setf features (if (if (listp prerequisites)
                             (cl-every (lambda (preq) (memq preq features)) prerequisites)
                           (memq prerequisites features))
                         (append (list potential-feature) features)
                       (delq potential-feature features)))))
  (dolist (feature features)
    (when-let ((prevents (plist-get (cdr (assoc feature org-latex-feature-implementations)) :prevents)))
      (setf features (cl-set-difference features (if (listp prevents) prevents (list prevents))))))
  (sort (delete-dups features)
        (lambda (feat1 feat2)
          (if (< (or (plist-get (cdr (assoc feat1 org-latex-feature-implementations)) :order) 1)
                 (or (plist-get (cdr (assoc feat2 org-latex-feature-implementations)) :order) 1))
              t nil))))
;; Conditional features:3 ends here

;; [[file:config.org::*Conditional features][Conditional features:4]]
(defun org-latex-generate-features-preamble (features)
  "Generate the LaTeX preamble content required to provide FEATURES.
This is done according to `org-latex-feature-implementations'"
  (let ((expanded-features (org-latex-expand-features features)))
    (concat
     (format "\n%% features: %s\n" expanded-features)
     (mapconcat (lambda (feature)
                  (when-let ((snippet (plist-get (cdr (assoc feature org-latex-feature-implementations)) :snippet)))
                    (concat
                     (pcase snippet
                       ((pred stringp) snippet)
                       ((pred functionp) (funcall snippet features))
                       ((pred listp) (eval `(let ((features ',features)) (,@snippet))))
                       ((pred symbolp) (symbol-value snippet))
                       (_ (user-error "org-latex-feature-implementations :snippet value %s unable to be used" snippet)))
                     "\n")))
                expanded-features
                "")
     "% end features\n")))
;; Conditional features:4 ends here

;; [[file:config.org::*Conditional features][Conditional features:5]]
(defvar info--tmp nil)

(defadvice! org-latex-save-info (info &optional t_ s_)
  :before #'org-latex-make-preamble
  (setq info--tmp info))

(defadvice! org-splice-latex-header-and-generated-preamble-a (orig-fn tpl def-pkg pkg snippets-p &optional extra)
  "Dynamically insert preamble content based on `org-latex-conditional-preambles'."
  :around #'org-splice-latex-header
  (let ((header (funcall orig-fn tpl def-pkg pkg snippets-p extra)))
    (if snippets-p header
      (concat header
              (org-latex-generate-features-preamble (org-latex-detect-features nil info--tmp))
              "\n"))))
;; Conditional features:5 ends here

;; [[file:config.org::*Class templates][Class templates:1]]
(after! ox-latex
  (let* ((article-sections '(("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")
                             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
         (book-sections (append '(("\\chapter{%s}" . "\\chapter*{%s}"))
                                article-sections))
         (hanging-secnum-preamble "
\\renewcommand\\sectionformat{\\llap{\\thesection\\autodot\\enskip}}
\\renewcommand\\subsectionformat{\\llap{\\thesubsection\\autodot\\enskip}}
\\renewcommand\\subsubsectionformat{\\llap{\\thesubsubsection\\autodot\\enskip}}
")
         (big-chap-preamble "
\\RedeclareSectionCommand[afterindent=false, beforeskip=0pt, afterskip=0pt, innerskip=0pt]{chapter}
\\setkomafont{chapter}{\\normalfont\\Huge}
\\renewcommand*{\\chapterheadstartvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterheadendvskip}{\\vspace*{0\\baselineskip}}
\\renewcommand*{\\chapterformat}{%
  \\fontsize{60}{30}\\selectfont\\rlap{\\hspace{6pt}\\thechapter}}
\\renewcommand*\\chapterlinesformat[3]{%
  \\parbox[b]{\\dimexpr\\textwidth-0.5em\\relax}{%
    \\raggedleft{{\\large\\scshape\\bfseries\\chapapp}\\vspace{-0.5ex}\\par\\Huge#3}}%
    \\hfill\\makebox[0pt][l]{#2}}
"))
    (setcdr (assoc "article" org-latex-classes)
            `(,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
              ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("report" ,(concat "\\documentclass{scrartcl}" hanging-secnum-preamble)
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("book" ,(concat "\\documentclass[twoside=false]{scrbook}"
                                   big-chap-preamble hanging-secnum-preamble)
                   ,@book-sections))
    (add-to-list 'org-latex-classes
                 `("blank" "[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc-article" "\\documentclass[article,code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@article-sections))
    (add-to-list 'org-latex-classes
                 `("bmc" "\\documentclass[code,maths]{bmc}\n[NO-DEFAULT-PACKAGES]\n[NO-PACKAGES]\n[EXTRA]"
                   ,@book-sections))))

(after! ox-latex
  (setq org-latex-tables-booktabs t
        org-latex-hyperref-template "\\colorlet{greenyblue}{blue!70!green}
\\colorlet{blueygreen}{blue!40!green}
\\providecolor{link}{named}{greenyblue}
\\providecolor{cite}{named}{blueygreen}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=,
  urlcolor=link,
  citecolor=cite\n}
\\urlstyle{same}
"
        org-latex-reference-command "\\cref{%s}"))
;; Class templates:1 ends here

;; [[file:config.org::*Adjust default packages][Adjust default packages:1]]
(after! org
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          ("" "fontspec" t)
          ("" "xcolor" nil)
          ("" "hyperref" nil)
          ("" "firamath-otf" t)
          "\\setmonofont{Liga SFMono Nerd Font}"
          "\\setmainfont{IBM Plex Sans}")))
;; Adjust default packages:1 ends here

;; [[file:config.org::*Cover page][Cover page:1]]
(after! org
  (defvar org-latex-cover-page 'auto
    "When t, use a cover page by default.
   When auto, use a cover page when the document's wordcount exceeds
   `org-latex-cover-page-wordcount-threshold'.

   Set with #+option: coverpage:{yes,auto,no} in org buffers.")
  (defvar org-latex-cover-page-wordcount-threshold 5000
    "Document word count at which a cover page will be used automatically.
   This condition is applied when cover page option is set to auto.")
  (defvar org-latex-subtitle-coverpage-format "\\\\\\bigskip\n\\LARGE\\mdseries\\itshape\\color{black!80} %s\\par"
    "Variant of `org-latex-subtitle-format' to use with the cover page.")
  (defvar org-latex-cover-page-maketitle "
\\usepackage{tikz}
\\usetikzlibrary{shapes.geometric}
\\usetikzlibrary{calc}

\\newsavebox\\orgicon
\\begin{lrbox}{\\orgicon}
  \\begin{tikzpicture}[y=0.80pt, x=0.80pt, inner sep=0pt, outer sep=0pt]
    \\path[fill=black!6] (16.15,24.00) .. controls (15.58,24.00) and (13.99,20.69) .. (12.77,18.06)arc(215.55:180.20:2.19) .. controls (12.33,19.91) and (11.27,19.09) .. (11.43,18.05) .. controls (11.36,18.09) and (10.17,17.83) .. (10.17,17.82) .. controls (9.94,18.75) and (9.37,19.44) .. (9.02,18.39) .. controls (8.32,16.72) and (8.14,15.40) .. (9.13,13.80) .. controls (8.22,9.74) and (2.18,7.75) .. (2.81,4.47) .. controls (2.99,4.47) and (4.45,0.99) .. (9.15,2.41) .. controls (14.71,3.99) and (17.77,0.30) .. (18.13,0.04) .. controls (18.65,-0.49) and (16.78,4.61) .. (12.83,6.90) .. controls (10.49,8.18) and (11.96,10.38) .. (12.12,11.15) .. controls (12.12,11.15) and (14.00,9.84) .. (15.36,11.85) .. controls (16.58,11.53) and (17.40,12.07) .. (18.46,11.69) .. controls (19.10,11.41) and (21.79,11.58) .. (20.79,13.08) .. controls (20.79,13.08) and (21.71,13.90) .. (21.80,13.99) .. controls (21.97,14.75) and (21.59,14.91) .. (21.47,15.12) .. controls (21.44,15.60) and (21.04,15.79) .. (20.55,15.44) .. controls (19.45,15.64) and (18.36,15.55) .. (17.83,15.59) .. controls (16.65,15.76) and (15.67,16.38) .. (15.67,16.38) .. controls (15.40,17.19) and (14.82,17.01) .. (14.09,17.32) .. controls (14.70,18.69) and (14.76,19.32) .. (15.50,21.32) .. controls (15.76,22.37) and (16.54,24.00) .. (16.15,24.00) -- cycle(7.83,16.74) .. controls (6.83,15.71) and (5.72,15.70) .. (4.05,15.42) .. controls (2.75,15.19) and (0.39,12.97) .. (0.02,10.68) .. controls (-0.02,10.07) and (-0.06,8.50) .. (0.45,7.18) .. controls (0.94,6.05) and (1.27,5.45) .. (2.29,4.85) .. controls (1.41,8.02) and (7.59,10.18) .. (8.55,13.80) -- (8.55,13.80) .. controls (7.73,15.00) and (7.80,15.64) .. (7.83,16.74) -- cycle;
  \\end{tikzpicture}
\\end{lrbox}

\\makeatletter
\\g@addto@macro\\tableofcontents{\\clearpage}
\\renewcommand\\maketitle{
  \\thispagestyle{empty}
  \\hyphenpenalty=10000 % hyphens look bad in titles
  \\renewcommand{\\baselinestretch}{1.1}
  \\let\\oldtoday\\today
  \\renewcommand{\\today}{\\LARGE\\number\\year\\\\\\large%
    \\ifcase \\month \\or Jan\\or Feb\\or Mar\\or Apr\\or May \\or Jun\\or Jul\\or Aug\\or Sep\\or Oct\\or Nov\\or Dec\\fi
    ~\\number\\day}
  \\begin{tikzpicture}[remember picture,overlay]
    %% Background Polygons %%
    \\foreach \\i in {2.5,...,22} % bottom left
    {\\node[rounded corners,black!3.5,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.west)+(2.5,-4.2)$) {} ;}
    \\foreach \\i in {0.5,...,22} % top left
    {\\node[rounded corners,black!5,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {} ;}
    \\node[rounded corners,fill=black!4,regular polygon,regular polygon sides=6, minimum size=5.5 cm,ultra thick] at ($(current page.north west)+(2.5,2)$) {};
    \\foreach \\i in {0.5,...,24} % top right
    {\\node[rounded corners,black!2,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {} ;}
    \\node[fill=black!3,rounded corners,regular polygon,regular polygon sides=6, minimum size=2.5 cm,ultra thick] at ($(current page.north east)+(0,-8.5)$) {};
    \\foreach \\i in {21,...,3} % bottom right
    {\\node[black!3,rounded corners,draw,regular polygon,regular polygon sides=6, minimum size=\\i cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {} ;}
    \\node[fill=black!3,rounded corners,regular polygon,regular polygon sides=6, minimum size=2 cm,ultra thick] at ($(current page.south east)+(-1.5,0.75)$) {};
    \\node[align=center, scale=1.4] at ($(current page.south east)+(-1.5,0.75)$) {\\usebox\\orgicon};
    %% Text %%
    \\node[left, align=right, black, text width=0.8\\paperwidth, minimum height=3cm, rounded corners,font=\\Huge\\bfseries] at ($(current page.north east)+(-2,-8.5)$)
    {\\@title};
    \\node[left, align=right, black, text width=0.8\\paperwidth, minimum height=2cm, rounded corners, font=\\Large] at ($(current page.north east)+(-2,-11.8)$)
    {\\scshape \\@author};
    \\renewcommand{\\baselinestretch}{0.75}
    \\node[align=center,rounded corners,fill=black!3,text=black,regular polygon,regular polygon sides=6, minimum size=2.5 cm,inner sep=0, font=\\Large\\bfseries ] at ($(current page.west)+(2.5,-4.2)$)
    {\\@date};
  \\end{tikzpicture}
  \\let\\today\\oldtoday
  \\clearpage}
\\makeatother
   "
    "LaTeX snippet for the preamble that sets \\maketitle to produce a cover page.")
  (after! ox
    (eval '(cl-pushnew '(:latex-cover-page nil "coverpage" org-latex-cover-page)
                       (org-export-backend-options (org-export-get-backend 'latex)))))

  (defun org-latex-cover-page-p ()
    "Whether a cover page should be used when exporting this Org file."
    (pcase (or (car
                (delq nil
                      (mapcar
                       (lambda (opt-line)
                         (plist-get (org-export--parse-option-keyword opt-line 'latex) :latex-cover-page))
                       (cdar (org-collect-keywords '("OPTIONS"))))))
               org-latex-cover-page)
      ((or 't 'yes) t)
      ('auto (when (> (count-words (point-min) (point-max)) org-latex-cover-page-wordcount-threshold) t))
      (_ nil)))

  (defadvice! org-latex-set-coverpage-subtitle-format-a (contents info)
    "Set the subtitle format when a cover page is being used."
    :before #'org-latex-template
    (when (org-latex-cover-page-p)
      (setf info (plist-put info :latex-subtitle-format org-latex-subtitle-coverpage-format))))

  (add-to-list 'org-latex-feature-implementations '(cover-page :snippet org-latex-cover-page-maketitle :order 9) t)
  (add-to-list 'org-latex-conditional-features '((org-latex-cover-page-p) . cover-page) t))
;; Cover page:1 ends here

;; [[file:config.org::*Condensed lists][Condensed lists:1]]
(after! org
  (defvar org-latex-condense-lists t
    "Reduce the space between list items.")
  (defvar org-latex-condensed-lists "
\\newcommand{\\setuplistspacing}{\\setlength{\\itemsep}{-0.5ex}\\setlength{\\parskip}{1.5ex}\\setlength{\\parsep}{0pt}}
\\let\\olditem\\itemize\\renewcommand{\\itemize}{\\olditem\\setuplistspacing}
\\let\\oldenum\\enumerate\\renewcommand{\\enumerate}{\\oldenum\\setuplistspacing}
\\let\\olddesc\\description\\renewcommand{\\description}{\\olddesc\\setuplistspacing}
   ")

  (add-to-list 'org-latex-conditional-features '((and org-latex-condense-lists "^[ \t]*[-+]\\|^[ \t]*[1Aa][.)] ") . condensed-lists) t)
  (add-to-list 'org-latex-feature-implementations '(condensed-lists :snippet org-latex-condensed-lists :order 0.7) t))
;; Condensed lists:1 ends here

;; [[file:config.org::*Pretty code blocks][Pretty code blocks:1]]
(use-package! engrave-faces-latex
  :after ox-latex
  :config
  (setq org-latex-listings 'engraved))
;; Pretty code blocks:1 ends here

;; [[file:config.org::*Pretty code blocks][Pretty code blocks:2]]
(defvar-local org-export-has-code-p nil)

(defadvice! org-export-expect-no-code (&rest _)
  :before #'org-export-as
  (setq org-export-has-code-p nil))

(defadvice! org-export-register-code (&rest _)
  :after #'org-latex-src-block
  :after #'org-latex-inline-src-block-engraved
  (setq org-export-has-code-p t))

(defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
  "Like `org-latex-example-block', but supporting an engraved backend"
  :around #'org-latex-example-block
  (let ((output-block (funcall orig-fn example-block contents info)))
    (if (eq 'engraved (plist-get info :latex-listings))
        (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
      output-block)))
;; Pretty code blocks:2 ends here

;; [[file:config.org::*Ox-chameleon][Ox-chameleon:1]]
(use-package! ox-chameleon
  :after ox)
;; Ox-chameleon:1 ends here

;; [[file:config.org::*Support images from URLs][Support images from URLs:1]]
(after! org
  (defadvice! +org-latex-link (orig-fn link desc info)
    "Acts as `org-latex-link', but supports remote images."
    :around #'org-latex-link
    (setq o-link link
          o-desc desc
          o-info info)
    (if (and (member (plist-get (cadr link) :type) '("http" "https"))
             (member (file-name-extension (plist-get (cadr link) :path))
                     '("png" "jpg" "jpeg" "pdf" "svg")))
        (org-latex-link--remote link desc info)
      (funcall orig-fn link desc info)))

  (defun org-latex-link--remote (link _desc info)
    (let* ((url (plist-get (cadr link) :raw-link))
           (ext (file-name-extension url))
           (target (format "%s%s.%s"
                           (temporary-file-directory)
                           (replace-regexp-in-string "[./]" "-"
                                                     (file-name-sans-extension (substring (plist-get (cadr link) :path) 2)))
                           ext)))
      (unless (file-exists-p target)
        (url-copy-file url target))
      (setcdr link (--> (cadr link)
                        (plist-put it :type "file")
                        (plist-put it :path target)
                        (plist-put it :raw-link (concat "file:" target))
                        (list it)))
      (concat "% fetched from " url "\n"
              (org-latex--inline-image link info)))))
;; Support images from URLs:1 ends here

;; [[file:config.org::*Beamer Exports][Beamer Exports:1]]
(setq org-beamer-theme "[progressbar=foot]metropolis"
      org-beamer-frame-level 2)

(defun org-beamer-p (info)
  (eq 'beamer (and (plist-get info :back-end)
                   (org-export-backend-name (plist-get info :back-end)))))

(defvar org-beamer-metropolis-tweaks "
\\NewCommandCopy{\\moldusetheme}{\\usetheme}
\\renewcommand*{\\usetheme}[2][]{\\moldusetheme[#1]{#2}
  \\setbeamertemplate{items}{$\\bullet$}
  \\setbeamerfont{block title}{size=\\normalsize, series=\\bfseries\\parbox{0pt}{\\rule{0pt}{4ex}}}}

\\makeatletter
\\newcommand{\\setmetropolislinewidth}{
  \\setlength{\\metropolis@progressinheadfoot@linewidth}{1.2px}}
\\makeatother

\\usepackage{etoolbox}
\\AtEndPreamble{\\setmetropolislinewidth}
")

(add-to-list 'org-latex-conditional-features '(org-beamer-p . beamer) t)
(add-to-list 'org-latex-feature-implementations '(beamer :requires .missing-koma :prevents (italic-quotes condensed-lists)) t)
(add-to-list 'org-latex-feature-implementations '(.missing-koma :snippet "\\usepackage{scrextend}" :order 2) t)
(add-to-list 'org-latex-conditional-features '((lambda (info) (and (org-beamer-p info) (string-match-p "metropolis$" org-beamer-theme))) . beamer-metropolis) t)
(add-to-list 'org-latex-feature-implementations '(beamer-metropolis :requires beamer :snippet org-beamer-metropolis-tweaks :order 3) t)
;; Beamer Exports:1 ends here
