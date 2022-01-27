;; carbon-theme.el --- A custom theme  -*- lexical-binding: t; -*-
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
;; This theme started as a fork of nano-emacs.
;; See https://github.com/rougier/nano-emacs.
;; Color palatte has been expanded and face definitions revised
;; -------------------------------------------------------------------
;;; Code

;;;; Requirements
(require 'carbon-themes)

(defvar evil-emacs-state-cursor)
(defvar evil-normal-state-cursor)
(defvar evil-visual-state-cursor)
(defvar evil-insert-state-cursor)
(defvar evil-replace-state-cursor)
(defvar evil-motion-state-cursor)
(defvar evil-operator-state-cursor)
(defvar hl-todo-keyword-faces)

;;;; Define group & colors

(defgroup carbon-themes nil
  "Faces and colors for carbon themes"
  :group 'faces)

;; Derive our default color set from core Emacs faces.
;; This allows use of carbon colors in independently themed Emacsen
;;
;; We memorize the default colorset in this var in order not to confuse
;; customize: the STANDARD argument of defcustom gets re-evaluated by customize
;; to determine if the current value is default or not.
(defvar carbon-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))
    (IBM . ,(face-foreground 'default nil t))))

(defun carbon-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name carbon-base-colors--defaults)))

(defcustom carbon-foreground (carbon-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-background (carbon-base-colors--get 'background)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-highlight (carbon-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-critical (carbon-base-colors--get 'critical)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-salient (carbon-base-colors--get 'salient)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-strong (carbon-base-colors--get 'strong)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-popout (carbon-base-colors--get 'popout)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-subtle (carbon-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-faded (carbon-base-colors--get 'faded)
  ""
  :type 'color
  :group 'carbon-themes)

(defcustom carbon-IBM (carbon-base-colors--get 'IBM)
  ""
  :type 'color
  :group 'carbon-themes)

;;;; Define Faces
;; The themes are fully defined by these faces

;;;;; Core faces
(defface carbon-default nil
  "Default face is for regular use."
  :group 'faces)

(defface carbon-critical nil
  "Critical face is for information that requires action---e.g.,
syntax or spelling errors. It should be of high constrast when
compared to other faces. This can be realized (for example) by
setting an intense background color, typically a shade of red or
orange. It should be used rarely."
  :group 'faces)

(defface carbon-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect (see
https://metapraxis.com/blog/blog/the-pop-out-effect/)."
  :group 'faces)

(defface carbon-strong nil
  "Strong face is used for information of a structural nature.
It is the same color as the default color. Only the
weight differs by one level (e.g., light/regular or
regular/bold). Usage might include titles, keywords,
directory, etc."
  :group 'faces)

(set-face-attribute 'carbon-strong nil
                    :foreground (face-foreground 'carbon-default)
                    :weight 'bold)

(defface carbon-salient nil
  "Salient face is used for important information, though not
necessarily that which needs immediate action or attention. To
suggest the information is important, the face uses a different
hue with approximately the same intensity as the default face.
This might be used, e.g., for links."
  :group 'faces)

(defface carbon-faded nil
  "Faded face is for less (immediately) important information. It
is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information."
  :group 'faces)

(defface carbon-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It's main use is for differentiating regions without drawing a
significant amount of attention. It is also closely related in
shade to the modeline color and to the highlight color."
  :group 'faces)

(defface carbon-IBM nil
  "IBM face is used for elements that are distinctly IBM.
It's main use is for accent regions without drawing a
significant amount of attention. "
  :group 'faces)


;;;;; Accent faces
;; The accent colors are used to fill out the color palatte. They are meant to be
;; used for attention or contrast with the core colors. Readability is important.

(defface carbon-highlight nil
  "This should be used primarily for highlighting. It is meant
subtlety stand out from the mode line and other adjacent faces."
  :group 'faces)

(defface carbon-modeline nil
  "Default face for the mode line."
  :group 'faces)

(defface carbon-inactive nil
  "Face for the inactive mode line"
  :group 'faces)

(defface carbon-green nil
  "A reddish accent face"
  :group 'faces)

(defface carbon-pink nil
  "A greenish accent face"
  :group 'faces)

(defface carbon-blue nil
  "A bluish accent face"
  :group 'faces)

(defface carbon-teal nil
  "A yellowish accent face"
  :group 'faces)

(defface carbon-purple nil
  "A brownish accent face"
  :group 'faces)

;;;; Define Theme
(deftheme carbon "A custom theme for yak shaving, with light and dark variants")

;;;; Set Colors

(defun carbon-theme--light-dark (light dark)
  "Determine theme using the LIGHT or the DARK color variants of carbon-theme."
  (if (eq carbon-set-theme 'light)
      light
    dark))
(defalias '--l/d #'carbon-theme--light-dark)

(defun carbon--set-theme-variant ()
  "Set theme colors according to LIGHT or DARK variant"
  (setq carbon-foreground (--l/d "#282b35" "#FFFFFF"))
  (setq carbon-background (--l/d "#fffef9" "#161616"))

  (setq carbon-modeline   (--l/d "#e3e7ef" "#262626"))
  (setq carbon-highlight  (--l/d "#dbe1eb" "#525252"))
  (setq carbon-inactive   (--l/d "#cbd3e1" "#393939"))

  (setq carbon-critical   (--l/d "#f53137" "#fa4d56"))
  (setq carbon-salient    (--l/d "#303db4" "#be95ff"))
  (setq carbon-strong     (--l/d "#000000" "#FAFAFA"))
  (setq carbon-popout     (--l/d "#940b96" "#3ddbd9"))
  (setq carbon-subtle     (--l/d "#eceff1" "#393939"))
  (setq carbon-faded      (--l/d "#727d97" "#6F6F6F"))
  (setq carbon-IBM        (--l/d "#abcdef" "#0043ce"))

  (setq carbon-blue       (--l/d "#30608c" "#33b1ff"))
  (setq carbon-pink       (--l/d "#00796b" "#ff7eb6"))
  (setq carbon-green      (--l/d "#960d36" "#42be65"))
  (setq carbon-purple     (--l/d "#966e53" "#be95ff"))
  (setq carbon-teal       (--l/d "#e0a500" "#3ddbd9")))


;;;; Customize Faces

;; Call color settings
(carbon--set-theme-variant)

;; Declare class and set faces
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   `carbon
   `(default ((,class :foreground ,carbon-foreground :background ,carbon-background)))


;;;;; Basic Faces
   `(buffer-menu-buffer                            ((,class :foreground ,carbon-strong)))
   `(minibuffer-prompt                             ((,class :foreground ,carbon-pink)))
   `(link                                          ((,class :foreground ,carbon-salient)))
   `(region                                        ((,class :background ,carbon-highlight)))
   `(fringe                                        ((,class :foreground ,carbon-faded :weight light)))
   `(highlight                                     ((,class :background ,carbon-subtle)))
   `(lazy-highlight                                ((,class :foreground ,carbon-pink)))
   `(trailing-whitespace                           ((,class :foreground ,carbon-faded)))
   `(show-paren-match                              ((,class :foreground ,carbon-teal :background ,carbon-pink)))
   `(show-paren-mismatch                           ((,class :foreground ,carbon-popout :background ,carbon-critical)))
   `(tooltip nil                                   ((,class :height 0.85)))

;;;;; Bespoke Faces
   ;; NOTE: We want the carbon colors to be available as faces. It seems like there
   ;; should be a better way to do this but...
   `(carbon-foreground ((,class :foreground ,carbon-foreground)))
   `(carbon-background ((,class :background ,carbon-background)))
   `(carbon-modeline   ((,class :background ,carbon-modeline)))
   `(carbon-highlight  ((,class :foreground ,carbon-highlight)))
   `(carbon-inactive   ((,class :foreground ,carbon-inactive)))
   `(carbon-critical   ((,class :foreground ,carbon-critical)))
   `(carbon-salient    ((,class :foreground ,carbon-salient)))
   `(carbon-strong     ((,class :foreground ,carbon-strong)))
   `(carbon-popout     ((,class :foreground ,carbon-popout)))
   `(carbon-subtle     ((,class :foreground ,carbon-subtle)))
   `(carbon-faded      ((,class :foreground ,carbon-faded)))
   `(carbon-blue       ((,class :foreground ,carbon-blue)))
   `(carbon-pink       ((,class :foreground ,carbon-pink)))
   `(carbon-green      ((,class :foreground ,carbon-green)))
   `(carbon-purple     ((,class :foreground ,carbon-purple)))
   `(carbon-teal       ((,class :foreground ,carbon-teal)))

;;;;; Buttons
   `(custom-button                                 ((,class :foreground ,carbon-foreground :background ,carbon-highlight :box nil)))
   `(custom-button-mouse                           ((,class :foreground ,carbon-foreground :background ,carbon-subtle :box nil)))
   `(custom-button-pressed                         ((,class :foreground ,carbon-background :background ,carbon-foreground :box nil)))

;;;;; Bookmarks
   `(bookmark-menu-heading                         ((,class :foreground ,carbon-strong)))
   `(bookmark-menu-bookmark                        ((,class :foreground ,carbon-salient)))
   `(bookmark-face                                 ((,class :foreground ,carbon-salient)))

;;;;; Childframes
;;;;;; Mini-Frame
   `(mini-popup-background ((,class :background ,carbon-subtle)))
   `(mini-popup-border     ((,class :background ,carbon-subtle)))

;;;;;; Mini-Popup (Childframe)
   `(mini-popup-background ((,class :background ,carbon-subtle)))
   `(mini-popup-border     ((,class :background ,carbon-subtle)))

;;;;;; Posframe

   `(which-key-posframe                           ((,class :background ,carbon-subtle)))
   `(which-key-posframe-border                    ((,class :background ,carbon-subtle)))
   `(transient-posframe-border                    ((,class :background ,carbon-subtle)))
   `(transient-posframe                           ((,class :foreground ,carbon-strong :background ,carbon-subtle)))

;;;;; Completion/Narrowing

;;;;;; Company
   `(company-scrollbar-fg                          ((,class :foreground ,carbon-faded)))
   `(company-scrollbar-bg                          ((,class :foreground ,carbon-faded)))
   `(company-preview                               ((,class :foreground ,carbon-faded :weight bold)))
   `(company-preview-common                        ((,class :foreground ,carbon-faded)))
   `(company-tooltip-selection                     ((,class :foreground ,carbon-salient)))
   `(company-tooltip                               ((,class :background ,carbon-subtle)))
   `(company-tooltip-common                        ((,class :background ,carbon-subtle)))
   `(company-tooltip-common-selection              ((,class :foreground ,carbon-salient)))
   `(company-tooltip-annotation                    ((,class :foreground ,carbon-faded)))
   `(company-tooltip-annotation-selection          ((,class :foreground ,carbon-salient)))


;;;;;; Counsel
   `(counsel-active-mode                           ((,class :foreground ,carbon-salient)))
   `(counsel-application-name                      ((,class :foreground ,carbon-green)))
   `(counsel-key-binding                           ((,class :inherit default)))
   `(counsel-outline-1                             ((,class :inherit org-level-1)))
   `(counsel-outline-2                             ((,class :inherit org-level-2)))
   `(counsel-outline-3                             ((,class :inherit org-level-3)))
   `(counsel-outline-4                             ((,class :inherit org-level-4)))
   `(counsel-outline-5                             ((,class :inherit org-level-5)))
   `(counsel-outline-6                             ((,class :inherit org-level-6)))
   `(counsel-outline-7                             ((,class :inherit org-level-7)))
   `(counsel-outline-8                             ((,class :inherit org-level-8)))
   `(counsel-outline-default                       ((,class :foreground ,carbon-foreground)))
   `(counsel-variable-documentation                ((,class :inherit default :foreground ,carbon-teal)))

;;;;;; Helm
   `(helm-selection                                ((,class :foreground ,carbon-subtle :weight bold)))
   `(helm-match                                    ((,class :foreground ,carbon-strong)))
   `(helm-source-header                            ((,class :foreground ,carbon-salient)))
   `(helm-visible-mark                             ((,class :foreground ,carbon-strong)))
   `(helm-swoop-target-line-face                   ((,class :foreground ,carbon-subtle :weight bold)))
   `(helm-moccur-buffer                            ((,class :foreground ,carbon-strong)))
   `(helm-ff-file                                  ((,class :foreground ,carbon-faded)))
   `(helm-ff-prefix                                ((,class :foreground ,carbon-strong)))
   `(helm-ff-dotted-directory                      ((,class :foreground ,carbon-faded)))
   `(helm-ff-directory                             ((,class :foreground ,carbon-strong)))
   `(helm-ff-executable                            ((,class :foreground ,carbon-popout)))
   `(helm-grep-match                               ((,class :foreground ,carbon-strong)))
   `(helm-grep-file                                ((,class :foreground ,carbon-faded)))
   `(helm-grep-lineno                              ((,class :foreground ,carbon-faded)))
   `(helm-grep-finish                              ((,class :foreground ,carbon-foreground)))


;;;;;; Ivy
   `(ivy-action                                    ((,class :foreground ,carbon-faded)))
   `(ivy-completions-annotations                   ((,class :foreground ,carbon-faded)))
   `(ivy-confirm-face                              ((,class :foreground ,carbon-faded)))
   `(ivy-current-match                             ((,class :foreground ,carbon-strong :weight bold :background ,carbon-highlight)))
   `(ivy-cursor                                    ((,class :inherit default)))
   `(ivy-grep-info                                 ((,class :foreground ,carbon-strong)))
   `(ivy-grep-line-number                          ((,class :foreground ,carbon-faded)))
   `(ivy-highlight-face                            ((,class :foreground ,carbon-strong)))
   `(ivy-match-required-face                       ((,class :foreground ,carbon-faded)))
   `(ivy-minibuffer-match-face-1                   ((,class :foreground ,carbon-popout)))
   `(ivy-minibuffer-match-face-2                   ((,class :foreground ,carbon-popout)))
   `(ivy-minibuffer-match-face-3                   ((,class :foreground ,carbon-popout)))
   `(ivy-minibuffer-match-face-4                   ((,class :foreground ,carbon-popout)))
   `(ivy-minibuffer-match-highlight                ((,class :foreground ,carbon-strong)))
   `(ivy-modified-buffer                           ((,class :foreground ,carbon-popout)))
   `(ivy-modified-outside-buffer                   ((,class :foreground ,carbon-strong)))
   `(ivy-org                                       ((,class :foreground ,carbon-faded)))
   `(ivy-prompt-match                              ((,class :foreground ,carbon-faded)))
   `(ivy-remote                                    ((,class :inherit default)))
   `(ivy-separator                                 ((,class :foreground ,carbon-faded)))
   `(ivy-subdir                                    ((,class :foreground ,carbon-faded)))
   `(ivy-virtual                                   ((,class :foreground ,carbon-faded)))
   `(ivy-yanked-word                               ((,class :foreground ,carbon-faded)))

;;;;;; Ido
   `(ido-first-match                               ((,class :foreground ,carbon-salient)))
   `(ido-only-match                                ((,class :foreground ,carbon-faded)))
   `(ido-subdir                                    ((,class :foreground ,carbon-strong)))

;;;;;; Selectrum
   `(selectrum-current-candidate                   ((,class :weight bold :background ,carbon-highlight)))
   `(selectrum-prescient-secondary-highlight       ((,class :weight bold :foreground ,carbon-blue)))
   `(selectrum-prescient-primary-highlight         ((,class :weight bold :foreground ,carbon-salient)))
   `(selectrum-completion-docsig                   ((,class :slant  italic :inherit selectrum-completion-annotation)))
   `(selectrum-completion-annotation               ((,class :inherit completions-annotations)))
   `(selectrum-group-separator                     ((,class :strike-through t :inherit shadow)))
   `(selectrum-group-title                         ((,class :slant  italic :inherit shadow)))
   `(selectrum-quick-keys-match                    ((,class :inherit isearch)))
   `(selectrum-quick-keys-highlight                ((,class :foreground ,carbon-popout)))

;;;;;; Vertico
   `(vertico-current                               ((,class :weight regular :background ,carbon-highlight)))

;;;;;; Orderless

   `(orderless-match-face-0                        ((,class :weight bold :foreground ,carbon-teal)))
   `(orderless-match-face-1                        ((,class :weight bold :foreground ,carbon-teal)))
   `(orderless-match-face-2                        ((,class :weight bold :foreground ,carbon-teal)))
   `(orderless-match-face-3                        ((,class :weight bold :foreground ,carbon-teal)))



;;;;; Customize
   `(widget-field                                  ((,class :background ,carbon-subtle)))
   `(widget-button                                 ((,class :foreground ,carbon-foreground :bold t)))
   `(widget-single-line-field                      ((,class :background ,carbon-subtle)))
   `(custom-group-subtitle                         ((,class :foreground ,carbon-foreground :bold t)))
   `(custom-group-tag                              ((,class :foreground ,carbon-foreground :bold t)))
   `(custom-group-tag-1                            ((,class :foreground ,carbon-foreground :bold t)))
   `(custom-comment                                ((,class :foreground ,carbon-faded)))
   `(custom-comment-tag                            ((,class :foreground ,carbon-faded)))
   `(custom-changed                                ((,class :foreground ,carbon-salient)))
   `(custom-modified                               ((,class :foreground ,carbon-salient)))
   `(custom-face-tag                               ((,class :foreground ,carbon-foreground :bold t)))
   `(custom-variable-tag                           ((,class :foreground ,carbon-foreground :bold t)))
   `(custom-invalid                                ((,class :foreground ,carbon-popout)))
   `(custom-visibility                             ((,class :foreground ,carbon-salient)))
   `(custom-state                                  ((,class :foreground ,carbon-salient)))
   `(custom-link                                   ((,class :foreground ,carbon-salient)))
   `(custom-button                                 ((,class :foreground ,carbon-faded :background ,carbon-background :box `(:line-width 1 :color ,(face-foreground 'carbon-faded) :style nil))))
   `(custom-button-mouse                           ((,class :foreground ,carbon-faded :background ,carbon-subtle :box `(:line-width 1 :color ,(face-foreground 'carbon-faded) :style nil))))
   `(custom-button-pressed                         ((,class :foreground ,carbon-foreground :background ,carbon-salient :inverse-video nil :box `(:line-width 1 :color ,(face-foreground 'carbon-salient) :style nil))))

;;;;; Deft
   `(deft-filter-string-error-face                 ((,class :foreground ,carbon-popout)))
   `(deft-filter-string-face                       ((,class :foreground ,carbon-teal)))
   `(deft-header-face                              ((,class :foreground ,carbon-salient)))
   `(deft-separator-face                           ((,class :foreground ,carbon-faded)))
   `(deft-summary-face                             ((,class :foreground ,carbon-faded)))
   `(deft-time-face                                ((,class :foreground ,carbon-salient)))
   `(deft-title-face                               ((,class :foreground ,carbon-strong :weight semi-bold)))

;;;;; Diff
   `(diff-header                                   ((,class :foreground ,carbon-faded)))
   `(diff-file-header                              ((,class :foreground ,carbon-strong)))
   `(diff-context                                  ((,class :inherit    default)))
   `(diff-removed                                  ((,class :foreground ,carbon-faded)))
   `(diff-changed                                  ((,class :foreground ,carbon-popout)))
   `(diff-added                                    ((,class :foreground ,carbon-salient)))
   `(diff-refine-added                             ((,class :foreground ,carbon-strong)))
   `(diff-refine-changed                           ((,class :foreground ,carbon-popout)))
   `(diff-refine-removed                           ((,class :foreground ,carbon-faded :strike-through t)))
   `(magit-section-highlight                       ((,class :background ,carbon-subtle)))


;;;;; Dired
;;;;;; All The Icons Dired
   `(all-the-icons-dired-dir-face                  ((,class :forground ,carbon-salient)))

;;;;;; Dired (plus)
   `(diredp-write-priv                             ((,class :foreground ,carbon-critical)))
   `(diredp-tagged-autofile-name                   ((,class :foreground ,carbon-background)))
   `(diredp-symlink                                ((,class :foreground ,carbon-popout)))
   `(diredp-read-priv                              ((,class :foreground ,carbon-popout)))
   `(diredp-rare-priv                              ((,class :foreground ,carbon-popout :background ,carbon-critical)))
   `(diredp-other-priv                             ((,class :background ,carbon-green)))
   `(diredp-omit-file-name                         ((,class :strike-through ,carbon-faded :inherit diredp-ignored-file-name)))
   `(diredp-number                                 ((,class :foreground ,carbon-salient)))
   `(diredp-no-priv                                ((,class :foreground ,carbon-critical)))
   `(diredp-mode-line-flagged                      ((,class :foreground ,carbon-critical)))
   `(diredp-mode-line-marked                       ((,class :foreground ,carbon-salient)))
   `(diredp-link-priv                              ((,class :foreground ,carbon-popout)))
   `(diredp-ignored-file-name                      ((,class :foreground ,carbon-faded)))
   `(diredp-flag-mark-line                         ((,class :foreground ,carbon-popout)))
   `(diredp-flag-mark                              ((,class :foreground ,carbon-popout :background ,carbon-salient)))
   `(diredp-file-suffix                            ((,class :foreground ,carbon-faded)))
   `(diredp-file-name                              ((,class :foreground ,carbon-foreground)))
   `(diredp-executable-tag                         ((,class :foreground ,carbon-critical)))
   `(diredp-exec-priv                              ((,class :foreground ,carbon-critical)))
   `(diredp-dir-priv                               ((,class :foreground ,carbon-faded)))
   `(diredp-dir-name                               ((,class :foreground ,carbon-pink)))
   `(diredp-dir-heading                            ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :foreground ,carbon-blue :background ,carbon-subtle)))
   `(diredp-deletion-file-name                     ((,class :foreground ,carbon-critical)))
   `(diredp-deletion                               ((,class :foreground ,carbon-popout :background ,carbon-critical)))
   `(diredp-date-time                              ((,class :foreground ,carbon-faded)))
   `(diredp-compressed-file-suffix                 ((,class :foreground ,carbon-faded)))
   `(diredp-compressed-file-name                   ((,class :foreground ,carbon-background)))
   `(diredp-autofile-name                          ((,class :background ,carbon-subtle)))

;;;;;; Dired Colors (Diredfl)
   `(diredfl-write-priv                            ((,class :foreground ,carbon-critical)))
   `(diredfl-tagged-autofile-name                  ((,class :foreground ,carbon-background)))
   `(diredfl-symlink                               ((,class :foreground ,carbon-popout)))
   `(diredfl-read-priv                             ((,class :foreground ,carbon-popout)))
   `(diredfl-rare-priv                             ((,class :foreground ,carbon-popout :background ,carbon-critical)))
   `(diredfl-other-priv                            ((,class :background ,carbon-green)))
   `(diredfl-omit-file-name                        ((,class :strike-through ,carbon-faded :inherit diredp-ignored-file-name)))
   `(diredfl-number                                ((,class :foreground ,carbon-salient)))
   `(diredfl-no-priv                               ((,class :foreground ,carbon-critical)))
   `(diredfl-mode-line-flagged                     ((,class :foreground ,carbon-critical)))
   `(diredfl-mode-line-marked                      ((,class :foreground ,carbon-salient)))
   `(diredfl-link-priv                             ((,class :foreground ,carbon-popout)))
   `(diredfl-ignored-file-name                     ((,class :foreground ,carbon-faded)))
   `(diredfl-flag-mark-line                        ((,class :foreground ,carbon-popout)))
   `(diredfl-flag-mark                             ((,class :foreground ,carbon-popout :background ,carbon-salient)))
   `(diredfl-file-suffix                           ((,class :foreground ,carbon-faded)))
   `(diredfl-file-name                             ((,class :foreground ,carbon-foreground)))
   `(diredfl-executable-tag                        ((,class :foreground ,carbon-critical)))
   `(diredfl-exec-priv                             ((,class :foreground ,carbon-critical)))
   `(diredfl-dir-priv                              ((,class :foreground ,carbon-faded)))
   `(diredfl-dir-name                              ((,class :foreground ,carbon-pink)))
   `(diredfl-dir-heading                           ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :foreground ,carbon-blue :background ,carbon-subtle)))
   `(diredfl-deletion-file-name                    ((,class :foreground ,carbon-critical)))
   `(diredfl-deletion                              ((,class :foreground ,carbon-popout :background ,carbon-critical)))
   `(diredfl-date-time                             ((,class :foreground ,carbon-faded)))
   `(diredfl-compressed-file-suffix                ((,class :foreground ,carbon-faded)))
   `(diredfl-compressed-file-name                  ((,class :foreground ,carbon-background)))
   `(diredfl-autofile-name                         ((,class :background ,carbon-subtle)))

;;;;; Flyspell
   `(flyspell-duplicate                            ((,class :foreground ,carbon-green)))
   `(flyspell-incorrect                            ((,class :foreground ,carbon-critical)))

;;;;; Font Lock
   `(font-lock-comment-face                        ((,class :foreground ,carbon-faded :slant ,(if carbon-set-italic-comments 'italic 'normal))))
   `(font-lock-comment-delimiter-face              ((,class :foreground ,carbon-faded :weight bold :slant ,(if carbon-set-italic-comments 'italic 'normal))))
   `(font-lock-doc-face                            ((,class :foreground ,carbon-faded)))
   `(font-lock-string-face                         ((,class :foreground ,carbon-popout)))
   `(font-lock-constant-face                       ((,class :foreground ,carbon-teal)))
   `(font-lock-builtin-face                        ((,class :foreground ,carbon-pink)))
   `(font-lock-function-name-face                  ((,class :foreground ,carbon-strong :weight semi-bold)))
   `(font-lock-variable-name-face                  ((,class :foreground ,carbon-strong :weight semi-bold)))
   `(font-lock-type-face                           ((,class :foreground ,carbon-salient)))
   `(font-lock-keyword-face                        ((,class :foreground ,carbon-salient :slant ,(if carbon-set-italic-keywords 'italic 'normal))))
   `(font-lock-reference-face                      ((,class :foreground ,carbon-salient)))
   `(font-lock-warning-face                        ((,class :foreground ,carbon-critical)))
   `(font-lock-regexp-grouping-backslash           ((,class :foreground ,carbon-critical)))
   `(font-lock-regexp-grouping-construct           ((,class :foreground ,carbon-critical)))

;;;;; Git
;;;;;; Git-gutter
   `(git-gutter:added        ((,class :foreground ,carbon-pink)))
   `(git-gutter:deleted      ((,class :foreground ,carbon-green)))
   `(git-gutter:modified     ((,class :foreground ,carbon-popout)))
   `(git-gutter:separator    ((,class :foreground ,carbon-subtle)))
   `(git-gutter:unchanged    ((,class :foreground ,carbon-background)))
;;;;;; Git-gutter-fr
   `(git-gutter-fr:added        ((,class :foreground ,carbon-pink)))
   `(git-gutter-fr:deleted      ((,class :foreground ,carbon-green)))
   `(git-gutter-fr:modified     ((,class :foreground ,carbon-popout)))

;;;;; Help(ful)

   `(helpful-heading                               ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :foreground ,carbon-blue :height 1.25)))


;;;;; Highlight-Indentation
   `(highlight-indentation-face ((,class :inherit ,carbon-highlight)))
   `(highlight-indentation-current-column-face ((,class :background ,carbon-teal)))

;;;;; Highlight Indentation Guides
   `(highlight-indent-guides-stack-odd-face        ((,class :foreground ,carbon-purple)))
   `(highlight-indent-guides-stack-even-face       ((,class :foreground ,carbon-teal)))
   `(highlight-indent-guides-top-odd-face          ((,class :foreground ,carbon-purple)))
   `(highlight-indent-guides-top-even-face         ((,class :foreground ,carbon-teal)))
   `(highlight-indent-guides-odd-face              ((,class :foreground ,carbon-purple)))
   `(highlight-indent-guides-even-face             ((,class :foreground ,carbon-teal)))
   `(highlight-indent-guides-character-face        ((,class :foreground ,carbon-highlight)))
   `(highlight-indent-guides-top-character-face    ((,class :foreground ,carbon-highlight)))
   `(highlight-indent-guides-stack-character-face  ((,class :foreground ,carbon-highlight)))

;;;;; Imenu List
   `(imenu-list-entry-face-0                       ((,class :inherit imenu-list-entry-face :foreground ,carbon-faded)))
   `(imenu-list-entry-face-1                       ((,class :inherit imenu-list-entry-face :foreground ,carbon-faded)))
   `(imenu-list-entry-face-2                       ((,class :inherit imenu-list-entry-face :foreground ,carbon-faded)))
   `(imenu-list-entry-face-3                       ((,class :inherit imenu-list-entry-face :foreground ,carbon-faded)))

;;;;; Info (Documentation)
   `(info-menu-header                              ((,class :foreground ,carbon-strong)))
   `(info-header-node                              ((,class :foreground ,carbon-pink)))
   `(info-index-match                              ((,class :foreground ,carbon-salient)))
   `(Info-quoted                                   ((,class :foreground ,carbon-faded)))
   `(info-title-1                                  ((,class :foreground ,carbon-strong)))
   `(info-title-2                                  ((,class :foreground ,carbon-strong)))
   `(info-title-3                                  ((,class :foreground ,carbon-strong)))
   `(info-title-4                                  ((,class :foreground ,carbon-strong)))

;;;;; Interface
   `(widget-field                                  ((,class :background ,carbon-subtle)))
   `(widget-button                                 ((,class :foreground ,carbon-strong)))
   `(widget-single-line-field                      ((,class :foreground ,carbon-subtle)))
   `(custom-group-subtitle                         ((,class :foreground ,carbon-strong)))
   `(custom-group-tag                              ((,class :foreground ,carbon-strong)))
   `(custom-group-tag-1                            ((,class :foreground ,carbon-strong)))
   `(custom-comment                                ((,class :foreground ,carbon-faded)))
   `(custom-comment-tag                            ((,class :foreground ,carbon-faded)))
   `(custom-changed                                ((,class :foreground ,carbon-salient)))
   `(custom-modified                               ((,class :foreground ,carbon-salient)))
   `(custom-face-tag                               ((,class :foreground ,carbon-strong)))
   `(custom-variable-tag                           ((,class :inherit    default)))
   `(custom-invalid                                ((,class :foreground ,carbon-popout)))
   `(custom-visibility                             ((,class :foreground ,carbon-salient)))
   `(custom-state                                  ((,class :foreground ,carbon-salient)))
   `(custom-link                                   ((,class :foreground ,carbon-salient)))

;;;;; Markdown Mode
   `(markdown-blockquote-face                      ((,class :foreground ,carbon-salient)))
   `(markdown-bold-face                            ((,class :foreground ,carbon-strong :weight bold)))
   `(markdown-code-face                            ((,class :inherit    default)))
   `(markdown-comment-face                         ((,class :foreground ,carbon-faded)))
   `(markdown-footnote-marker-face                 ((,class :inherit    default)))
   `(markdown-footnote-text-face                   ((,class :inherit    default)))
   `(markdown-gfm-checkbox-face                    ((,class :inherit    default)))
   `(markdown-header-delimiter-face                ((,class :foreground ,carbon-faded)))
   `(markdown-header-face                          ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default))))
   `(markdown-header-face-1                        ((,class :inherit outline-1)))
   `(markdown-header-face-2                        ((,class :inherit outline-2)))
   `(markdown-header-face-3                        ((,class :inherit outline-1)))
   `(markdown-header-face-4                        ((,class :inherit outline-2)))
   `(markdown-header-face-5                        ((,class :inherit outline-1)))
   `(markdown-header-face-6                        ((,class :inherit outline-2)))
   `(markdown-header-rule-face                     ((,class :inherit default)))
   `(markdown-highlight-face                       ((,class :inherit default)))
   `(markdown-hr-face                              ((,class :inherit default)))
   `(markdown-html-attr-name-face                  ((,class :inherit default)))
   `(markdown-html-attr-value-face                 ((,class :inherit default)))
   `(markdown-html-entity-face                     ((,class :inherit default)))
   `(markdown-html-tag-delimiter-face              ((,class :inherit default)))
   `(markdown-html-tag-name-face                   ((,class :inherit default)))
   `(markdown-inline-code-face                     ((,class :foreground ,carbon-popout)))
   `(markdown-italic-face                          ((,class :foreground ,carbon-strong :slant italic)))
   `(markdown-language-info-face                   ((,class :inherit   default)))
   `(markdown-language-keyword-face                ((,class :inherit   default)))
   `(markdown-line-break-face                      ((,class :inherit   default)))
   `(markdown-link-face                            ((,class :foreground ,carbon-salient)))
   `(markdown-link-title-face                      ((,class :inherit    default)))
   `(markdown-list-face                            ((,class :foreground ,carbon-faded)))
   `(markdown-markup-face                          ((,class :foreground ,carbon-faded)))
   `(markdown-math-face                            ((,class :inherit    default)))
   `(markdown-metadata-key-face                    ((,class :foreground ,carbon-faded)))
   `(markdown-metadata-value-face                  ((,class :foreground ,carbon-faded)))
   `(markdown-missing-link-face                    ((,class :inherit    default)))
   `(markdown-plain-url-face                       ((,class :inherit    default)))
   `(markdown-pre-face                             ((,class :inherit    default)))
   `(markdown-reference-face                       ((,class :foreground ,carbon-salient)))
   `(markdown-strike-through-face                  ((,class :foreground ,carbon-faded)))
   `(markdown-table-face                           ((,class :inherit    default)))
   `(markdown-url-face                             ((,class :foreground ,carbon-salient)))

;;;;; Magit
   `(magit-branch-current      ((,class :foreground ,carbon-salient :box t :weight semi-bold)))
   `(magit-branch-local        ((,class :foreground ,carbon-salient :weight semi-bold)))
   `(magit-branch-remote       ((,class :foreground ,carbon-pink :weight semi-bold)))
   `(magit-branch-remote-head  ((,class :foreground ,carbon-popout :box t)))
   `(magit-branch-upstream     ((,class :inherit italic)))
   `(magit-cherry-equivalent   ((,class :background ,carbon-background :foreground ,carbon-popout)))
   `(magit-cherry-unmatched ((,class :background ,carbon-background :foreground ,carbon-salient)))
   `(magit-head                ((,class :inherit magit-branch-local)))
   `(magit-header-line ((,class :inherit bold :foreground ,carbon-foreground)))
   `(magit-header-line-key ((,class :foregrond ,carbon-pink)))
   `(magit-header-line-log-select ((,class :inherit bold :foreground ,carbon-foreground)))
   `(magit-keyword ((,class :foreground ,carbon-popout)))
   `(magit-keyword-squash ((,class :inherit bold :foreground ,carbon-teal)))
   `(magit-section ((,class :background ,carbon-subtle :foreground ,carbon-foreground)))
   `(magit-section-heading     ((,class :weight semi-bold :foreground ,carbon-teal)))
   `(magit-section-heading-selection ((,class :foreground ,carbon-salient)))
   `(magit-section-highlight ((,class :background ,carbon-highlight)))
   `(magit-tag                 ((,class :foreground ,carbon-teal)))
   `(magit-header-line         ((,class :foreground ,carbon-foreground
                                        :background ,carbon-modeline
                                        :box (:line-width (if (fboundp 'carbon-modeline) carbon-modeline-size 3))
                                        :color ,carbon-modeline
                                        :style nil)
                                :overline nil
                                :underline nil))
   `(magit-header-line-log-select ((,class :foreground ,carbon-foreground
                                           :background ,carbon-modeline
                                           :box (:line-width (if (fboundp 'carbon-modeline) carbon-modeline-size 3))
                                           :color ,carbon-modeline
                                           :style nil)
                                   :overline nil
                                   :underline nil))




;;;;; Message
   `(message-cited-text                            ((,class :foreground ,carbon-faded)))
   `(message-header-cc                             ((,class :inherit default)))
   `(message-header-name                           ((,class :foreground ,carbon-strong)))
   `(message-header-newsgroups                     ((,class :inherit default)))
   `(message-header-other                          ((,class :inherit default)))
   `(message-header-subject                        ((,class :foreground ,carbon-salient)))
   `(message-header-to                             ((,class :foreground ,carbon-salient)))
   `(message-header-xheader                        ((,class :inherit default)))
   `(message-mml                                   ((,class :foreground ,carbon-popout)))
   `(message-separator                             ((,class :foreground ,carbon-faded)))

;;;;; Meow
   `(meow-normal-cursor         ((,class :background ,carbon-teal)))
   `(meow-insert-cursor         ((,class :background ,carbon-critical)))
   `(meow-keypad-cursor         ((,class :background ,carbon-purple)))
   `(meow-motion-cursor         ((,class :background ,carbon-pink)))
   `(meow-kmacro-cursor         ((,class :background ,carbon-salient)))
   `(meow-beacon-cursor         ((,class :background ,carbon-teal)))
   `(meow-beacon-fake-selection ((,class :background ,carbon-modeline)))
   `(meow-beacon-fake-cursor    ((,class :background ,carbon-teal)))

;;;;; Mode line/Header line
;;;;;; Conditional Loading
   ;; NOTE: these settings are specifically for carbon-modeline
   ;; See https://github.com/mclear-tools/carbon-modeline
   ;; Mode line settings based on position
   (when (fboundp 'carbon-modeline)
     (when (eq carbon-modeline-position 'top)
       `(header-line ((,class :foreground ,carbon-foreground
                              :background ,carbon-modeline
                              :box (:line-width ,carbon-modeline-size
                                    :color ,carbon-modeline
                                    :style nil)
                              :overline nil
                              :underline nil)))))

   (when (fboundp 'carbon-modeline)
     (when (eq carbon-modeline-position 'top)
       `(mode-line  ((,class :height 0.1
                             :underline ,carbon-subtle
                             :overline nil
                             :box nil)))))


   (when (fboundp 'carbon-modeline)
     (when (eq carbon-modeline-position 'top)
       `(mode-line-inactive  ((,class :height 0.1
                                      :underline ,carbon-subtle
                                      :overline nil
                                      :box nil)))))


   (when (fboundp 'carbon-modeline)
     (when (eq carbon-modeline-position 'bottom)
       `(mode-line ((,class :foreground ,carbon-foreground
                            :background ,carbon-modeline
                            :box (:line-width ,carbon-modeline-size
                                  :color ,carbon-modeline
                                  :style nil)
                            :overline nil
                            :underline nil)))))

   (when (fboundp 'carbon-modeline)
     (when (eq carbon-modeline-position 'bottom)
       `(mode-line-inactive ((,class :foreground ,carbon-subtle
                                     :background ,carbon-modeline
                                     :box (:line-width ,carbon-modeline-size
                                           :color ,carbon-modeline
                                           :style nil)
                                     :overline nil
                                     :underline nil)))))

   ;; No underline in terminal
   ;; FIXME: for some reason this seems necessary
   ;; to disable underline in terminal
   (when (not (display-graphic-p))
     (set-face-attribute 'mode-line nil
                         :underline nil)
     (set-face-attribute 'mode-line-inactive nil
                         :underline nil))


   (when (fboundp 'carbon-modeline)
     (when (eq carbon-modeline-position nil)
       `(mode-line ((,class :foreground ,carbon-foreground
                            :background ,carbon-modeline
                            :box (:line-width ,carbon-modeline-size
                                  :color ,carbon-modeline
                                  :style nil)
                            :overline nil
                            :underline nil)))))

   (when (fboundp 'carbon-modeline)
     (when (eq carbon-modeline-position nil)
       `(mode-line-inactive ((,class :foreground ,carbon-faded
                                     :background ,carbon-modeline
                                     :box (:line-width ,carbon-modeline-size
                                           :color ,carbon-modeline
                                           :style nil)
                                     :overline nil
                                     :underline nil)))))

;;;;;; Mode line indicators

   ;; Active
   (when (fboundp 'carbon-modeline)
     `(carbon-modeline-active               ((,class (:foreground ,carbon-foreground
                                                       :background ,carbon-modeline
                                                       :box (:line-width ,carbon-modeline-size
                                                             :color ,carbon-modeline
                                                             :style nil)
                                                       :overline nil
                                                       :underline nil)))))

   `(carbon-modeline-active-name          ((,class (:background ,carbon-modeline
                                                     :foreground ,carbon-foreground))))
   `(carbon-modeline-active-primary       ((,class (:foreground ,carbon-faded :weight light))))
   `(carbon-modeline-active-secondary     ((,class (:foreground ,carbon-foreground))))
   `(carbon-modeline-active-status-RW ((,class  :foreground ,carbon-foreground
                                                :background ,carbon-IBM
                                                :box (:line-width 1 :color ,carbon-IBM :style nil))))

   `(carbon-modeline-active-status-** ((,class :foreground ,carbon-background
                                                :background ,carbon-green
                                                :box (:line-width 1 :color ,carbon-green :style nil))))

   `(carbon-modeline-active-status-RO ((,class :foreground ,carbon-foreground
                                                :background ,carbon-IBM
                                                :box (:line-width 1 :color ,carbon-IBM :style nil))))

   ;; Inactive
   (when (fboundp 'carbon-modeline)
     `(carbon-modeline-inactive             ((,class (:foreground ,carbon-subtle
                                                       :background ,carbon-modeline
                                                       :box (:line-width ,carbon-modeline-size
                                                             :color ,carbon-modeline
                                                             :style nil)
                                                       :overline nil
                                                       :underline nil)))))
   `(carbon-modeline-inactive-name        ((,class (:foreground ,carbon-faded :background ,carbon-modeline :weight light))))
   `(carbon-modeline-inactive-primary     ((,class (:foreground ,carbon-faded :background ,carbon-modeline :weight light))))
   `(carbon-modeline-inactive-secondary   ((,class (:foreground ,carbon-faded :background ,carbon-modeline :weight light))))

   `(carbon-modeline-inactive-status-RO   ((,class :foreground ,carbon-subtle
                                                    :background ,carbon-inactive
                                                    :box (:line-width 1
                                                          :color ,carbon-inactive
                                                          :style nil)
                                                    :overline nil
                                                    :underline nil)))

   `(carbon-modeline-inactive-status-RW ((,class :foreground ,carbon-subtle
                                                  :background ,carbon-inactive
                                                  :box (:line-width 1
                                                        :color ,carbon-inactive
                                                        :style nil)
                                                  :overline nil
                                                  :underline nil)))

   `(carbon-modeline-inactive-status-**  ((,class :foreground ,carbon-subtle
                                                   :background ,carbon-inactive
                                                   :box (:line-width 1
                                                         :color ,carbon-inactive
                                                         :style nil)
                                                   :overline nil
                                                   :underline nil)))

   (when (not (fboundp 'carbon-modeline))
     `(mode-line ((,class :foreground ,carbon-foreground
                          :background ,carbon-modeline
                          :box (:line-width 3
                                :color ,carbon-modeline
                                :style nil)
                          :overline nil
                          :underline nil))))

   (when (not (fboundp 'carbon-modeline))
     `(mode-line-inactive ((,class :foreground ,carbon-faded
                                   :background ,carbon-modeline
                                   :box (:line-width 3
                                         :color ,carbon-modeline
                                         :style nil)
                                   :overline nil
                                   :underline nil))))

;;;;; Mu4e
   `(mu4e-attach-number-face                      ((,class :foreground ,carbon-strong)))
   `(mu4e-cited-1-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-cited-2-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-cited-3-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-cited-4-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-cited-5-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-cited-6-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-cited-7-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-compose-header-face                     ((,class :foreground ,carbon-faded)))
   `(mu4e-compose-separator-face                  ((,class :foreground ,carbon-faded)))
   `(mu4e-contact-face                            ((,class :foreground ,carbon-salient)))
   `(mu4e-context-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-draft-face                              ((,class :foreground ,carbon-faded)))
   `(mu4e-flagged-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-footer-face                             ((,class :foreground ,carbon-faded)))
   `(mu4e-forwarded-face                          ((,class :inherit    default)))
   `(mu4e-header-face                             ((,class :inherit    default)))
   `(mu4e-header-highlight-face                   ((,class :foreground ,carbon-subtle)))
   `(mu4e-header-key-face                         ((,class :foreground ,carbon-strong)))
   `(mu4e-header-marks-face                       ((,class :foreground ,carbon-faded)))
   `(mu4e-header-title-face                       ((,class :foreground ,carbon-strong)))
   `(mu4e-header-value-face                       ((,class :inherit    default)))
   `(mu4e-highlight-face                          ((,class :foreground ,carbon-popout)))
   `(mu4e-link-face                               ((,class :foreground ,carbon-salient)))
   `(mu4e-modeline-face                           ((,class :foreground ,carbon-faded)))
   `(mu4e-moved-face                              ((,class :foreground ,carbon-faded)))
   `(mu4e-ok-face                                 ((,class :foreground ,carbon-faded)))
   `(mu4e-region-code                             ((,class :foreground ,carbon-faded)))
   `(mu4e-replied-face                            ((,class :foreground ,carbon-salient)))
   `(mu4e-special-header-value-face               ((,class :inherit    default)))
   `(mu4e-system-face                             ((,class :foreground ,carbon-faded)))
   `(mu4e-title-face                              ((,class :foreground ,carbon-strong)))
   `(mu4e-trashed-face                            ((,class :foreground ,carbon-faded)))
   `(mu4e-unread-face                             ((,class :foreground ,carbon-strong)))
   `(mu4e-url-number-face                         ((,class :foreground ,carbon-faded)))
   `(mu4e-view-body-face                          ((,class :inherit    default)))
   `(mu4e-warning-face                            ((,class :foreground ,carbon-faded)))

;;;;; Org-agenda
   `(org-agenda-calendar-event                    ((,class :inherit default)))
   `(org-agenda-calendar-sexp                     ((,class :foreground ,carbon-faded)))
   `(org-agenda-clocking                          ((,class :foreground ,carbon-faded)))
   `(org-agenda-column-dateline                   ((,class :foreground ,carbon-faded)))
   `(org-agenda-current-time                      ((,class :foreground ,carbon-faded)))
   `(org-agenda-date                              ((,class :foreground ,carbon-salient)))
   `(org-agenda-date-today                        ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :height 1.25 :foreground ,carbon-blue)))
   `(org-super-agenda-header                      ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :foreground ,carbon-blue)))
   `(org-agenda-date-weekend                      ((,class :foreground ,carbon-faded)))
   `(org-agenda-diary                             ((,class :foreground ,carbon-faded)))
   `(org-agenda-dimmed-todo-face                  ((,class :foreground ,carbon-faded)))
   `(org-agenda-done                              ((,class :foreground ,carbon-faded :strike-through t)))
   `(org-agenda-filter-category                   ((,class :foreground ,carbon-faded)))
   `(org-agenda-filter-effort                     ((,class :foreground ,carbon-faded)))
   `(org-agenda-filter-regexp                     ((,class :foreground ,carbon-faded)))
   `(org-agenda-filter-tags                       ((,class :foreground ,carbon-faded)))
   `(org-agenda-restriction-lock                  ((,class :foreground ,carbon-faded)))
   `(org-agenda-structure                         ((,class :foreground ,carbon-faded)))

;;;;; Org mode
   `(org-archived                                 ((,class :foreground ,carbon-faded)))
   `(org-block                                    ((,class :foreground ,carbon-faded)))
   `(org-block-begin-line                         ((,class :foreground ,carbon-faded)))
   `(org-block-end-line                           ((,class :foreground ,carbon-faded)))
   `(org-checkbox                                 ((,class :foreground ,carbon-faded)))
   `(org-checkbox-statistics-done                 ((,class :foreground ,carbon-faded)))
   `(org-checkbox-statistics-todo                 ((,class :foreground ,carbon-faded)))
   `(org-cite                                     ((,class :foreground ,carbon-salient)))
   `(org-cite-key                                 ((,class :foreground ,carbon-pink)))
   `(org-clock-overlay                            ((,class :foreground ,carbon-faded)))
   `(org-code                                     ((,class :foreground ,carbon-faded)))
   `(org-column                                   ((,class :foreground ,carbon-faded)))
   `(org-column-title                             ((,class :foreground ,carbon-faded)))
   `(org-date                                     ((,class :foreground ,carbon-faded)))
   `(org-date-selected                            ((,class :foreground ,carbon-faded)))
   `(org-default                                  ((,class :foreground ,carbon-faded)))
   `(org-document-info                            ((,class :foreground ,carbon-faded :weight light)))
   `(org-document-info-keyword                    ((,class :foreground ,carbon-faded :weight light)))
   `(org-document-title                           ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :height 1.1 :foreground ,carbon-salient)))
   `(org-done                                     ((,class :foreground ,carbon-faded :strike-through t)))
   `(org-drawer                                   ((,class :foreground ,carbon-faded :weight light)))
   `(org-ellipsis                                 ((,class :foreground ,carbon-faded)))
   `(org-footnote                                 ((,class :foreground ,carbon-faded)))
   `(org-formula                                  ((,class :foreground ,carbon-faded)))
   `(org-habit-alert-face                         ((,class :inherit default)))
   `(org-headline-done                            ((,class :foreground ,carbon-faded)))
   `(org-latex-and-related                        ((,class :foreground ,carbon-faded)))
   `(org-level-1                                  ((,class :inherit 'outline-1)))
   `(org-level-2                                  ((,class :inherit 'outline-2)))
   `(org-level-3                                  ((,class :inherit 'outline-1)))
   `(org-level-4                                  ((,class :inherit 'outline-2)))
   `(org-level-5                                  ((,class :inherit 'outline-1)))
   `(org-level-6                                  ((,class :inherit 'outline-2)))
   `(org-level-7                                  ((,class :inherit 'outline-1)))
   `(org-level-8                                  ((,class :inherit 'outline-2)))
   `(org-link                                     ((,class :foreground ,carbon-salient)))
   `(org-list-dt                                  ((,class :foreground ,carbon-blue)))
   `(org-macro                                    ((,class :foreground ,carbon-faded)))
   `(org-meta-line                                ((,class :foreground ,carbon-faded :weight light)))
   `(org-mode-line-clock                          ((,class :foreground ,carbon-faded)))
   `(org-mode-line-clock-overrun                  ((,class :foreground ,carbon-faded)))
   `(org-priority                                 ((,class :foreground ,carbon-faded)))
   `(org-property-value                           ((,class :foreground ,carbon-faded :weight light)))
   `(org-quote                                    ((,class :foreground ,carbon-salient)))
   `(org-scheduled                                ((,class :foreground ,carbon-salient)))
   `(org-scheduled-previously                     ((,class :foreground ,carbon-salient)))
   `(org-scheduled-today                          ((,class :foreground ,carbon-salient)))
   `(org-sexp-date                                ((,class :foreground ,carbon-faded)))
   `(org-special-keyword                          ((,class :foreground ,carbon-faded :weight light)))
   `(org-table                                    ((,class :inherit    default)))
   `(org-tag                                      ((,class :foreground ,carbon-faded)))
   `(org-tag-group                                ((,class :foreground ,carbon-faded)))
   `(org-target                                   ((,class :foreground ,carbon-faded)))
   `(org-time-grid                                ((,class :foreground ,carbon-faded)))
   `(org-todo                                     ((,class :weight normal :foreground ,carbon-teal)))
   `(org-upcoming-deadline                        ((,class :foreground ,carbon-strong)))
   `(org-upcoming-distant-deadline                ((,class :foreground ,carbon-foreground)))
   `(org-verbatim                                 ((,class :foreground ,carbon-faded)))
   `(org-verse                                    ((,class :foreground ,carbon-faded)))
   `(org-warning                                  ((,class :foreground ,carbon-popout)))

;;;;; Outline
   `(outline-minor-0      ((,class :background ,carbon-highlight)))
   `(outline-1            ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,carbon-pink)))
   `(outline-2            ((,class :inherit ,(if carbon-set-variable-pitch 'variable-pitch 'default) :weight normal :foreground ,carbon-blue)))
   `(outline-3            ((,class :inherit outline-1)))
   `(outline-4            ((,class :inherit outline-2)))
   `(outline-5            ((,class :inherit outline-1)))
   `(outline-6            ((,class :inherit outline-2)))
   `(outline-7            ((,class :inherit outline-1)))
   `(outline-8            ((,class :inherit outline-2)))

;;;;; Search
   `(evil-ex-search                               ((,class :background ,carbon-popout)))
   `(isearch                                      ((,class :background ,carbon-popout :foreground ,carbon-highlight :weight bold)))
   `(isearch-fail                                 ((,class :background ,carbon-critical)))
   `(isearch-group-1                              ((,class :background ,carbon-blue)))
   `(isearch-group-2                              ((,class :background ,carbon-green)))
   `(query-replace                                ((,class :background ,carbon-teal)))

;;;;; Semantic
   `(italic                                       ((,class :slant italic)))
   `(bold                                         ((,class :foreground ,carbon-strong :weight bold)))
   `(bold-italic                                  ((,class :foreground ,carbon-strong :weight bold :slant italic)))
   `(underline                                    ((,class :underline t)))
   `(shadow                                       ((,class :foreground ,carbon-faded)))
   `(success                                      ((,class :foreground ,carbon-salient)))
   `(warning                                      ((,class :foreground ,carbon-popout)))
   `(error                                        ((,class :foreground ,carbon-critical)))
   `(match                                        ((,class :forgeround ,carbon-popout :weight bold)))

;;;;; Speed Bar
   `(speedbar-button-face                         ((,class :foreground ,carbon-faded)))
   `(speedbar-directory-face                      ((,class :foreground ,carbon-foreground :bold t)))
   `(speedbar-file-face                           ((,class :foreground ,carbon-foreground :background ,carbon-background)))
   `(speedbar-highlight-face                      ((,class :foreground ,carbon-highlight)))
   `(speedbar-selected-face                       ((,class :background ,carbon-subtle :bold t)))
   `(speedbar-separator-face                      ((,class :foreground ,carbon-faded)))
   `(speedbar-tag-face                            ((,class :foreground ,carbon-faded)))

;;;;; Term
   `(term-bold                                    ((,class :foreground ,carbon-strong :weight semi-bold)))
   `(term-color-black                             ((,class :foregroud  ,carbon-background)))
   `(term-color-white                             ((,class :foreground ,carbon-foreground)))
   `(term-color-blue                              ((,class :foreground ,carbon-blue)))
   `(term-color-cyan                              ((,class :foreground ,carbon-salient)))
   `(term-color-green                             ((,class :foreground ,carbon-pink)))
   `(term-color-magenta                           ((,class :foreground ,carbon-popout)))
   `(term-color-red                               ((,class :foreground ,carbon-critical)))
   `(term-color-yellow                            ((,class :foreground ,carbon-teal)))

;;;;; Window Divs
   ;; divide windows more attractively
   `(window-divider                               ((,class :foreground ,carbon-background)))
   `(window-divider-first-pixel                   ((,class :foreground ,carbon-background)))
   `(window-divider-last-pixel                    ((,class :foreground ,carbon-background)))
   ;; divide windows better in terminal
   ;; see https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
   (when (not (display-graphic-p))
     (set-face-background 'vertical-border carbon-background)
     (set-face-foreground 'vertical-border (face-background 'vertical-border)))

;;;;; End Custom faces
   ))

;;;; Define evil cursor colors
(defun carbon--evil-load-cursors ()
  "Load theme specific cursor colors"
  (setq evil-emacs-state-cursor    `(,carbon-salient box))
  (setq evil-normal-state-cursor   `(,carbon-teal box))
  (setq evil-visual-state-cursor   `(,carbon-faded box))
  (setq evil-insert-state-cursor   `(,carbon-critical (bar . 2)))
  (setq evil-replace-state-cursor  `(,carbon-critical hbar))
  (setq evil-motion-state-cursor   `(,carbon-pink box))
  (setq evil-operator-state-cursor `(,carbon-purple hollow)))

(when carbon-set-evil-cursors
  (add-hook 'carbon-after-load-theme-hook #'carbon--evil-load-cursors))

;;;; Set Hl-Todo
;; inherit faces
(setq hl-todo-keyword-faces
      '(("HOLD" .       query-replace)
        ("TODO" .       warning)
        ("NEXT" .       highlight)
        ("OKAY" .       success)
        ("DONT" .       error)
        ("FAIL" .       error)
        ("DONE" .       shadow)
        ("NOTE" .       warning)
        ("KLUDGE" .     warning)
        ("HACK" .       warning)
        ("TEMP" .       warning)
        ("FIXME" .      error)
        ("XXX+" .       error)
        ("BUG" .        error)
        ("REVIEW" .     shadow)
        ("DEPRECATED" . shadow)))

;;;; Set Minibuffer & Echo Area
(defun carbon-theme--minibuffer ()
  "Derive minibuffer / echo area faces from carbon faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'fringe)))))
(carbon-theme--minibuffer)

;;; Provide theme

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'carbon)
(provide 'carbon-faces-colors)


;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:
;;; carbon-faces-colors.el ends here
