;;; nano-face-override.el -*- lexical-binding: t; -*-

; this is my way of customizing the faces used by nano.
; by default, nano's faces are too limiting in my opinion;
; syntax highlighting is much less effective when so many
; fields receive the same face attributes.

(after! flycheck
  (set-face-attribute 'flycheck-error nil
                      :underline
                      '(:color "#9C2F18" :style wave  ))
  (set-face-attribute 'flycheck-warning nil
                      :underline
                      '(:color "#AB9D27" :style wave  )))
(after! nano-faces
(set-face-attribute 'nano-face-critical nil
                    :foreground dn-critical
                    :background dn-background                   )
(set-face-attribute 'nano-face-tag-critical nil
                    :foreground dn-critical
                    :background dn-background                   ))
(set-face-attribute 'help-key-binding nil
                    :foreground dn-foreground
                    :background dn-background
                    :weight 'bold
                    :box nil                                    )
(after! eros
(set-face-attribute 'eros-result-overlay-face nil
                     :foreground dn-foreground
                     :background dn-background
                     :weight 'bold
                     :box t                                     ))
(after! avy
(set-face-attribute 'avy-lead-face nil
                     :foreground dn-foreground
                     :background dn-subtle
                     :underline t                               )
(set-face-attribute 'avy-lead-face-1 nil
                     :foreground dn-foreground
                     :background dn-subtle
                     :underline t                               )
(set-face-attribute 'avy-lead-face-0 nil
                     :foreground dn-foreground
                     :background dn-subtle
                     :underline t                               ))
(after! lsp-ui
(set-face-attribute 'lsp-ui-doc-url nil
                      :background dn-contrast
                      :inherit nil
                      :underline t                              )
(set-face-attribute 'lsp-ui-doc-background nil
                      :background dn-contrast
                      :inherit nil
                      :box t                                    ))
(after! lsp-mode
(set-face-attribute 'lsp-signature-face nil
                      :background dn-contrast
                      :inherit nil                              )
(set-face-attribute 'lsp-face-highlight-read nil
                      :background dn-contrast
                      :underline nil
                      :inherit nil                              )
(set-face-attribute 'lsp-face-highlight-write nil
                      :background dn-contrast
                      :underline t
                      :inherit nil                              ))
(after! git-gutter-fringe
(set-face-attribute 'git-gutter-fr:added nil
                      :foreground dn-popout
                      :weight 'bold )
(set-face-attribute 'git-gutter:modified nil
                      :foreground dn-attention
                      :weight 'bold)
(set-face-attribute 'git-gutter-fr:modified nil
                      :foreground dn-attention
                      :weight 'bold))
(after! magit
(set-face-attribute 'magit-diff-hunk-heading-highlight nil
                      :background dn-medium
                      :box nil
                      :underline t                              )
(set-face-attribute 'magit-diff-hunk-heading nil
                      :background dn-medium                     )
(set-face-attribute 'magit-diff-context-highlight nil
                      :foreground dn-foreground
                      :background dn-background                 )
(set-face-attribute 'magit-diff-added-highlight nil
                      :foreground dn-popout
                      :background dn-background
                      :weight 'bold)
(set-face-attribute 'magit-diff-added nil
                      :foreground dn-popout
                      :background dn-background                 )
(set-face-attribute 'magit-diff-removed-highlight nil
                      :foreground dn-critical
                      :background dn-background
                      :weight 'bold                 )
(set-face-attribute 'magit-diff-removed nil
                      :foreground dn-critical
                      :background dn-background                 )
(set-face-attribute 'magit-hash nil
                      :foreground dn-grey
                      :weight 'bold)
(set-face-attribute 'magit-section-highlight nil
                      :background dn-contrast
                      :inherit nil                              ))
(after! info
(set-face-attribute 'info-xref-visited nil
                     :foreground dn-violet                      )
(set-face-attribute 'info-menu-star nil
                     :foreground dn-foreground                  ))
(after! popup
(set-face-attribute 'popup-tip-face nil
                     :background dn-contrast                    ))
(set-face-attribute 'link nil
                    :underline  t                               )
(set-face-attribute 'button nil
                    :underline  t                               )
(after! hl-line
(set-face-attribute 'hl-line nil :background dn-subtle          ))
(set-face-attribute 'cursor nil
                    :background dn-subtle )
(after! company
(set-face-attribute 'company-tooltip-search nil
                     :background dn-medium                      )
(set-face-attribute 'company-tooltip-common nil
                     :background dn-medium                      )
;(set-face-attribute 'company-box-scrollbar nil
;                    :background dn-medium                      )
(set-face-attribute 'company-tooltip-annotation nil
                     :background dn-medium                      )
(set-face-attribute 'company-scrollbar-bg nil
                     :background dn-medium                      )
(set-face-attribute 'company-scrollbar-fg nil
                     :background dn-medium                      ))
(after! diredfl
(set-face-attribute 'diredfl-dir-heading nil
                     :foreground dn-faded
                     :background nil
                     :weight 'bold
                     :underline  t                              )
(set-face-attribute 'diredfl-dir-name nil
                     :foreground dn-salient
                     :background nil                            )
(set-face-attribute 'diredfl-compressed-file-suffix nil
                     :foreground dn-salient
                     :background nil                            )
(set-face-attribute 'diredfl-file-name nil
                     :foreground dn-foreground                  )
(set-face-attribute 'diredfl-deletion nil
                     :foreground dn-critical
                     :background dn-background                    )
:weight 'bold
(set-face-attribute 'diredfl-deletion-file-name nil
                     :foreground dn-critical                    )
(set-face-attribute 'diredfl-file-suffix nil
                     :foreground dn-faded                       ))
(set-face-attribute 'font-lock-builtin-face nil
                    :foreground dn-violet                       )
(defun vterm-faces
(                                            )
(set-face-attribute 'vterm-color-red nil
                     :foreground dn-critical
                     :background nil                            )
(set-face-attribute 'vterm-color-yellow nil
                     :foreground dn-attention
                     :background nil                            )
(set-face-attribute 'vterm-color-blue nil
                     :foreground dn-blue
                     :background nil                            )
(set-face-attribute 'vterm-color-magenta nil
                     :foreground dn-violet
                     :background nil                            )
(set-face-attribute 'vterm-color-green nil
                     :foreground dn-popout
                     :background nil                            ))
(add-hook 'vterm-mode-hook #'vterm-faces                        )
(after! hydra
(set-face-attribute 'hydra-face-red nil
                     :foreground dn-critical
                     :weight 'bold                              ))
(after! smartparens
(set-face-attribute 'show-paren-match nil
                     :foreground dn-violet
                     :background dn-blue-green
                     :weight 'bold
                     :inverse-video t                           )
(set-face-attribute 'sp-show-pair-match-face nil
                     :foreground dn-violet
                     :background dn-blue-green
                     :weight 'bold
                     :inverse-video t                           ))

; for reference
;(set-face-attribute face nil
;                    :foreground 'unspecified :background 'unspecified
;                    :family     'unspecified :slant      'unspecified
;                    :weight     'unspecified :height     'unspecified
;                    :underline  'unspecified :overline   'unspecified
;                    :box        'unspecified :inherit    style )

(provide 'nano-face-override)
