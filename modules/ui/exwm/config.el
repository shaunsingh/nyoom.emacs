;;; ui/exwm/config.el -*- lexical-binding: t; -*-

;; Make the launcher only show app names
(use-package! counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

(defun playerctl-format (function format)
  "Invoke playerctl for FUNCTION using FORMAT to present output"
  (string-trim (shell-command-to-string (format "playerctl %s --format '%s'" function format))))

(defun exwm-get-index (index)
  "Get the correct index from the passed index"
  (- index 1))

(defun run-application (command)
  "Run the specified command as an application"
  (call-process "gtk-launch" nil 0 nil command))

(defun exwm-update-class ()
  "Update the buffer to be the name of the window"
  (exwm-workspace-rename-buffer exwm-class-name))

(defun run-in-background (command &optional args)
  "Run the specified command as a daemon"
  (kill-process--action (assoc command process-alist))
  (setq process-alist
        (cons `(,command . ,(start-process-shell-command command nil (format "%s %s" command (or args "")))) process-alist)))

(defun reload-tray ()
  "Restart the systemtray"
  (interactive)
  (exwm-systemtray--exit)
  (exwm-systemtray--init))

(defun exwm-update-title ()
  "Update the window title when needed"
  (pcase (downcase (or exwm-class-name ""))
    ("spotify" (exwm-workspace-rename-buffer (format "Spotify: %s" (playerctl-format "--player=spotify metadata" "{{ artist }} - {{ title }}"))))
    (_ (exwm-workspace-rename-buffer (format "%s" exwm-title)))))

(defun configure-window-by-class()
  "Configuration for windows (grouped by WM_CLASS)"
  (interactive)
  (pcase (downcase (or exwm-class-name ""))
    ("mpv" (progn
             (exwm-floating-toggle-floating)
             (exwm-layout-toggle-mode-line)))
    ("discord" (progn
                 (exwm-workspace-move-window (exwm-get-index 3))
                 (reload-tray)))
    ("megasync" (progn
                  (exwm-floating-toggle-floating)
                  (exwm-layout-toggle-mode-line)))
    ("spotify" (exwm-workspace-move-window (exwm-get-index 4)))
    ("firefox" (exwm-workspace-move-window (exwm-get-index 2)))))

(defun exwm-init-hook ()
  "Various init processes for exwm"
  ;; Daemon applications
  (run-in-background "pasystray")
  (run-in-background "megasync")
  (run-in-background "nm-applet")

  ;; Startup applications
  (run-application "spotify")
  (run-application "discord")
  (run-application "firefox")

  ;; Default emacs behaviours
  ;; TODO Take this out of emacs
  (mu4e t))

(defvar process-alist '())

(defun kill-process--action (process)
  "Do the actual process killing"
  (when process
    (ignore-errors
      (kill-process (cdr process))))
  (setq process-alist (remove process process-alist)))

(defun kill-process ()
  "Kill a background process"
  (interactive)
  (ivy-read "Kill process: " process-alist
            :action #'kill-process--action
            :caller 'kill-process))

;; Used to handle screen locking (currently unused), media keys and screenshotting
(use-package! desktop-environment
  :after exwm
  :config
  (setq desktop-environment-screenshot-command "flameshot gui")
  (desktop-environment-mode))

;; The meat and potatoes as they say
(use-package! exwm
  :commands (exwm-enable)
  :config
  ;; Enabled debugging when doom is in debug mode
  (when doom-debug-p
    (advice-add #'exwm-input--on-buffer-list-update :before
                (lambda (&rest r)
                (exwm--log "CALL STACK: %s" (cddr (reverse (xcb-debug:-call-stack))))))
    (exwm-debug))

  ;; Show all buffers for switching
  (setq exwm-workspace-show-all-buffers t)

  ;; Set a sane number of default workspaces
  (setq exwm-workspace-number 5)

  ;; Define workspace setup for monitors
  (setq exwm-randr-workspace-monitor-plist `(,(exwm-get-index 2) "DP-0" ,(exwm-get-index 3) "DP-0"))

  (setq exwm-workspace-index-map
        (lambda (index) (number-to-string (+ 1 index))))

  ;; Set the buffer to the name of the window class
  (add-hook 'exwm-update-class-hook #'exwm-update-class)

  ;; Init hook
  (add-hook 'exwm-init-hook #'exwm-init-hook)

  ;; Update window title
  (add-hook 'exwm-update-title-hook #'exwm-update-title)

  ;; Configure windows as created
  (add-hook 'exwm-manage-finish-hook #'configure-window-by-class)

  ;; /Always/ pass these to emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\C-w
          ?\M-`
          ?\M-&
          ?\M-:
          ?\M-\
          ?\C-g
          ?\C-\M-j
          ?\C-\ ))

  ;; Shortcut to passthrough next keys
  (map! :map exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Setup screen layout
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; TODO Change this to work with my laptops
  (start-process-shell-command "xrandr" nil "xrandr --output HDMI-0 --primary --mode 2560x1440 --pos 0x1080 --output DP-0 --mode 2560x1080 --pos 0x0")

  ;; Setup tray
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)

  ;; Date/time
  (setq display-time-format " [ %H:%M %d/%m/%y]")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  (exwm-input-set-key (kbd "<s-return>") '+eshell/toggle)

  (setq exwm-input-global-keys
        '(
          ([?\s- ] . counsel-linux-app)
          ([?\s-r] . exwm-reset)
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-&] . (lambda (command) (interactive (list (read-shell-command "[command] $ ")))
                       (start-process-shell-command command nil command)))

          ([?\s-e] . (lambda () (interactive) (dired "~")))

          ([?\s-w] . exwm-workspace-switch)

          ([?\s-Q] . (lambda () (interactive) (kill-buffer)))
          ([?\s-1] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 1))))
          ([?\s-2] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 2))))
          ([?\s-3] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 3))))
          ([?\s-4] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 4))))
          ([?\s-5] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 5))))
          ([?\s-6] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 6))))
          ([?\s-7] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 7))))
          ([?\s-8] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 8))))
          ([?\s-9] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create (exwm-get-index 9)))))))
