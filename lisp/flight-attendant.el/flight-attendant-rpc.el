;;; flight-attendant-rpc.el --- Minor mode to provide Github Copilot support -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;; Author: cryptobadger <https://github.com/cryptobadger>
;; Maintainer: cryptobadger
;; Created: December 02, 2021
;; Modified: December 02, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cryptobadger/flight-attendant.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Major mode to provide Github Copilot support
;;  taken pretty much from elpy.
;;
;;; Code:
;;;

(require 'json)

(message "flight-attendant-rpc.el loaded")

(defvar fa-libroot (file-name-directory (or load-file-name buffer-file-name))
  "Probably only interim. do not really know why this is here.")
(defvar gh-token nil)
(defvar gh-ratelimit-remaining nil)
(defvar gh-ratelimit-reset nil)
(defvar gh-token-expires nil)
(defcustom fa-copilot-path (concat fa-libroot "copilot/dist/agent.js")
  "The full path to the copilot executable 'agent.js'."
  :group 'fa)

(defcustom fa-oauth-token (cdr (caddar (json-read-file "~/.config/github-copilot/hosts.json")))
  "The github oauth token. Usually found in ~/.config/github-copilot/hosts.json."
  :group 'fa)
(defcustom fa-rpc-maximum-buffer-age (* 5 60)
  "Seconds after which Fa automatically closes an unused RPC buffer.
Fa creates RPC buffers over time, depending on python interpreters
and the project root. When there are many projects being worked on,
these can accumulate. Setting this variable to an integer will close
buffers and processes when they have not been used for this amount of
seconds.
Setting this variable to nil will disable the behavior."
  :type '(choice (const :tag "Never" nil)
                 integer)
  :group 'fa)

(defcustom fa-rpc-large-buffer-size 4096
  "Size for a source buffer up to which it will be sent directly.
The Fa RPC protocol uses JSON as the serialization format.
Large buffers take a long time to encode, so Fa can transmit
them via temporary files. If a buffer is larger than this value,
it is sent via a temporary file."
  :type 'integer
  :safe #'integerp
  :group 'fa)

(defcustom fa-rpc-ignored-buffer-size 102400
  "Size for a source buffer over which Fa completion will not work.
To provide completion, Fa's backends have to parse the whole
file every time. For very large files, this is slow, and can make
Emacs laggy. Fa will simply not work on buffers larger than
this to prevent this from happening."
  :type 'integer
  :safe #'integerp
  :group 'fa)

(defcustom fa-rpc-node-command (executable-find "node")
  "The Python interpreter for the RPC backend.
This should NOT be an interactive shell like ipython or jupyter.
As the RPC should be independent of any virtual environment, Fa
will try to use the system interpreter if it exists. If you wish
to use a specific python interpreter (from a virtual environment
for example), set this to the full interpreter path."
  ;; Make sure there is no obsolete rpc running
  :set (lambda (var val)                ;
         (set-default var val)
         (when (and (fboundp 'fa-rpc-restart)
                    (not (autoloadp #'fa-rpc-restart)))
           (fa-rpc-restart)))
  :group 'fa)


(defcustom fa-rpc-timeout 1
  "Number of seconds to wait for a response when blocking.
When Fa blocks Emacs to wait for a response from the RPC
process, it will assume it won't come or wait too long after this
many seconds. On a slow computer, or if you have a large project,
you might want to increase this.
A setting of nil means to block indefinitely."
  :type '(choice (const :tag "Block indefinitely" nil)
                 integer)
  :safe (lambda (val)
          (or (integerp val)
              (null val)))
  :group 'fa)

(defcustom fa-rpc-error-timeout 30
  "Minimum number of seconds between error popups.
When Fa encounters an error in the backend, it will display a
lengthy description of the problem for a bug report. This hangs
Emacs for a moment, and can be rather annoying if it happens
repeatedly while editing a source file.
If this variabl is non-nil, Fa will not display the error
message again within this amount of seconds."
  :type 'integer
  :group 'fa)

(defvar fa-rpc--call-id 0
  "Call id of the last call to `fa-rpc`.
Used to associate responses to callbacks.")
(make-variable-buffer-local 'fa-rpc--call-id)

(defvar fa-rpc--buffer-p nil
  "Non-nil if the current buffer is an fa-rpc buffer.")
(make-variable-buffer-local 'fa-rpc--buffer-p)

(defvar fa-rpc--buffer nil
  "The fa-rpc buffer associated with this buffer.")
(make-variable-buffer-local 'fa-rpc--buffer)

(defvar fa-rpc--backend-library-root nil
  "The project root used by this backend.")
(make-variable-buffer-local 'fa-rpc--backend-library-root)

(defvar fa-rpc--backend-callbacks nil
  "The callbacks registered for calls to the current backend.
This maps call IDs to functions.")
(make-variable-buffer-local 'fa-rpc--backend-callbacks)

(defvar fa-rpc--last-call nil
  "The time of the last RPC call issued for this backend.")
(make-variable-buffer-local 'fa-rpc--last-call)

(defvar fa-rpc--last-error-popup nil
  "The last time an error popup happened.")

(defvar fa-rpc--jedi-available nil
  "Whether jedi is available or not.")


(defmacro fa-insert--popup (buffer-name &rest body)
  "Pop up a help buffer named BUFFER-NAME and execute BODY in it."
  (declare (indent 1))
  `(with-help-window ,buffer-name
     (with-current-buffer standard-output
       ,@body)))

(defun fa-rpc--default-error-callback (error-object)
  "Display an error from the RPC backend."
  ;; We actually might get an (error "foo") thing here.
  (if (and (consp error-object)
           (not (consp (car error-object))))
      (signal (car error-object) (cdr error-object))
    (let ((message (cdr (assq 'message error-object)))
          (code (cdr (assq 'code error-object)))
          (data (cdr (assq 'data error-object))))
      (cond
       ((not (numberp code))
        (error "Bad response from RPC: %S" error-object))
       ((< code 300)
        (message "Fa warning: %s" message))
       ((< code 500)
        (error "Fa error: %s" message))
       ((= code -3001)
        (error "Fa error - Offline?: %s" message))
       ((and fa-rpc-error-timeout
             fa-rpc--last-error-popup
             (<= (float-time)
                 (+ fa-rpc--last-error-popup
                    fa-rpc-error-timeout)))
        (message "Fa error popup ignored, see `fa-rpc-error-timeout': %s"
                 message))
       (t
        (let ((config nil))
          (fa-insert--popup "*Fa Error*"
            (fa-insert--header "Fa Error")
            (fa-insert--para
             "The backend encountered an unexpected error. This indicates "
             "a bug in Fa. Please open a bug report with the data below "
             "in the Fa bug tracker:")
            (insert "\n"
                    "\n")
            (insert-button
             "https://github.com/cryptobadger/flight-attendant.el/issues/new"
             'action (lambda (button)
                       (browse-url (button-get button 'url)))
             'url "https://github.com/flight-attendant.el/issues/new")
            (insert "\n"
                    "\n"
                    "```\n")
            (fa-insert--header "Error Message")
            (insert message "\n\n"))
          (setq fa-rpc--last-error-popup (float-time))))))))

(defun fa-insert--para (&rest messages)
  "Insert MESSAGES, a list of strings, and then fill it."
  (let ((start (point)))
    (mapc (lambda (obj)
            (if (stringp obj)
                (insert obj)
              (insert (format "%s" obj))))
          messages)
    (fill-region start (point))))
(defun fa-insert--header (&rest text)
  "Insert TEXT has a header for a buffer."
  (insert (propertize (mapconcat #'(lambda (x) x)
                                 text
                                 "")
                      'face 'header-line)
          "\n"
          "\n"))
(defun fa-rpc--buffer-contents ()
  "Return the contents of the current buffer.
This returns either a string, or a file object for the RPC
protocol if the buffer is larger than
`fa-rpc-large-buffer-size'."
  (if (< (buffer-size) fa-rpc-large-buffer-size)
      (buffer-string)
    (let ((file-name (make-temp-file "fa-rpc-"))
          (coding-system-for-write 'utf-8))
      (write-region nil nil file-name nil :nomessage)
      `((filename . ,file-name)
        (delete_after_use . t)))))

(defun fa-rpc (method params &optional success error)
  "Call METHOD with PARAMS in the backend.
If SUCCESS and optionally ERROR is given, return immediately and
call those when a result is available. Otherwise, wait for a
result and return that."
  (unless error
    (setq error #'fa-rpc--default-error-callback))
  (if success
      (fa-rpc--call method params success error)
    (fa-rpc--call-blocking method params)))

(defun fa-rpc--call-blocking (method-name params)
  "Call METHOD-NAME with PARAMS in the current RPC backend.
Returns the result, blocking until this arrived."
  (let* ((result-arrived nil)
         (error-occured nil)
         (result-value nil)
         (error-object nil)
         (promise (fa-rpc--call method-name params
                                (lambda (result)
                                  (setq result-value result
                                        result-arrived t))
                                (lambda (err)
                                  (setq error-object err
                                        error-occured t)))))
    (fa-promise-wait promise fa-rpc-timeout)
    (cond
     (error-occured
      (fa-rpc--default-error-callback error-object))
     (result-arrived
      result-value)
     (t
      (error "Timeout during RPC call %s from backend"
             method-name)))))

(defun fa-rpc--call (method-name params success error)
  "Call METHOD-NAME with PARAMS in the current RPC backend.
When a result is available, SUCCESS will be called with that
value as its sole argument. If an error occurs, ERROR will be
called with the error list.
Returns a PROMISE object."
  (let ((promise (fa-promise success error)))
    (with-current-buffer (fa-rpc--get-rpc-buffer)
      (setq fa-rpc--call-id (1+ fa-rpc--call-id)
            fa-rpc--last-call (float-time))
      (fa-rpc--register-callback fa-rpc--call-id promise)
      (process-send-string
       (get-buffer-process (current-buffer))
       (let ((json-encoding-pretty-print nil))  ;; Link to bug https://github.com/jorgenschaefer/ec/issues/1521
         (concat (json-encode `((jsonrpc . "2.0")
                                (id . ,fa-rpc--call-id)
                                (method . ,method-name)
                                (params . ,params)))
                 "\n"))))
    promise))

(defun fa-rpc--register-callback (call-id promise)
  "Register for PROMISE to be called when CALL-ID returns.
Must be called in an fa-rpc buffer."
  (unless fa-rpc--buffer-p
    (error "Must be called in RPC buffer"))
  (unless fa-rpc--backend-callbacks
    (setq fa-rpc--backend-callbacks (make-hash-table :test #'equal)))
  (puthash call-id promise fa-rpc--backend-callbacks))

(defun fa-rpc--get-rpc-buffer ()
  "Return the RPC buffer associated with the current buffer,
creating one if necessary."
  (unless (fa-rpc--process-buffer-p fa-rpc--buffer)
    (setq fa-rpc--buffer
          (or (fa-rpc--find-buffer)
              (fa-rpc--open))))
  fa-rpc--buffer)

(defun fa-rpc--process-buffer-p (buffer)
  "Return non-nil when BUFFER is a live fa-rpc process buffer.
If BUFFER is a buffer for an fa-rpc process, but the process
died, this will kill the process and buffer."
  (cond
   ((or (not buffer)
        (not (buffer-live-p buffer)))
    nil)
   ((not (buffer-local-value 'fa-rpc--buffer-p buffer))
    nil)
   ((and (get-buffer-process buffer)
         (process-live-p (get-buffer-process buffer)))
    t)
   (t
    (ignore-errors
      (kill-process (get-buffer-process buffer)))
    (ignore-errors
      (kill-buffer buffer))
    nil)))

(defun fa-rpc--cleanup-buffers ()
  "Close RPC buffers that have not been used in five minutes."
  (when fa-rpc-maximum-buffer-age
    (let ((old (- (float-time)
                  fa-rpc-maximum-buffer-age)))
      (dolist (buffer (buffer-list))
        (when (and (fa-rpc--process-buffer-p buffer)
                   (< (or (buffer-local-value 'fa-rpc--last-call buffer)
                          old)
                      old))
          (ignore-errors
            (kill-process (get-buffer-process buffer)))
          (ignore-errors
            (kill-buffer buffer)))))))

(defun fa-rpc--find-buffer ()
  "Return an existing RPC buffer for this project root and command."
  (catch 'return
    (let ((node-cmd fa-rpc-node-command))
      (dolist (buf (buffer-list))
        (when (and (fa-rpc--process-buffer-p buf))
          (throw 'return buf))))
    nil))

(defun fa-config-error (&optional fmt &rest args)
  "Note a configuration problem.

FMT is the formating string.

This will show a message in the minibuffer that tells the user to
use \\[fa-config]."
  (let ((msg (if fmt
                 (apply #'format fmt args)
               "Fa is not properly configured")))
    (error "%s; use M-x fa-config to configure it" msg)))

(defun fa-rpc--open ()
  "Start a new RPC process and return the associated buffer."
  (fa-rpc--cleanup-buffers)
  (let* ((node-cmd (executable-find fa-rpc-node-command))
         (name (format " *fa-rpc [project:%s]*"
                       fa-libroot))
         (new-fa-rpc-buffer (generate-new-buffer name))
         (proc nil))
    (unless node-cmd
      (error "Can't find Node command, configure `fa-rpc-node-command'"))
    (with-current-buffer new-fa-rpc-buffer
      (setq fa-rpc--buffer-p t
            fa-rpc--buffer (current-buffer)
            fa-rpc--backend-library-root fa-libroot
            default-directory "/"
            proc (condition-case err
                     (let ((process-connection-type nil))
                       (start-process name
                                      (current-buffer)
                                      node-cmd fa-copilot-path))
                   (error
                    (fa-config-error
                     "Fa can't start Python (%s: %s)"
                     (car err) (cadr err)))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'fa-rpc--sentinel)
      (set-process-filter proc #'fa-rpc--filter)
      (fa-rpc-init))
      ;;  (lambda (result)
      ;;    (message (cdr (assq 'version result))))))
    new-fa-rpc-buffer))


(defun fa-rpc--region-contents ()
  "Return the selected region as a string."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))))

(defun fa-rpc--disconnect ()
  "Disconnect rpc process from fa buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when fa-mode
        (setq fa-rpc--buffer nil)))))

;; RPC API functions

(defun fa-rpc-restart ()
  "Restart all RPC processes."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (fa-rpc--process-buffer-p buffer)
      (ignore-errors
        (kill-process (get-buffer-process buffer)))
      (ignore-errors
        (kill-buffer buffer)))))

(defun fa-rpc-init (&optional success error)
  "Initialize the backend.

This has to be called as the first method, else Fa won't be
able to respond to other calls.

+LIBRARY-ROOT is the current project root,
+ENVIRONMENT-BINARIES is the path to the python binaries of the environment to work in."
  (message "Starting copilot")
  (message (concat "Github Copilot started - " (cdr (assq 'version
                                                        (fa-rpc "getVersion"
                                                    ;; This uses a vector because otherwise, json-encode in
                                                    ;; older Emacsen gets seriously confused, especially when
                                                    ;; backend is nil.
                                                                  (make-hash-table))))))
  (fa-rpc-get-token))
'((version . 1.7.4))
(defun fa-rpc--sentinel (process event)
  "The sentinel for the RPC process.
As process sentinels are only ever called when the process
terminates, this will call the error handler of all registered
RPC calls with the event."
  (let ((buffer (process-buffer process))
        (err (list 'process-sentinel (substring event 0 -1))))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (when fa-rpc--backend-callbacks
          (maphash (lambda (_call-id promise)
                     (ignore-errors
                       (fa-promise-reject promise err)))
                   fa-rpc--backend-callbacks)
          (setq fa-rpc--backend-callbacks nil))))))

(defun fa-rpc--filter (process output)
  "The filter for the RPC process."
  (let ((buffer (process-buffer process)))
    (when (and buffer
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output)
        (while (progn
                 (goto-char (point-min))
                 (search-forward "\n" nil t))
          (let ((line-end (point))
                (json nil)
                (did-read-json nil))
            (goto-char (point-min))
            (condition-case _err
                (progn
                  (setq json (let ((json-array-type 'list)
                                   (json-false nil)
                                   (json-encoding-pretty-print nil))  ;; Link to bug https://github.com/jorgenschaefer/ec/issues/1521
                               (json-read)))
                  (if (listp json)
                      (setq  line-end (1+ (point))
                             did-read-json t)
                    (goto-char (point-min))))
              (error
               (goto-char (point-min))))
            (cond
             (did-read-json
              (delete-region (point-min) line-end)
              (fa-rpc--handle-json json))
             ;; ((looking-at "fa-rpc ready\n")
             ;;  (replace-match "")
             ;;  (fa-rpc--check-backend-version "1.1"))
             ;; ((looking-at "fa-rpc ready (\\([^ ]*\\))\n")
             ;;  (let ((rpc-version (match-string 1)))
             ;;    (replace-match "")
             ;;    (fa-rpc--check-backend-version rpc-version)))
             ;; ((looking-at ".*No module named fa\n")
             ;;  (replace-match "")
             ;;  (fa-config-error "Fa module not found"))
             ((looking-at ".*request cancelled")
              (replace-match "")
              (message "Request Cancelled")
              (fa-clear-overlay))
             (t
              (let ((line (buffer-substring (point-min)
                                            line-end)))
                (delete-region (point-min) line-end)
                (fa-rpc--handle-unexpected-line line))))))))))

(defun fa-rpc--handle-unexpected-line (line)
  "Handle an unexpected line from the backend.
This is usually an error or backtrace."
  (let ((buf (get-buffer "*Fa Output*")))
    (unless buf
      (fa-insert--popup "*Fa Output*"
        (fa-insert--header "Output from Backend")
        (fa-insert--para
         "There was some unexpected output from the Fa backend. "
         "This is usually not a problem and should usually not be "
         "reported as a bug with Fa. You can safely hide this "
         "buffer and ignore it. You can also see the output below "
         "in case there is an actual problem.\n\n")
        (fa-insert--header "Output")
        (setq buf (current-buffer))))
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert line)))))

(defun fa-rpc--handle-json (json)
  "Handle a single JSON object from the RPC backend."
  (let ((call-id (cdr (assq 'id json)))
        (error-object (cdr (assq 'error json)))
        (result (cdr (assq 'result json))))
    (let ((promise (gethash call-id fa-rpc--backend-callbacks)))
      (unless promise
        (error "Received a response for unknown call-id %s" call-id))
      (remhash call-id fa-rpc--backend-callbacks)
      (if error-object
          (fa-promise-reject promise error-object)
        (fa-promise-resolve promise result)))))

;; promises
;;

(defvar fa-promise-marker (make-symbol "*fa-promise*")
  "An uninterned symbol marking an Fa promise object.")

(defun fa-promise (success &optional error)
  "Return a new promise.
A promise is an object with a success and error callback. If the
promise is resolved using `fa-promise-resolve', the SUCCESS
callback is called with the given value. The current buffer is
restored, too.
If the promise is rejected using `fa-promise-reject', the ERROR
callback is called. For this function, the current buffer is not
necessarily restored, as it is also called when the buffer does
not exist anymore."
  (vector fa-promise-marker ; 0 id
          success             ; 1 success-callback
          error               ; 2 error-callback
          (current-buffer)    ; 3 current-buffer
          nil))                 ; 4 run


(defun fa-promise-p (obj)
  "Return non-nil if OBJ is a promise object."
  (and (vectorp obj)
       (= (length obj) 5)
       (eq (aref obj 0) fa-promise-marker)))

(defsubst fa-promise-success-callback (promise)
  "Return the success callback for PROMISE."
  (aref promise 1))

(defsubst fa-promise-error-callback (promise)
  "Return the error callback for PROMISE."
  (aref promise 2))

(defsubst fa-promise-buffer (promise)
  "Return the buffer for PROMISE."
  (aref promise 3))

(defsubst fa-promise-resolved-p (promise)
  "Return non-nil if the PROMISE has been resolved or rejected."
  (aref promise 4))

(defsubst fa-promise-set-resolved (promise)
  "Mark PROMISE as having been resolved."
  (aset promise 4 t))

(defun fa-promise-resolve (promise value)
  "Resolve PROMISE with VALUE."
  (unless (fa-promise-resolved-p promise)
    (unwind-protect
        (let ((success-callback (fa-promise-success-callback promise)))
          (when success-callback
            (condition-case err
                (with-current-buffer (fa-promise-buffer promise)
                  (funcall success-callback value))
              (error
               (fa-promise-reject promise err)))))
      (fa-promise-set-resolved promise))))

(defun fa-promise-reject (promise reason)
  "Reject PROMISE because of REASON."
  (unless (fa-promise-resolved-p promise)
    (unwind-protect
        (let ((error-callback (fa-promise-error-callback promise)))
          (when error-callback
            (if (buffer-live-p (fa-promise-buffer promise))
                (with-current-buffer (fa-promise-buffer promise)
                  (funcall error-callback reason))
              (with-temp-buffer
                (funcall error-callback reason)))))
      (fa-promise-set-resolved promise))))

(defun fa-promise-wait (promise &optional timeout)
  "Wait for PROMISE to be resolved, for up to TIMEOUT seconds.
This will accept process output while waiting.
This will wait for the current Fa RPC process specifically, as
Emacs currently has a bug where it can wait for the entire time
of the timeout, even if output arrives.
See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17647"
  (let ((end-time (when timeout
                    (time-add (current-time)
                              (seconds-to-time timeout))))
        (process (get-buffer-process (fa-rpc--get-rpc-buffer))))
    (while (and (not (fa-promise-resolved-p promise))
                (or (not end-time)
                    (time-less-p (current-time)
                                 end-time)))
      (accept-process-output process timeout))))


;;; Commands

(defun fa-rpc-get-token (&optional success error) ;
  "Call the get_token API function.

Not async because we have no id to keep track of the promise"
  (let ((result (fa-rpc "httpRequest"
                       `((:headers
                          (:Authorization . ,(concat "Bearer " fa-oauth-token)))
                         (:url . "https://api.github.com/copilot_internal/token")
                         (:timeout . 30000)))))
      (setq gh-token (cdr (assq 'token (json-read-from-string (cdr (assq 'body result))))))
      (setq gh-ratelimit-remaining (cdr (assq 'x-ratelimit-remaining (assq 'headers result))))
      (setq gh-ratelimit-reset (cdr (assq 'x-ratelimit-reset (assq 'headers result))))
      (setq gh-token-expires (cdr (assq 'expires_at (json-read-from-string (cdr (assq 'body result))))))))

(defun fa--get-path ()
    (or buffer-file-name
        (ignore-errors
          (buffer-file-name
           (buffer-base-buffer)))))
(defun fa--get-relative-path ()
    (file-name-nondirectory buffer-file-name))

(defun fa--get-buffer-str ()
  (let ((pos))
    (list :line (1- (line-number-at-pos pos t)) ; F!@&#$CKING OFF-BY-ONE
          :character (progn (when pos (goto-char pos))
                            (funcall coglot-current-column-function)))))


(defun fa--get-line-no ()
  (1- (line-number-at-pos)))
(defun fa--get-col-no ()
  (current-column))
(defun fa--get-buffer-text ()
  (buffer-substring-no-properties (point-min) (point-max)))
(defun fa--get-tab-width ()
  tab-width)
(defun fa--get-indent-size ()
  tab-width)
(defun fa--get-insert-spaces ()
  t)
(defvar language-ids '((python-mode . "python")
                       ;; (emacs-lisp-mode . "python")
                       (typescript-mode . "typescript")))
(defun fa--get-language-id ()
  (cdr (assq major-mode language-ids)))

(defun fa--get-completion-params ()
  `((:options . ,(make-hash-table))
    (:token . ,gh-token)
    (:doc . ((:path . ,(fa--get-path))
             (:relativePath . ,(fa--get-relative-path))
             (:source . ,(fa--get-buffer-text))
             (:tabSize . ,(fa--get-tab-width))
             (:indentSize . ,(fa--get-indent-size))
             (:languageId . ,(fa--get-language-id))
             (:insertSpaces . ,(fa--get-insert-spaces))
             (:position . ((:character . ,(fa--get-col-no))
                           (:line . ,(fa--get-line-no))))))))

(defun fa-rpc-request-completion (&optional success error)
  "Call the get_calltip API function.

Returns a calltip string for the function call at point."
  (when (and (< (buffer-size) fa-rpc-ignored-buffer-size)
             (fa--get-language-id)
             gh-token)
    (fa-rpc "getCompletions"
            (fa--get-completion-params)
            success
            (lambda (err) (message err) (setq fa-error err)))))

(defvar fa-result nil)
(defvar fa-error nil)
(provide 'flight-attendant-rpc)
;;; flight-attendant-rpc.el ends here
