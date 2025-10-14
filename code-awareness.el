;;; code-awareness.el --- Code Awareness collaboration package -*- lexical-binding: t -*-

;; Author: Mark Vasile <mark@code-awareness.com>
;; Package-Requires: ((emacs "27.1"))
;; Keywords: code awareness, collaboration, development, convenience tools
;; Homepage: https://github.com/CodeAwareness/ca.emacs

;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This package is licensed under GPLv3. It depends on the Code Awareness
;; binary, available at https://code-awareness.com

;;; Commentary:

;; Code Awareness highlights the code intersections between your working
;; copy and other team members. This provides an early warning system
;; for merge conflicts, as well as instant traveling between working
;; copies of multiple developers without needing to commit and push.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'code-awareness-pipe)
(require 'code-awareness-list-pipe)
(require 'code-awareness-process-sockets)
(require 'hl-line nil t)

;;; Configuration

(defconst code-awareness--caw-schema "caw"
  "Schema for Code Awareness URIs.")

(defconst code-awareness--extract-repo-dir "extract"
  "Directory name for extracted repository files.")

(defconst code-awareness--pipe-catalog "catalog"
  "Catalog pipe name.")

;;;###autoload
(defgroup code-awareness-config nil
  "Code Awareness configuration."
  :group 'code-awareness
  :prefix "code-awareness-")

;;;###autoload
(defcustom code-awareness-catalog "catalog"
  "Catalog name for Code Awareness."
  :type 'string
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-highlight-intensity 0.3
  "Intensity of highlighting (0.0 to 1.0)."
  :type 'number
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-highlight-refresh-delay 0.5
  "Delay in seconds before refreshing highlights after changes."
  :type 'number
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-highlight-persistent nil
  "Whether highlights should persist across buffer switches."
  :type 'boolean
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-full-width-highlights t
  "Whether to use full-width highlights that extend to the end of the line."
  :type 'boolean
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-update-delay 0.5
  "Delay in seconds before running a Code Awareness update."
  :type 'number
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-debug nil
  "Enable debug mode for Code Awareness."
  :type 'boolean
  :group 'code-awareness-config)

;;; Theme Support

;;;###autoload
(defcustom code-awareness-change-color-light "#00b1a420"
  "Color for changed lines in light theme."
  :type 'string
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-change-color-dark "#03445f"
  "Color for changed lines in dark theme."
  :type 'string
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-peer-color-light "#ffdd34"
  "Color for peer code in light theme."
  :type 'string
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-peer-color-dark "#1f1cc2"
  "Color for peer code in dark theme."
  :type 'string
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-merge-color-light "#ffc000"
  "Color for merged code in light theme."
  :type 'string
  :group 'code-awareness-config)

;;;###autoload
(defcustom code-awareness-merge-color-dark "#141299"
  "Color for merged code in dark theme."
  :type 'string
  :group 'code-awareness-config)

;;; Utility Functions

(defun code-awareness--get-theme-color (light-color dark-color)
  "Get the appropriate color for the current theme.
Argument LIGHT-COLOR color for light theme.
Argument DARK-COLOR color for dark theme."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      dark-color
    light-color))

(defun code-awareness--get-change-color ()
  "Get the color for changed lines."
  (code-awareness--get-theme-color
   code-awareness-change-color-light
   code-awareness-change-color-dark))

(defun code-awareness--get-peer-color ()
  "Get the color for peer code."
  (code-awareness--get-theme-color
   code-awareness-peer-color-light
   code-awareness-peer-color-dark))

(defun code-awareness--get-merge-color ()
  "Get the color for merged code."
  (code-awareness--get-theme-color
   code-awareness-merge-color-light
   code-awareness-merge-color-dark))


;;; Customization

(defgroup code-awareness nil
  "Code Awareness, low noise collaboration."
  :group 'applications
  :prefix "code-awareness-")

;;; Internal Variables

(defvar code-awareness--guid nil
  "Unique identifier for this Emacs instance.")

(defvar code-awareness--client-registered nil
  "Whether the client has been registered with the catalog service.")

(defvar code-awareness--ipc-process nil
  "IPC process for communicating with the Code Awareness IPC.")

(defvar code-awareness--ipc-catalog-process nil
  "IPC process for catalog communication.")

(defvar code-awareness--response-handlers (make-hash-table :test 'equal)
  "Hash table of response handlers for IPC requests.")

(defvar code-awareness--active-project nil
  "Currently active project data.")

(defvar code-awareness--active-buffer nil
  "Currently active buffer.")

(defvar code-awareness--poll-attempts 0
  "Number of polling attempts for Code Awareness IPC socket.")

(defvar code-awareness--update-timer nil
  "Timer for debounced updates.")

(defvar code-awareness--connected nil
  "Whether we're connected to the Code Awareness IPC.")

(defvar code-awareness--config nil
  "Configuration data.")

;;; Logging Variables

(defvar code-awareness--log-buffer "*Code Awareness Log*"
  "Buffer name for Code Awareness logs.")

(defvar code-awareness--log-level 'info
  "Current log level.")

;;; Log Levels

(defconst code-awareness--log-levels
  '((error . 0)
    (warn . 1)
    (info . 2)
    (log . 3)
    (debug . 4))
  "Log levels with their numeric values.")

;;; Logging Utility Functions

(defun code-awareness--get-log-level-value (level)
  "Get the numeric value for a log LEVEL."
  (cdr (assoc level code-awareness--log-levels)))

(defun code-awareness--should-log (level)
  "Check if the given LEVEL should be logged."
  (<= (code-awareness--get-log-level-value level)
      (code-awareness--get-log-level-value code-awareness--log-level)))

(defun code-awareness--write-to-log-buffer (message)
  "Write a MESSAGE to the log buffer."
  (let ((buffer (get-buffer-create code-awareness--log-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert message "\n")
      (when (and code-awareness-debug (get-buffer-window buffer))
        (recenter -1)))))

;;; Logging Public API

;;;###autoload
(defun code-awareness-log-error (message &rest args)
  "Log an error MESSAGE.
Optional argument ARGS optional formatting ."
  (when (code-awareness--should-log 'error)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [ERROR] %s" timestamp formatted-message)))
      (code-awareness--write-to-log-buffer log-entry)
      (message "Code Awareness Error: %s" formatted-message))))

;;;###autoload
(defun code-awareness-log-warn (message &rest args)
  "Log a warning MESSAGE.
Optional argument ARGS optional formatting."
  (when (code-awareness--should-log 'warn)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [WARN] %s" timestamp formatted-message)))
      (code-awareness--write-to-log-buffer log-entry))))

;;;###autoload
(defun code-awareness-log-info (message &rest args)
  "Log an info MESSAGE.
Optional argument ARGS formatting."
  (when (code-awareness--should-log 'info)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [INFO] %s" timestamp formatted-message)))
      (code-awareness--write-to-log-buffer log-entry))))

;;;###autoload
(defun code-awareness-log (message &rest args)
  "Log a general MESSAGE.
Optional argument ARGS formatting."
  (when (code-awareness--should-log 'log)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [LOG] %s" timestamp formatted-message)))
      (code-awareness--write-to-log-buffer log-entry))))

;;;###autoload
(defun code-awareness-log-debug (message &rest args)
  "Log a debug MESSAGE.
Optional argument ARGS formatting."
  (when (and code-awareness-debug (code-awareness--should-log 'debug))
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [DEBUG] %s" timestamp formatted-message)))
      (code-awareness--write-to-log-buffer log-entry))))

;;;###autoload
(defun code-awareness-show-log-buffer ()
  "Show the Code Awareness log buffer."
  (interactive)
  (let ((buffer (get-buffer-create code-awareness--log-buffer)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

;;;###autoload
(defun code-awareness-clear-log-buffer ()
  "Clear the Code Awareness log buffer."
  (interactive)
  (let ((buffer (get-buffer code-awareness--log-buffer)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))

;;; Store/State Management

(defvar code-awareness--store nil
  "Central store for Code Awareness state.")

(defvar code-awareness--projects nil
  "List of all projects.")

(defvar code-awareness--active-selections nil
  "Currently active selections.")

(defvar code-awareness--selected-peer nil
  "Currently selected peer.")

(defvar code-awareness--color-theme 1
  "Current color theme (1=Light, 2=Dark, 3=High Contrast).")

(defvar code-awareness--tmp-dir (expand-file-name "caw.emacs" (temporary-file-directory))
  "Temporary directory for Code Awareness.")

(defvar code-awareness--peer-fs (make-hash-table :test 'equal)
  "Peer file system tree structure.")

(defvar code-awareness--events-table (make-hash-table :test 'equal)
  "Hash table mapping event names to handler functions.")

(defvar code-awareness--user nil
  "Current user data.")

(defvar code-awareness--tokens nil
  "Authentication tokens.")

(defvar code-awareness--authenticated nil
  "Whether the client is authenticated with the local service.")

;;; Highlighting System

(defvar code-awareness--highlights (make-hash-table :test 'equal)
  "Hash table of highlights by buffer, tracking line numbers and overlay objects.")

(defvar code-awareness--highlight-faces nil
  "Predefined faces for different types of highlights.")

(defvar code-awareness--highlight-timer nil
  "Timer for debounced highlight refresh.")

;;; HL-Line Integration

(defvar code-awareness--hl-line-overlays (make-hash-table :test 'equal)
  "Hash table of hl-line overlays by buffer and line number.")

(defvar code-awareness--hl-line-faces (make-hash-table :test 'equal)
  "Hash table of custom hl-line faces by highlight type.")

;;; Configuration

(defun code-awareness--init-config ()
  "Initialize configuration."
  (setq code-awareness--config
        `((catalog . ,code-awareness-catalog)
          (update-delay . ,code-awareness-update-delay)))
  (code-awareness-log-info "Configuration initialized"))

(defun code-awareness--init-store ()
  "Initialize the central store."
  (setq code-awareness--store
        `((active-project . ,code-awareness--active-project)
          (projects . ,code-awareness--projects)
          (active-buffer . ,code-awareness--active-buffer)
          (active-selections . ,code-awareness--active-selections)
          (selected-peer . ,code-awareness--selected-peer)
          (color-theme . ,code-awareness--color-theme)
          (tmp-dir . ,code-awareness--tmp-dir)
          (peer-fs . ,code-awareness--peer-fs)
          (user . ,code-awareness--user)
          (tokens . ,code-awareness--tokens)))
  (code-awareness-log-info "Store initialized"))

(defun code-awareness--register-event-handler (event-name handler-function)
  "Register an event handler function for the given event name.
Argument EVENT-NAME string in the format category:action, e.g. peer:select.
Argument HANDLER-FUNCTION a ref to the function that should handle the event."
  (puthash event-name handler-function code-awareness--events-table)
  (code-awareness-log-info "Registered event handler for: %s" event-name))

(defun code-awareness--init-event-handlers ()
  "Initialize all event handlers."
  ;; Clear existing handlers
  (clrhash code-awareness--events-table)

  ;; Register event handlers
  (code-awareness--register-event-handler "peer:select" #'code-awareness--handle-peer-select)
  (code-awareness--register-event-handler "peer:unselect" #'code-awareness--handle-peer-unselect)
  (code-awareness--register-event-handler "branch:select" #'code-awareness--handle-branch-select)
  (code-awareness--register-event-handler "branch:unselect" #'code-awareness--handle-branch-unselect)
  (code-awareness--register-event-handler "branch:refresh" #'code-awareness--handle-branch-refresh)
  (code-awareness--register-event-handler "auth:logout" #'code-awareness--handle-auth-logout)
  (code-awareness--register-event-handler "context:add" #'code-awareness--handle-context-add)
  (code-awareness--register-event-handler "context:del" #'code-awareness--handle-context-del)
  (code-awareness--register-event-handler "context:open-rel" #'code-awareness--handle-context-open-rel)
  ;; Add more event handlers here as needed

  (code-awareness-log-info "Event handlers initialized"))

(defun code-awareness--clear-store ()
  "Clear the store and reset all state."
  (code-awareness-log-info "Clearing store")
  (setq code-awareness--tokens nil
        code-awareness--user nil
        code-awareness--authenticated nil
        code-awareness--active-project nil
        code-awareness--active-buffer nil
        code-awareness--active-selections nil
        code-awareness--selected-peer nil
        code-awareness--color-theme 1
        code-awareness--tmp-dir (expand-file-name "caw.emacs" (temporary-file-directory))
        code-awareness--peer-fs (make-hash-table :test 'equal)
        code-awareness--poll-attempts 0)
  (code-awareness--init-store))

(defun code-awareness--reset-store ()
  "Reset store state (keep user/tokens)."
  (code-awareness-log-info "Resetting store")
  (setq code-awareness--peer-fs (make-hash-table :test 'equal)
        code-awareness--active-buffer nil
        code-awareness--active-selections nil)
  (code-awareness--init-store))

;;; Project Management

(defun code-awareness--add-project (project)
  "Add a PROJECT to the store."
  (code-awareness-log-info "Adding project %s" (alist-get 'root project))
  (setq code-awareness--active-project project)
  ;; Add to projects list if not already present
  (unless (cl-find (alist-get 'root project) code-awareness--projects
                   :key (lambda (p) (alist-get 'root p)) :test 'string=)
    (push project code-awareness--projects))
  (code-awareness--init-store)
  project)

(defun code-awareness--get-active-file-path ()
  "Get the path of the currently active file."
  (when (and code-awareness--active-buffer
             (buffer-live-p code-awareness--active-buffer))
    (buffer-file-name code-awareness--active-buffer)))

(defun code-awareness--get-active-file-content ()
  "Get the content of the currently active file."
  (when (and code-awareness--active-buffer
             (buffer-live-p code-awareness--active-buffer))
    (with-current-buffer code-awareness--active-buffer
      (buffer-string))))

(defun code-awareness--cross-platform-path (path)
  "Convert PATH to cross-platform format (forward slashes)."
  (when path
    (replace-regexp-in-string "\\\\" "/" path)))

;;; Workspace Management

;; The refreshActiveFile hook implementation follows the VSCode pattern:
;; 1. Called immediately after authentication is successful
;; 2. Called whenever the active buffer changes
;; 3. Sends a repo:active-path message to CodeAwareness app with the current file path and content
;; 4. Updates highlights and project data based on the response

(defun code-awareness--refresh-active-file ()
  "Refresh the currently active file by sending repo:active-path message."

  ;; Check if we have the necessary components to send a refresh request
  (let ((fpath (code-awareness--get-active-file-path))
        (doc (code-awareness--get-active-file-content)))
    (if (not fpath)
        (code-awareness-log-info "No active file to refresh")
      (if (not code-awareness--authenticated)
          (code-awareness-log-warn "Not authenticated, skipping file refresh")
        (if (not (and code-awareness--ipc-process
                      (eq (process-status code-awareness--ipc-process) 'open)))
            (code-awareness-log-warn "Code Awareness IPC process not ready, skipping file refresh")
          (code-awareness-log-info "Refreshing active file %s" fpath)
          (let ((message-data `((fpath . ,(code-awareness--cross-platform-path fpath))
                                (doc . ,doc)
                                (caw . ,code-awareness--guid))))
            (code-awareness--transmit "repo:active-path" message-data)
            (code-awareness--setup-response-handler "code" "repo:active-path" fpath)))))))

;;; Highlighting System

(defun code-awareness--init-highlight-faces ()
  "Initialize predefined faces for different highlight types."
  (setq code-awareness--highlight-faces
        `((conflict . ,(make-face 'code-awareness-conflict-face))
          (overlap . ,(make-face 'code-awareness-overlap-face))
          (peer . ,(make-face 'code-awareness-peer-face))
          (modified . ,(make-face 'code-awareness-modified-face))))

  ;; Set face attributes based on color theme
  (let ((conflict-face (alist-get 'conflict code-awareness--highlight-faces))
        (overlap-face (alist-get 'overlap code-awareness--highlight-faces))
        (peer-face (alist-get 'peer code-awareness--highlight-faces))
        (modified-face (alist-get 'modified code-awareness--highlight-faces)))

    ;; Detect if we're in a dark theme
    (let ((is-dark-theme (eq (frame-parameter nil 'background-mode) 'dark)))
      (if is-dark-theme
          ;; Dark theme colors
          (progn
            ;; Conflict highlights (red background for dark theme)
            (set-face-attribute conflict-face nil
                                :background "#4a1a1a"
                                :foreground "#ff6b6b"
                                :weight 'bold)

            ;; Overlap highlights (yellow/orange background for dark theme)
            (set-face-attribute overlap-face nil
                                :background "#4a3a1a"
                                :foreground "#ffd93d"
                                :weight 'normal)

            ;; Peer highlights (blue background for dark theme)
            (set-face-attribute peer-face nil
                                :background "#1a2a4a"
                                :foreground "#74c0fc"
                                :weight 'normal)

            ;; Modified highlights (green background for dark theme)
            (set-face-attribute modified-face nil
                                :background "#1a4a1a"
                                :foreground "#69db7c"
                                :weight 'normal))

        ;; Light theme colors
        (progn
          ;; Conflict highlights (red background for light theme)
          (set-face-attribute conflict-face nil
                              :background "#ffebee"
                              :foreground "#c62828"
                              :weight 'bold)

          ;; Overlap highlights (yellow background for light theme)
          (set-face-attribute overlap-face nil
                              :background "#fff8e1"
                              :foreground "#f57f17"
                              :weight 'normal)

          ;; Peer highlights (blue background for light theme)
          (set-face-attribute peer-face nil
                              :background "#e3f2fd"
                              :foreground "#1565c0"
                              :weight 'normal)

          ;; Modified highlights (green background for light theme)
          (set-face-attribute modified-face nil
                              :background "#e8f5e8"
                              :foreground "#2e7d32"
                              :weight 'normal)))))

  ;; Initialize hl-line faces if hl-line is available
  (when (featurep 'hl-line)
    (code-awareness--init-hl-line-faces))

  (code-awareness-log-info "Highlight faces initialized"))

(defun code-awareness--get-highlight-face (type)
  "Get the face for the given highlight TYPE."
  (alist-get type code-awareness--highlight-faces))

(defun code-awareness--create-line-overlay (buffer line-number face &optional properties)
  "Create an overlay for a specific line in the given BUFFER.
Uses hl-line technique to properly handle empty lines.
Argument LINE-NUMBER the zero-based line number to highlight.
Argument FACE the face to use for highlighting.
Optional argument PROPERTIES optional overlay properties (hl-lines)."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let* ((line-count (line-number-at-pos (point-max)))
             ;; Use save-excursion to get absolute line positions regardless of cursor position
             (start (save-excursion
                      ;; Suppress warning: goto-line is needed for absolute positioning
                      (with-suppressed-warnings ((interactive-only goto-line))
                        (goto-line line-number))
                      (line-beginning-position)))
             ;; Use hl-line technique: end at start of next line instead of end of current line
             ;; This ensures empty lines get proper overlay span
             (end (save-excursion
                    ;; Suppress warning: goto-line is needed for absolute positioning
                    (with-suppressed-warnings ((interactive-only goto-line))
                      (goto-line (1+ line-number)))
                    (line-beginning-position))))
        (when (and (<= line-number line-count) (>= line-number 1))
          (let ((overlay (make-overlay start end buffer t nil)))
            (overlay-put overlay 'face face)
            (overlay-put overlay 'code-awareness-type 'line-highlight)
            (overlay-put overlay 'code-awareness-line line-number)
            ;; Add any additional properties
            (when properties
              (dolist (prop properties)
                (overlay-put overlay (car prop) (cdr prop))))
            overlay))))))

(defun code-awareness--clear-buffer-highlights (buffer)
  "Clear all Code Awareness highlight from the given BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (overlay-get overlay 'code-awareness-type)
          (delete-overlay overlay))))
    ;; Remove from highlights hash table
    (remhash buffer code-awareness--highlights)
    ;; Also clear hl-line highlights if using that mode
    (code-awareness--clear-buffer-hl-line-highlights buffer)
    (code-awareness-log-info "Cleared highlights for buffer %s" buffer)))

(defun code-awareness--clear-all-highlights ()
  "Clear all Code Awareness highlight from all buffers."
  (dolist (buffer (buffer-list))
    (code-awareness--clear-buffer-highlights buffer))
  (clrhash code-awareness--highlights)
  ;; Also clear hl-line highlights if using that mode
  (dolist (buffer (buffer-list))
    (code-awareness--clear-buffer-hl-line-highlights buffer))
  (clrhash code-awareness--hl-line-overlays)
  (code-awareness-log-info "Cleared all highlights"))

(defun code-awareness--apply-highlights-from-data (buffer highlight-data)
  "Apply highlight to BUFFER based on data from the local service.
Argument HIGHLIGHT-DATA the array of lines to highlight."
  (when (and buffer (buffer-live-p buffer) highlight-data)
    ;; Use hl-line mode if configured, otherwise use custom overlays
    (code-awareness--apply-hl-line-highlights-from-data buffer highlight-data)))

(defun code-awareness--convert-hl-to-highlights (hl-data)
  "Convert hl data structure to highlight format.
HL-DATA should be an array
of line numbers. Returns a list of highlight alists with \\='line and \\='type keys."
  (let ((highlights '()))
    ;; Handle both lists and vectors (JSON arrays are parsed as vectors)
    (when (and (or (listp hl-data) (vectorp hl-data))
               (> (length hl-data) 0))
      (dolist (line-number (if (vectorp hl-data)
                               (append hl-data nil)
                             hl-data))
        (when (numberp line-number)
          ;; Convert 0-based line numbers to 1-based (Emacs convention)
          (let ((emacs-line (1+ line-number)))
            (push `((line . ,emacs-line)
                    (type . modified)
                    (properties . ((source . hl))))
                  highlights)))))
    highlights))

;;; HL-Line Integration Functions

(defun code-awareness--init-hl-line-faces ()
  "Initialize hl-line faces for different highlight types."
  (when (featurep 'hl-line)
    (setq code-awareness--hl-line-faces
          `((conflict . ,(make-face 'code-awareness-hl-line-conflict))
            (overlap . ,(make-face 'code-awareness-hl-line-overlap))
            (peer . ,(make-face 'code-awareness-hl-line-peer))
            (modified . ,(make-face 'code-awareness-hl-line-modified))))
    ;; Set face properties based on theme
    (let ((conflict-face (alist-get 'conflict code-awareness--hl-line-faces))
          (overlap-face (alist-get 'overlap code-awareness--hl-line-faces))
          (peer-face (alist-get 'peer code-awareness--hl-line-faces))
          (modified-face (alist-get 'modified code-awareness--hl-line-faces)))
      (if (eq (frame-parameter nil 'background-mode) 'dark)
          ;; Dark theme colors - more prominent
          (progn
            (set-face-attribute conflict-face nil :background "#ff0000" :foreground "#ffffff" :extend t)
            (set-face-attribute overlap-face nil :background "#ff8800" :foreground "#ffffff" :extend t)
            (set-face-attribute peer-face nil :background "#0088ff" :foreground "#ffffff" :extend t)
            (set-face-attribute modified-face nil :background "#13547f" :foreground "#ffffff" :extend t))
        ;; Light theme colors - more prominent
        (progn
          (set-face-attribute conflict-face nil :background "#ffcccc" :foreground "#cc0000" :extend t)
          (set-face-attribute overlap-face nil :background "#ffdd88" :foreground "#884400" :extend t)
          (set-face-attribute peer-face nil :background "#88ccff" :foreground "#004488" :extend t)
          (set-face-attribute modified-face nil :background "#a0e1a4" :foreground "#004400" :extend t))))
    (code-awareness-log-info "HL-line faces initialized")))

(defun code-awareness--get-hl-line-face (type)
  "Get the hl-line face for the given highlight TYPE."
  (or (alist-get type code-awareness--hl-line-faces)
      ;; Fallback to default hl-line face if not found
      'hl-line))

(defun code-awareness--add-hl-line-highlight (buffer line-number type &optional properties)
  "Add a highlight using hl-line mode to the specified line in the given BUFFER.
Argument LINE-NUMBER the line number to highlight.
Argument TYPE the type of highlight to use (one of the hl-lines faces).
Optional argument PROPERTIES optional properties for hl-lines overlay."
  (when (and buffer line-number type (featurep 'hl-line))
    ;; Ensure hl-line faces are initialized
    (unless code-awareness--hl-line-faces
      (code-awareness--init-hl-line-faces))
    (let* ((face (code-awareness--get-hl-line-face type))
           ;; Use save-excursion to get absolute line positions regardless of cursor position
           (overlay (with-current-buffer buffer
                      (make-overlay (save-excursion
                                      ;; Suppress warning: goto-line is needed for absolute positioning
                                      (with-suppressed-warnings ((interactive-only goto-line))
                                        (goto-line line-number))
                                      (line-beginning-position))
                                    (save-excursion
                                      ;; Suppress warning: goto-line is needed for absolute positioning
                                      (with-suppressed-warnings ((interactive-only goto-line))
                                        (goto-line (1+ line-number)))
                                      (line-beginning-position))
                                    buffer t nil))))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'code-awareness-type 'hl-line-highlight)
      (overlay-put overlay 'code-awareness-line line-number)
      (overlay-put overlay 'code-awareness-highlight-type type)
      (overlay-put overlay 'code-awareness-properties properties)
      ;; Store highlight information
      (let ((buffer-highlights (gethash buffer code-awareness--hl-line-overlays)))
        (unless buffer-highlights
          (setq buffer-highlights (make-hash-table :test 'equal))
          (puthash buffer buffer-highlights code-awareness--hl-line-overlays))
        (puthash line-number overlay buffer-highlights))
      overlay)))

(defun code-awareness--clear-buffer-hl-line-highlights (buffer)
  "Clear all Code Awareness hl-line highlight from the given BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get overlay 'code-awareness-type) 'hl-line-highlight)
          (delete-overlay overlay))))
    ;; Remove from highlights hash table
    (remhash buffer code-awareness--hl-line-overlays)))

(defun code-awareness--apply-hl-line-highlights-from-data (buffer highlight-data)
  "Apply hl-line highlight to BUFFER based on data from the local service.
Argument HIGHLIGHT-DATA the array of lines to highlight."
  (when (and buffer (buffer-live-p buffer) highlight-data (featurep 'hl-line))
    ;; Clear existing highlights first
    (code-awareness--clear-buffer-hl-line-highlights buffer)
    ;; Apply new highlights
    (dolist (highlight highlight-data)
      (let ((line (alist-get 'line highlight))
            (type (alist-get 'type highlight))
            (properties (alist-get 'properties highlight)))
        (when (and line type)
          (run-with-timer 1.0 nil
                          (lambda ()
                            (code-awareness--add-hl-line-highlight buffer line type properties))))))))

;;; IPC Communication

(defun code-awareness--generate-guid ()
  "Generate a unique GUID for this Emacs instance."
  (concat (number-to-string (emacs-pid)) "-" (number-to-string (random 1000000))))

(defun code-awareness--get-socket-path (guid)
  "Get the socket path for the given GUID."
  (if (eq system-type 'windows-nt)
      (format "\\\\.\\pipe\\caw.%s" guid)
    (format "%s/caw.%s" (directory-file-name (temporary-file-directory)) guid)))

(defun code-awareness--get-catalog-socket-path ()
  "Get the catalog socket path."
  (code-awareness--get-socket-path code-awareness-catalog))

(defun code-awareness--ipc-sentinel (_process event)
  "Handle IPC process sentinel EVENTs."
  (code-awareness-log-info "Code Awareness IPC: %s" event)
  (cond
   ((string-match "failed" event)
    (code-awareness-log-error "Code Awareness IPC connection failed")
    (setq code-awareness--connected nil)
    ;; Retry connection
    (run-with-timer 2.0 nil #'code-awareness--connect-to-local-service))
   ((string-match "exited" event)
    (code-awareness-log-warn "Code Awareness IPC connection closed")
    (setq code-awareness--connected nil))
   ((string-match "connection broken by remote peer" event)
    (code-awareness-log-warn "Code Awareness IPC rejected connection")
    (setq code-awareness--connected nil)
    ;; Retry connection after a delay
    (run-with-timer 2.0 nil #'code-awareness--connect-to-local-service))
   ((string-match "open" event)
    (code-awareness-log-info "Successfully connected to Code Awareness IPC")
    (setq code-awareness--connected t)
    ;; Initialize workspace after connection (like VS Code)
    (code-awareness--init-workspace))
   (t
    (code-awareness-log-warn "Unknown IPC sentinel event: %s" event))))

(defun code-awareness--ipc-filter (process data)
  "Handle IPC PROCESS DATA."
  (let ((buffer (process-buffer process)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert data)
        (code-awareness--process-ipc-messages)))))

(defun code-awareness--process-ipc-messages ()
  "Process complete IPC messages from the buffer."
  (let ((delimiter "\f"))
    (goto-char (point-min))
    (while (search-forward delimiter nil t)
      (let* ((end-pos (point))
             (start-pos (point-min))
             (message (buffer-substring-no-properties start-pos (1- end-pos))))
        (delete-region start-pos end-pos)
        (code-awareness--handle-ipc-message message)))))

(defun code-awareness--handle-ipc-message (message)
  "Handle a single IPC MESSAGE."
  (condition-case err
      (let* ((data (json-read-from-string message))
             (flow (alist-get 'flow data))
             (domain (alist-get 'domain data))
             (action (alist-get 'action data))
             (response-data (alist-get 'data data))
             (error-data (alist-get 'err data)))
        (code-awareness-log-info "%s:%s" domain action)
        (if (and (string= flow "res") action)
            (code-awareness--handle-response domain action response-data)
          (if (and (string= flow "err") action)
              (code-awareness--handle-error domain action error-data)
            (if (and (string= flow "req") action)
                (progn
                  (code-awareness-log-info "Received request: %s:%s" domain action)
                  ;; Handle events using the events table
                  (let ((event-key (format "%s:%s" domain action))
                        (handler (gethash action code-awareness--events-table)))
                    (if handler
                        (funcall handler response-data)
                      (code-awareness-log-info "No handler for event: %s" event-key))))
              (code-awareness-log-warn "Unknown message format: %s" message)))))
    (error
     (code-awareness-log-error "Error parsing IPC message: %s" err))))

(defun code-awareness--handle-response (domain action data)
  "Handle an IPC response.
Argument DOMAIN string describing the event domain, e.g. code, auth, etc.
Argument ACTION string describing the event action, e.g. auth:info.
Argument DATA additional data received from Code Awareness (JSON)."
  (let* ((key (format "res:%s:%s" domain action))
         (handler (gethash key code-awareness--response-handlers)))
    ;; Handle broadcast responses automatically (they may come from external sources)
    (cond
     ;; Auth responses from broadcast
     ((and (string= domain "*") (or (string= action "auth:info") (string= action "auth:login")))
      (code-awareness--handle-auth-info-response data))
     ;; Open peer file response from local service broadcast
     ((and (string= domain "code") (string= action "repo:open-peer-file"))
      (code-awareness--handle-open-peer-file-response data))
     ;; Branch diff response from local service broadcast
     ((and (string= domain "code") (string= action "repo:branch:select"))
      (code-awareness--handle-branch-diff-response data))
     ;; Other responses with registered handlers
     (handler
      (remhash key code-awareness--response-handlers)
      (funcall handler data)))))

(defun code-awareness--handle-repo-active-path-response (data &optional expected-file-path)
  "Handle response from repo:active-path request.
EXPECTED-FILE-PATH is the
file path that was originally requested (for validation).
Argument DATA the data received from Code Awareness application."
  (code-awareness-log-info "Received repo:active-path response")
  ;; Add the project to our store
  (code-awareness--add-project data)
  ;; Extract and apply highlights from the hl data structure
  (let* ((hl-data (alist-get 'hl data))
         (buffer code-awareness--active-buffer))
    ;; Debug logging for highlight data
    (code-awareness-log-info "hl-data received: %s" (prin1-to-string hl-data))
    (code-awareness-log-info "hl-data type: %s, length: %s"
                            (type-of hl-data)
                            (if hl-data (length hl-data) "nil"))
    (if (and hl-data buffer (buffer-live-p buffer))
        ;; Validate that the buffer still corresponds to the expected file
        (let ((current-file-path (buffer-file-name buffer)))
          (if (and expected-file-path current-file-path
                   (string= (code-awareness--cross-platform-path expected-file-path)
                            (code-awareness--cross-platform-path current-file-path)))
              ;; File paths match, apply highlights
              (progn
                ;; Convert hl data to highlight format
                (let ((highlights (code-awareness--convert-hl-to-highlights hl-data)))
                  (code-awareness-log-info "Number of highlights: %s" (length highlights))
                  (when highlights
                    (code-awareness--apply-highlights-from-data buffer highlights)))))))))

(defun code-awareness--handle-auth-info-response (data)
  "Handle response from auth:info request.
Argument DATA the data received from Code Awareness application."
  (if (and data (listp data) (alist-get 'user data))
      (progn
        (setq code-awareness--user (alist-get 'user data))
        (setq code-awareness--tokens (alist-get 'tokens data))
        (setq code-awareness--authenticated t)
        (code-awareness-log-info "Authentication successful")
        (message "Authenticated as %s" (alist-get 'name code-awareness--user))
        ;; Refresh active file immediately after authentication (like VSCode's init function)
        (code-awareness--refresh-active-file))
    (setq code-awareness--authenticated nil)
    (code-awareness-log-warn "No authentication data received - user needs to authenticate")))

(defun code-awareness--handle-peer-select (peer-data)
  "Handle peer selection event from Muninn app.
Argument PEER-DATA the data received from Code Awareness (peer info)."
  (code-awareness-log-info "Peer selected: %s" (alist-get 'name peer-data))
  (setq code-awareness--selected-peer peer-data)

  ;; Get active project information
  (let* ((active-project code-awareness--active-project)
         (origin (alist-get 'origin active-project))
         (fpath (alist-get 'activePath active-project)))
    (if (not fpath)
        (code-awareness-log-warn "No active file path for peer diff")
      ;; Send request for peer diff
      (let ((message-data `((origin . ,origin)
                            (fpath . ,fpath)
                            (caw . ,code-awareness--guid)
                            (peer . ,peer-data))))
        (code-awareness-log-info "Requesting peer diff for %s" fpath)
        (code-awareness--transmit "repo:diff-peer" message-data)
        (code-awareness--setup-response-handler "code" "repo:diff-peer")))))

(defun code-awareness--handle-peer-unselect ()
  "Handle peer unselection event from Muninn app."
  (code-awareness-log-info "Peer unselected")
  (setq code-awareness--selected-peer nil)
  ;; Close any open diff buffers
  (code-awareness--close-diff-buffers))

(defun code-awareness--handle-peer-diff-response (data)
  "Handle response from repo:diff-peer request.
Argument DATA the data received from Code Awareness (peer file info)."
  (code-awareness-log-info "Received peer diff response")
  (let* ((peer-file (alist-get 'peerFile data))
         (title (alist-get 'title data))
         (active-project code-awareness--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (user-file (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (and peer-file user-file)
        (progn
          (code-awareness-log-info "Opening diff: %s vs %s" peer-file user-file)
          (code-awareness--open-diff-view peer-file user-file title))
      (code-awareness-log-error "Missing file paths for diff: peer-file=%s, user-file=%s"
                               peer-file user-file))))

(defun code-awareness--handle-open-peer-file-response (data)
  "Handle response from repo:open-peer-file request.
The local service has downloaded/extracted the file and provides the full path.
Argument DATA the data received from Code Awareness local service."
  (code-awareness-log-info "Received open-peer-file response")
  (let* ((file-path (alist-get 'filePath data))
         (file-paths (alist-get 'filePaths data))
         (exists (alist-get 'exists data)))
    (cond
     ;; Case 1: Single file path provided
     (file-path
      (code-awareness-log-info "Opening peer file: %s (exists locally: %s)" file-path exists)
      (if (file-exists-p file-path)
          (progn
            ;; Open the file in a new buffer
            (let ((buffer (find-file-noselect file-path)))
              (unless exists
                ;; If it's a downloaded peer file (not local), make it read-only
                (with-current-buffer buffer
                  (setq-local buffer-read-only t)
                  (setq-local header-line-format "Peer file (read-only)")))
              ;; Display the buffer
              (switch-to-buffer buffer)
              (message "Opened peer file: %s" (file-name-nondirectory file-path))))
        (code-awareness-log-error "File does not exist: %s" file-path)
        (message "Error: File does not exist: %s" file-path)))

     ;; Case 2: Two file paths provided (diff mode)
     ((and file-paths (vectorp file-paths) (>= (length file-paths) 2))
      (let ((file1 (aref file-paths 0))
            (file2 (aref file-paths 1)))
        (code-awareness-log-info "Opening diff between: %s and %s" file1 file2)
        (if (and (file-exists-p file1) (file-exists-p file2))
            (progn
              ;; Open diff view using ediff or fallback to diff-mode
              (code-awareness--open-diff-view file1 file2 "Peer File Comparison"))
          (code-awareness-log-error "One or both files do not exist: %s, %s" file1 file2)
          (message "Error: One or both files do not exist"))))

     ;; Case 3: No valid data
     (t
      (code-awareness-log-error "Invalid repo:open-peer-file response: %s" data)
      (message "Error: Invalid file path data in repo:open-peer-file response")))))

(defun code-awareness--open-diff-view (peer-file user-file title)
  "Open a diff view comparing peer file with user file.
Argument PEER-FILE the path of the peer file that was extracted (in tmp folder).
Argument USER-FILE the path of the existing file in the buffer.
Argument TITLE title of the diff buffer."
  ;; Close any existing EDiff session first
  (when (and (boundp 'ediff-control-buffer) ediff-control-buffer
             (buffer-live-p ediff-control-buffer))
    (code-awareness-log-info "Closing existing EDiff session")
    (with-current-buffer ediff-control-buffer
      (ediff-quit t)))

  (let* ((peer-buffer (find-file-noselect peer-file))
         (user-buffer (find-file-noselect user-file)))
    ;; Configure peer buffer to auto-revert without prompting
    (with-current-buffer peer-buffer
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert-file-contents peer-file nil nil nil t))))
      (setq-local buffer-read-only t))

    ;; Use ediff for a better diff experience if available
    (if (fboundp 'ediff-buffers)
        (progn
          (code-awareness-log-info "Using ediff for diff view")
          (ediff-buffers peer-buffer user-buffer))
      ;; Fallback to diff-mode in a separate buffer
      (let ((diff-buffer-name (format "*CodeAwareness Diff: %s*" title)))
        (let ((diff-buffer (get-buffer-create diff-buffer-name)))
          (with-current-buffer diff-buffer
            ;; Clear the buffer
            (erase-buffer)
            ;; Insert diff content
            (let ((diff-output (code-awareness--generate-diff peer-file user-file)))
              (insert diff-output)
              ;; Set up the buffer for diff viewing
              (diff-mode)
              ;; Make the buffer read-only
              (setq buffer-read-only t)
              ;; Display the buffer
              (switch-to-buffer diff-buffer)
              (message "Opened diff view: %s" title))))))))

(defun code-awareness--generate-diff (file1 file2)
  "Generate diff output between two files.
Argument FILE1 the first file in the diff command.
Argument FILE2 the second file in the diff command."
  (let ((diff-command (format "diff -u %s %s" file1 file2)))
    (with-temp-buffer
      (let ((exit-code (call-process-shell-command diff-command nil t)))
        (if (= exit-code 0)
            "Files are identical"
          (buffer-string))))))

(defun code-awareness--close-diff-buffers ()
  "Close all CodeAwareness diff buffers."
  (dolist (buffer (buffer-list))
    (when (and (string-match "\\*CodeAwareness Diff:" (buffer-name buffer))
               (buffer-live-p buffer))
      (kill-buffer buffer)))
  (message "Closed CodeAwareness diff buffers"))

;;; Additional Event Handlers

(defun code-awareness--handle-branch-select (data)
  "Handle BRANCH selection event.
Argument DATA the data received from the branch:select event."
  (let ((branch-name (alist-get 'branch data)))
    (code-awareness-log-info "Branch selected: %s" branch-name)
    (when branch-name
      (let ((message-data `((branch . ,branch-name)
                            (caw . ,code-awareness--guid))))
        (code-awareness--transmit "repo:branch:select" message-data)
        (code-awareness--setup-response-handler "code" "repo:branch:select")))))

(defun code-awareness--handle-branch-unselect ()
  "Handle branch unselection event."
  (code-awareness-log-info "Branch unselected")
  (code-awareness--close-diff-buffers))

(defun code-awareness--handle-branch-refresh (_data)
  "Handle branch refresh event."
  (code-awareness-log-info "Branch refresh requested")
  ;; TODO: Implement branch refresh using git and display in panel
  (message "Branch refresh not yet implemented"))

(defun code-awareness--handle-auth-logout ()
  "Handle auth logout event."
  (code-awareness-log-info "Auth logout requested")
  (code-awareness--clear-store)
  (code-awareness--clear-all-highlights)
  (message "Logged out"))

(defun code-awareness--handle-context-add (context)
  "Handle CONTEXT add event. TODO: work in progress."
  (code-awareness-log-info "Context add requested: %s" context)
  (let* ((active-project code-awareness--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (full-path (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (not full-path)
        (code-awareness-log-warn "No active file path for context add")
      (let ((message-data `((fpath . ,full-path)
                            (selections . ,code-awareness--active-selections)
                            (context . ,context)
                            (op . "add")
                            (caw . ,code-awareness--guid))))
        (code-awareness--transmit "context:apply" message-data)
        (code-awareness--setup-response-handler "code" "context:apply")))))

(defun code-awareness--handle-context-del (context)
  "Handle CONTEXT delete event."
  (code-awareness-log-info "Context delete requested: %s" context)
  (let* ((active-project code-awareness--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (full-path (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (not full-path)
        (code-awareness-log-warn "No active file path for context delete")
      (let ((message-data `((fpath . ,full-path)
                            (selections . ,code-awareness--active-selections)
                            (context . ,context)
                            (op . "del")
                            (caw . ,code-awareness--guid))))
        (code-awareness--transmit "context:apply" message-data)
        (code-awareness--setup-response-handler "code" "context:apply")))))

(defun code-awareness--handle-context-open-rel (data)
  "Handle context open relative event.
Argument DATA the data received from Code Awareness application."
  (code-awareness-log-info "Context open relative requested: %s" (alist-get 'sourceFile data))
  (let ((source-file (alist-get 'sourceFile data)))
    (when source-file
      (find-file source-file))))


;;; Response Handlers

(defun code-awareness--handle-branch-diff-response (data)
  "Handle response from repo:branch:select request.
Argument DATA the data received from Code Awareness application."
  (code-awareness-log-info "Received branch diff response")
  (let* ((peer-file (alist-get 'peerFile data))
         (user-file (alist-get 'userFile data))
         (title (alist-get 'title data)))
    (if (and peer-file user-file)
        (progn
          (code-awareness-log-info "Opening branch diff: %s vs %s" peer-file user-file)
          (code-awareness--open-diff-view peer-file user-file title))
      (code-awareness-log-error "Missing file paths for branch diff: peer-file=%s, user-file=%s"
                               peer-file user-file))))

(defun code-awareness--handle-context-apply-response (_data)
  "Handle response from context:apply request."
  (code-awareness-log-info "Received context apply response")
  ;; TODO: Handle context update response
  (message "Context applied successfully"))

(defun code-awareness--handle-error (domain action error-data)
  "Handle an IPC error.
Argument DOMAIN the request domain, e.g. auth, code, etc.
Argument ACTION the request action, e.g. auth:info.
Argument ERROR-DATA incoming error message."
  (let* ((key (format "err:%s:%s" domain action))
         (handler (gethash key code-awareness--response-handlers)))
    (when handler
      (remhash key code-awareness--response-handlers)
      (funcall handler error-data))))

(defun code-awareness--transmit (action data)
  "Transmit a message to the Code Awareness IPC.
Argument ACTION the action to send to Code Awareness app,
e.g. repo:active-path, auth:info, etc.
Argument DATA data to send to Code Awareness application."
  (let* ((domain (if (member action '("auth:info" "auth:login")) "*" "code"))
         (flow "req")
         (message (json-encode `((flow . ,flow)
                                 (domain . ,domain)
                                 (action . ,action)
                                 (data . ,data)
                                 (caw . ,code-awareness--guid)))))
    (if code-awareness--ipc-process
        (if (eq (process-status code-awareness--ipc-process) 'open)
            (progn
              (code-awareness-log-info "Sending %s:%s" domain action)
              (process-send-string code-awareness--ipc-process (concat message "\f"))
              (code-awareness--setup-response-handler domain action))
          (code-awareness-log-error "IPC process exists but is not open (status: %s)"
                                   (process-status code-awareness--ipc-process)))
      (code-awareness-log-error "No IPC process available for transmission"))))

(defun code-awareness--setup-response-handler (domain action &optional file-path)
  "Setup response handlers for the given DOMAIN and ACTION.
FILE-PATH is the file path associated with this request (for validation)."
  (let ((res-key (format "res:%s:%s" domain action))
        (err-key (format "err:%s:%s" domain action)))
    ;; Set up specific handlers for known actions
    (cond
     ((string= (format "%s:%s" domain action) "code:repo:active-path")
      (puthash res-key (lambda (data) (code-awareness--handle-repo-active-path-response data file-path)) code-awareness--response-handlers))
     ((string= (format "%s:%s" domain action) "code:repo:diff-peer")
      (puthash res-key #'code-awareness--handle-peer-diff-response code-awareness--response-handlers))
     ((string= (format "%s:%s" domain action) "code:repo:branch:select")
      (puthash res-key #'code-awareness--handle-branch-diff-response code-awareness--response-handlers))
     ((string= (format "%s:%s" domain action) "code:repo:get-tmp-dir")
      (puthash res-key #'code-awareness--handle-get-tmp-dir-response code-awareness--response-handlers))
     ((string= (format "%s:%s" domain action) "code:context:apply")
      (puthash res-key #'code-awareness--handle-context-apply-response code-awareness--response-handlers))
     ((or (string= (format "%s:%s" domain action) "*:auth:info")
          (string= (format "%s:%s" domain action) "*:auth:login"))
      (puthash res-key #'code-awareness--handle-auth-info-response code-awareness--response-handlers))
     (t
      (puthash res-key #'code-awareness--handle-success code-awareness--response-handlers)))
    (puthash err-key #'code-awareness--handle-failure code-awareness--response-handlers)))

(defun code-awareness--handle-success (data)
  "Handle successful IPC response.
Argument DATA data received from the request."
  (code-awareness-log-info "Success - %s" (format "%s" data)))

(defun code-awareness--handle-failure (error-data)
  "Handle failed IPC response for unknown actions.
Argument ERROR-DATA error message received from the request."
  (code-awareness-log-error "Error handle for unknown action - %s" (format "%s" error-data)))

;;; Connection Management

(defun code-awareness--init-ipc ()
  "Initialize IPC communication."
  (setq code-awareness--guid (code-awareness--generate-guid))
  (code-awareness-log-info "Initializing IPC with GUID %s" (format "%s" code-awareness--guid))
  (code-awareness--connect-to-catalog))

(defun code-awareness--connect-to-catalog ()
  "Connect to the catalog service."
  (let* ((catalog-path (code-awareness--get-catalog-socket-path))
         (process-name "code-awareness-catalog")
         (buffer-name "*code-awareness-catalog*"))
    (code-awareness-log-info "Connecting to catalog")
    (condition-case err
        (progn
          (setq code-awareness--ipc-catalog-process
                (make-network-process
                 :name process-name
                 :buffer buffer-name
                 :family 'local
                 :service catalog-path
                 :sentinel #'code-awareness--catalog-sentinel
                 :filter #'code-awareness--catalog-filter
                 :noquery t))
          (code-awareness-log-info "Catalog connection initiated")
          ;; Check process status immediately and after a delay
          (code-awareness--check-catalog-process-status)
          (run-with-timer 0.5 nil #'code-awareness--check-catalog-process-status))
      (error
       (code-awareness-log-error "Failed to create catalog connection: %s" err)
       (message "Failed to connect to catalog service at %s. Error: %s"
                catalog-path err)))))

(defun code-awareness--catalog-sentinel (_process event)
  "Handle catalog process sentinel EVENTs."
  (cond
   ((string-match "failed" event)
    (code-awareness-log-error "Failed to connect to catalog service at %s"
                             (code-awareness--get-catalog-socket-path))
    (message "Failed to connect to catalog service. Check if the service is running on %s"
             (code-awareness--get-catalog-socket-path))
    (setq code-awareness--connected nil))
   ((string-match "exited" event)
    (code-awareness-log-warn "Catalog connection closed")
    (setq code-awareness--connected nil))
   ((string-match "open" event)
    (code-awareness-log-info "Successfully connected to catalog service")
    (message "Connected to catalog service")
    (setq code-awareness--connected t)
    ;; Send 'connected' message to trigger client registration (matching VSCode behavior)
    (code-awareness--catalog-filter "connected"))))

(defun code-awareness--catalog-filter (data)
  "Handle catalog process DATA."
  (when (string= data "connected")
    (code-awareness--register-client)))

(defun code-awareness--register-client ()
  "Register this client with the catalog service."
  (unless code-awareness--client-registered
    (let ((message (json-encode `((flow . "req")
                                  (domain . "*")
                                  (action . "clientId")
                                  (data . ,code-awareness--guid)
                                  (caw . ,code-awareness--guid)))))
      (when code-awareness--ipc-catalog-process
        (process-send-string code-awareness--ipc-catalog-process (concat message "\f"))
        (code-awareness-log-info "Client registered")
        (setq code-awareness--client-registered t)
        (code-awareness--init-server))))
  (code-awareness-log-info "Client already registered, skipping"))

(defun code-awareness--init-server ()
  "Initialize the server connection."
  (code-awareness-log-info "Initializing server poll connection")
  ;; Start polling for local service socket with exponential backoff
  (code-awareness--poll-for-local-service))

(defun code-awareness--init-workspace ()
  "Initialize workspace."
  (code-awareness-log-info "Workspace initialized")
  ;; Request temp directory from local service
  (code-awareness--request-tmp-dir)
  ;; Send auth:info request after a short delay to ensure connection is ready
  (run-with-timer 0.1 nil #'code-awareness--send-auth-info))

(defun code-awareness--request-tmp-dir ()
  "Request temp directory from the Code Awareness local service."
  (code-awareness-log-info "Requesting temp directory from local service")
  (if (and code-awareness--ipc-process
           (eq (process-status code-awareness--ipc-process) 'open))
      (progn
        (code-awareness--transmit "repo:get-tmp-dir" code-awareness--guid)
        (code-awareness--setup-response-handler "code" "repo:get-tmp-dir"))
    (code-awareness-log-error "IPC process not ready for get-tmp-dir request")))

(defun code-awareness--handle-get-tmp-dir-response (data)
  "Handle response from repo:get-tmp-dir request.
Argument DATA the data received from Code Awareness application."
  (let ((tmp-dir (alist-get 'tmpDir data)))
    (when tmp-dir
      (setq code-awareness--tmp-dir tmp-dir)
      (code-awareness-log-info "Received temp directory from local service: %s" tmp-dir)
      ;; Update LSP blocklist with the actual temp directory
      (when (and (featurep 'lsp-mode) (boundp 'lsp-session-folders-blocklist))
        (add-to-list 'lsp-session-folders-blocklist tmp-dir)
        (code-awareness-log-info "Added actual temp directory to LSP blocklist: %s" tmp-dir))
      ;; Update recentf exclude with the actual temp directory
      (when (and (featurep 'recentf) (boundp 'recentf-exclude))
        (add-to-list 'recentf-exclude
                     (concat "^" (regexp-quote (expand-file-name tmp-dir))))
        (code-awareness-log-info "Added actual temp directory to recentf-exclude: %s" tmp-dir)))))

(defun code-awareness--send-auth-info ()
  "Send auth:info request to the Code Awareness IPC."
  (code-awareness-log-info "Sending auth:info request")
  (if (and code-awareness--ipc-process
           (eq (process-status code-awareness--ipc-process) 'open))
      (code-awareness--transmit "auth:info" nil)
    (code-awareness-log-error "IPC process not ready for auth:info request")))

(defvar code-awareness--max-poll-attempts 10
  "Maximum number of polling attempts.")

(defun code-awareness--poll-for-local-service ()
  "Poll for Code Awareness IPC socket with exponential backoff."
  (let ((socket-path (code-awareness--get-socket-path code-awareness--guid)))
    (if (file-exists-p socket-path)
        (progn
          (setq code-awareness--poll-attempts 0)
          (code-awareness--connect-to-local-service))
      (if (>= code-awareness--poll-attempts code-awareness--max-poll-attempts)
          (progn
            (code-awareness-log-error "Failed to find Code Awareness IPC socket after %d attempts"
                                     code-awareness--max-poll-attempts)
            (message "Failed to connect to Code Awareness IPC after %d attempts"
                     code-awareness--max-poll-attempts))
        (setq code-awareness--poll-attempts (1+ code-awareness--poll-attempts))
        ;; Exponential backoff: 0.5s, 1s, 2s, 4s, 8s, etc.
        (let ((delay (expt 2 (1- code-awareness--poll-attempts))))
          (run-with-timer delay nil #'code-awareness--poll-for-local-service))))))

(defun code-awareness--connect-to-local-service ()
  "Connect to the Code Awareness IPC with retry logic."
  (let* ((socket-path (code-awareness--get-socket-path code-awareness--guid))
         (process-name (format "code-awareness-ipc-%s" code-awareness--guid))
         (buffer-name (format "*%s*" process-name)))

    (condition-case err
        (progn
          (setq code-awareness--ipc-process
                (make-network-process
                 :name process-name
                 :buffer buffer-name
                 :family 'local
                 :service socket-path
                 :sentinel #'code-awareness--ipc-sentinel
                 :filter #'code-awareness--ipc-filter
                 :noquery t))
          (code-awareness-log-info "Connected to Code Awareness IPC")
          ;; Set up a timeout to detect stuck connections
          (run-with-timer 5.0 nil #'code-awareness--check-connection-timeout)
          ;; Set up a fallback to trigger workspace init if sentinel doesn't fire
          (run-with-timer 1.0 nil #'code-awareness--fallback-workspace-init))
      (error
       (code-awareness-log-warn "Failed to connect to Code Awareness IPC, will retry in 5 seconds")
       (code-awareness-log-warn "Error: %s" err)
       ;; Schedule retry
       (run-with-timer 5.0 nil #'code-awareness--connect-to-local-service)))))

(defun code-awareness--check-connection-timeout ()
  "Check if the connection is stuck and handle timeout."
  (when (and code-awareness--ipc-process
             (not code-awareness--connected))
    (let ((status (process-status code-awareness--ipc-process)))
      (if (eq status 'connect)
          (progn
            (code-awareness-log-error "Connection stuck, retrying")
            (delete-process code-awareness--ipc-process)
            (setq code-awareness--ipc-process nil)
            (run-with-timer 1.0 nil #'code-awareness--connect-to-local-service))))))

(defun code-awareness--fallback-workspace-init ()
  "Fallback workspace initialization if sentinel doesn't fire."
  (when (and code-awareness--ipc-process
             (eq (process-status code-awareness--ipc-process) 'open)
             (not code-awareness--connected))
    (code-awareness-log-info "Using fallback workspace init")
    (code-awareness--init-workspace)))

(defun code-awareness--force-cleanup ()
  "Force cleanup of all Code Awareness processes and state."
  (code-awareness-log-info "Force cleaning up all processes")

  ;; Cancel any pending timers
  (when code-awareness--update-timer
    (cancel-timer code-awareness--update-timer)
    (setq code-awareness--update-timer nil))
  (when code-awareness--highlight-timer
    (cancel-timer code-awareness--highlight-timer)
    (setq code-awareness--highlight-timer nil))

  ;; Force delete processes regardless of status
  (when code-awareness--ipc-catalog-process
    (condition-case err
        (progn
          ;; Try to close the process gracefully first
          (when (eq (process-status code-awareness--ipc-catalog-process) 'open)
            (process-send-eof code-awareness--ipc-catalog-process))
          ;; Force delete the process
          (delete-process code-awareness--ipc-catalog-process)
          (code-awareness-log-info "Force deleted catalog process"))
      (error
       (code-awareness-log-error "Error deleting catalog process: %s" err)))
    (setq code-awareness--ipc-catalog-process nil))

  (when code-awareness--ipc-process
    (condition-case err
        (progn
          ;; Try to close the process gracefully first
          (when (eq (process-status code-awareness--ipc-process) 'open)
            (process-send-eof code-awareness--ipc-process))
          ;; Force delete the process
          (delete-process code-awareness--ipc-process)
          (code-awareness-log-info "Force deleted IPC process"))
      (error
       (code-awareness-log-error "Error deleting IPC process: %s" err)))
    (setq code-awareness--ipc-process nil))

  ;; Remove hooks
  (remove-hook 'after-save-hook #'code-awareness--after-save-hook)
  (remove-hook 'post-command-hook #'code-awareness--post-command-hook)

  ;; Reset all state
  (setq code-awareness--connected nil
        code-awareness--authenticated nil
        code-awareness--active-buffer nil
        code-awareness--active-project nil
        code-awareness--guid nil
        code-awareness--client-registered nil
        code-awareness--poll-attempts 0)

  (code-awareness-log-info "Force cleanup completed"))

(defun code-awareness--send-disconnect-messages ()
  "Send disconnect messages to catalog service."
  (code-awareness-log-info "Sending disconnect messages")

  ;; Send disconnect message to catalog
  (when (and code-awareness--ipc-catalog-process
             (eq (process-status code-awareness--ipc-catalog-process) 'open))
    (let ((message (json-encode `((flow . "req")
                                  (domain . "*")
                                  (action . "clientDisconnect")
                                  (data . ,code-awareness--guid)
                                  (caw . ,code-awareness--guid)))))
      (code-awareness-log-info "Sending clientDisconnect to catalog")
      (condition-case err
          (process-send-string code-awareness--ipc-catalog-process (concat message "\f"))
        (error
         (code-awareness-log-error "Failed to send clientDisconnect to catalog: %s" err))))))

(defun code-awareness--check-catalog-process-status ()
  "Check the status of the catalog process."
  (when code-awareness--ipc-catalog-process
    (let ((status (process-status code-awareness--ipc-catalog-process)))
      (if (eq status 'open)
          (progn
            (code-awareness-log-info "Catalog connected")
            ;; If client is not registered yet, trigger registration as fallback
            (unless code-awareness--client-registered
              (code-awareness--catalog-filter "connected")))
        (code-awareness-log-error "Catalog process not open, status: %s" status)))))

;;; Buffer Management

(defun code-awareness--schedule-update ()
  "Schedule a debounced update."
  (when code-awareness--update-timer
    (cancel-timer code-awareness--update-timer))
  (setq code-awareness--update-timer
        (run-with-timer code-awareness-update-delay nil #'code-awareness--update)))

(defun code-awareness--update ()
  "Update Code Awareness for the current buffer."
  (setq code-awareness--update-timer nil)
  (when (and code-awareness--active-buffer
             (buffer-live-p code-awareness--active-buffer)
             (buffer-file-name code-awareness--active-buffer))
    (let ((filename (buffer-file-name code-awareness--active-buffer)))
      ;; TODO: we're currently not handling the response -- line-diffs get refreshed only upon buffer switch
      (code-awareness--transmit "repo:file-saved"
                               `((fpath . ,filename)
                                 (doc . ,(buffer-string))
                                 (caw . ,code-awareness--guid))))))

;;; Hooks and Event Handling

(defun code-awareness--after-save-hook ()
  "Hook function for `after-save-hook'."
  (code-awareness--update))

(defun code-awareness--post-command-hook ()
  "Hook function for `post-command-hook'."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (not (eq current-buffer code-awareness--active-buffer)))
      (if (buffer-file-name current-buffer)
          ;; Only update active buffer if switching to a different file
          (let ((current-file (buffer-file-name current-buffer))
                (active-file (when code-awareness--active-buffer
                               (buffer-file-name code-awareness--active-buffer))))
            (if (and code-awareness--active-buffer active-file
                     (string= current-file active-file))
                ;; Same file, no need to update active buffer
                ;; Different file or no active buffer, update and refresh
                (progn
                  (setq code-awareness--active-buffer current-buffer)
                  (code-awareness--refresh-active-file))))
        ;; Don't clear active buffer when switching to non-file buffers
        ))))

;;; Public API

;;;###autoload
(defun code-awareness-refresh ()
  "Refresh Code Awareness data."
  (interactive)
  (code-awareness--refresh-active-file))

;;;###autoload
(defun code-awareness-clear-all-highlights ()
  "Clear all Code Awareness highlight from all buffers."
  (interactive)
  (code-awareness--clear-all-highlights)
  (message "Cleared all highlights"))

;;;###autoload
(defun code-awareness-auth-status ()
  "Show the current authentication status."
  (interactive)
  (if code-awareness--authenticated
      (message "Authenticated as %s" (alist-get 'name code-awareness--user))
    (message "Not authenticated")))

;;;###autoload
(defun code-awareness-connection-status ()
  "Show the current connection status."
  (interactive)
  (message "Catalog connected: %s, Local service connected: %s, Authenticated: %s, Client registered: %s"
           (if (and code-awareness--ipc-catalog-process
                    (eq (process-status code-awareness--ipc-catalog-process) 'open)) "yes" "no")
           (if (and code-awareness--ipc-process
                    (eq (process-status code-awareness--ipc-process) 'open)) "yes" "no")
           (if code-awareness--authenticated "yes" "no")
           (if code-awareness--client-registered "yes" "no")))

;;; Reinit on Emacs restart

;;;###autoload
(defun code-awareness-reinit-faces ()
  "Force reinitialize hl-line faces."
  (interactive)
  (when (featurep 'hl-line)
    (setq code-awareness--hl-line-faces nil) ; Clear existing faces
    (code-awareness--init-hl-line-faces)
    (message "HL-line faces reinitialized")))

;;; Minor Mode

;;;###autoload
(define-minor-mode code-awareness-mode
  "Toggle Code Awareness mode.
Enable Code Awareness functionality for collaborative development."
  :init-value nil
  :global t
  :lighter " CAW"
  :group 'code-awareness
  :require 'code-awareness
  (if code-awareness-mode
      (code-awareness--enable)
    (code-awareness--disable)))

(defun code-awareness--enable ()
  "Enable Code Awareness."
  (code-awareness--init-config)
  (code-awareness--init-store)
  (code-awareness--init-event-handlers)
  (code-awareness--init-ipc)
  (code-awareness--init-highlight-faces)
  (add-hook 'after-save-hook #'code-awareness--after-save-hook)
  (add-hook 'post-command-hook #'code-awareness--post-command-hook)
  (add-hook 'buffer-list-update-hook #'code-awareness--buffer-list-update-hook)
  (add-hook 'kill-emacs-hook #'code-awareness--cleanup-on-exit)
  ;; Set the current buffer as active if it has a file (like VSCode's activeTextEditor)
  (when (and (current-buffer) (buffer-file-name (current-buffer)))
    (setq code-awareness--active-buffer (current-buffer)))
  ;; Add temp directory to LSP blocklist to prevent LSP from treating temp files as projects
  (when (and (featurep 'lsp-mode) (boundp 'lsp-session-folders-blocklist))
    (add-to-list 'lsp-session-folders-blocklist code-awareness--tmp-dir)
    (code-awareness-log-info "Added temp directory to LSP blocklist: %s" code-awareness--tmp-dir))
  ;; Add temp directory to recentf exclude list to prevent temp files from appearing in recent files
  (when (and (featurep 'recentf) (boundp 'recentf-exclude))
    (add-to-list 'recentf-exclude
                 (concat "^" (regexp-quote (expand-file-name code-awareness--tmp-dir))))
    (code-awareness-log-info "Added temp directory to recentf-exclude: %s" code-awareness--tmp-dir))
  (code-awareness-log-info "Code Awareness enabled"))

(defun code-awareness--disable ()
  "Disable Code Awareness."
  (code-awareness-log-info "Disabling and disconnecting")

  ;; Remove hooks
  (remove-hook 'after-save-hook #'code-awareness--after-save-hook)
  (remove-hook 'post-command-hook #'code-awareness--post-command-hook)
  (remove-hook 'buffer-list-update-hook #'code-awareness--buffer-list-update-hook)
  (remove-hook 'kill-emacs-hook #'code-awareness--cleanup-on-exit)

  ;; Clear all highlights
  (code-awareness--clear-all-highlights)

  ;; Send disconnect messages before closing connections
  (code-awareness--send-disconnect-messages)

  ;; Use force cleanup to ensure all processes are properly deleted
  (code-awareness--force-cleanup)

  ;; Clear the store
  (code-awareness--clear-store)

  (code-awareness-log-info "Code Awareness disabled"))

;;; Cleanup on Emacs exit

(defun code-awareness--buffer-list-update-hook ()
  "Hook function to detect when buffers are displayed."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (buffer-file-name current-buffer)
               (not (eq current-buffer code-awareness--active-buffer)))
      (let ((current-file (buffer-file-name current-buffer))
            (active-file (when code-awareness--active-buffer
                           (buffer-file-name code-awareness--active-buffer))))
        (if (and code-awareness--active-buffer active-file
                 (string= current-file active-file))
            ;; Same file, no need to update active buffer
            ()
          ;; Different file or no active buffer, update and refresh
          (progn
            (setq code-awareness--active-buffer current-buffer)
            (code-awareness--refresh-active-file)))))))

(defun code-awareness--cleanup-on-exit ()
  "Cleanup Code Awareness when Emacs is about to exit."
  (when code-awareness-mode
    (code-awareness-log-info "Emacs exiting, disabling code-awareness-mode")
    ;; Disable the mode which will trigger proper cleanup including disconnect messages
    (code-awareness-mode -1)))

;; Note: kill-emacs-hook is added in code-awareness--enable, not at load time

;;; Provide

(provide 'code-awareness)
;;; code-awareness.el ends here
