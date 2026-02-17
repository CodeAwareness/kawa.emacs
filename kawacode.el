;;; kawacode.el --- Kawa Code collaboration package -*- lexical-binding: t -*-

;; Author: Mark Vasile <mark@codeawareness.com>
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience, vc
;; Homepage: https://github.com/codeawareness/kawa.emacs

;; Version: 2.0.0

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

;; This package is licensed under GPLv3. It depends on the Kawa Code
;; binary, available at https://codeawareness.com/product

;;; Commentary:

;; Emacs integration for Kawa Code (https://codeawareness.com/product),
;; a real-time collaboration platform for software teams.
;;
;; Kawa Code is a standalone desktop application that monitors your
;; working copy and communicates with team members via a cloud service.
;; This package connects Emacs to the Kawa Code app over a local socket,
;; enabling the following features directly in your editor:
;;
;; - Live diff highlights showing where your code intersects with
;;   teammates' uncommitted changes (early merge-conflict warning)
;; - One-click navigation between your version and a peer's version
;;   of the same file, without committing or pushing
;; - Per-file peer selection to compare against specific team members
;; - Intent-driven development (more details on Code Awareness website)
;; - Other possible features via Kawa Code extensions
;;
;; Requirements:
;;   - The Kawa Code desktop app must be installed and running.
;;     Download it from https://codeawareness.com/product
;;   - Emacs 27.1 or later

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'kawacode-pipe)
(require 'kawacode-list-pipe)
(require 'kawacode-process-sockets)
(require 'hl-line nil t)

;; Declare functions from ediff-util (optional dependency)
(declare-function ediff-buffers "ediff" (buffer-a buffer-b &optional startup-hooks job-name merge-buffer-file))
(declare-function ediff-quit "ediff-util" (reverse-default-keep-variants))

;;; Configuration

(defconst kawacode--caw-schema "caw"
  "Schema for Kawa Code URIs.")

(defconst kawacode--extract-repo-dir "extract"
  "Directory name for extracted repository files.")

;;;###autoload
(defgroup kawacode-config nil
  "Kawa Code configuration."
  :group 'kawacode
  :prefix "kawacode-")

;;;###autoload
(defcustom kawacode-highlight-intensity 0.3
  "Intensity of highlighting (0.0 to 1.0)."
  :type 'number
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-highlight-refresh-delay 0.5
  "Delay in seconds before refreshing highlights after changes."
  :type 'number
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-highlight-persistent nil
  "Whether highlights should persist across buffer switches."
  :type 'boolean
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-full-width-highlights t
  "Whether to use full-width highlights that extend to the end of the line."
  :type 'boolean
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-update-delay 0.5
  "Delay in seconds before running a Kawa Code update."
  :type 'number
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-selection-delay 0.15
  "Delay in seconds before sending cursor position to Muninn."
  :type 'number
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-debug nil
  "Enable debug mode for Kawa Code."
  :type 'boolean
  :group 'kawacode-config)

;;; Theme Support

;;;###autoload
(defcustom kawacode-change-color-light "#00b1a420"
  "Color for changed lines in light theme."
  :type 'string
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-change-color-dark "#03445f"
  "Color for changed lines in dark theme."
  :type 'string
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-peer-color-light "#ffdd34"
  "Color for peer code in light theme."
  :type 'string
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-peer-color-dark "#1f1cc2"
  "Color for peer code in dark theme."
  :type 'string
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-merge-color-light "#ffc000"
  "Color for merged code in light theme."
  :type 'string
  :group 'kawacode-config)

;;;###autoload
(defcustom kawacode-merge-color-dark "#141299"
  "Color for merged code in dark theme."
  :type 'string
  :group 'kawacode-config)

;;; Utility Functions

(defun kawacode--get-theme-color (light-color dark-color)
  "Get the appropriate color for the current theme.
Argument LIGHT-COLOR color for light theme.
Argument DARK-COLOR color for dark theme."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      dark-color
    light-color))

(defun kawacode--get-change-color ()
  "Get the color for changed lines."
  (kawacode--get-theme-color
   kawacode-change-color-light
   kawacode-change-color-dark))

(defun kawacode--get-peer-color ()
  "Get the color for peer code."
  (kawacode--get-theme-color
   kawacode-peer-color-light
   kawacode-peer-color-dark))

(defun kawacode--get-merge-color ()
  "Get the color for merged code."
  (kawacode--get-theme-color
   kawacode-merge-color-light
   kawacode-merge-color-dark))


;;; Customization

(defgroup kawacode nil
  "Kawa Code, low noise collaboration."
  :group 'applications
  :prefix "kawacode-")

;;; Internal Variables

(defvar kawacode--caw nil
  "Client ID assigned by Muninn via handshake.")

(defvar kawacode--ipc-process nil
  "IPC process for communicating with Muninn.")

(defvar kawacode--response-handlers (make-hash-table :test 'equal)
  "Hash table of response handlers for IPC requests.")

(defvar kawacode--pending-requests (make-hash-table :test 'equal)
  "Hash table mapping _msgId to callback for request-response matching.")

(defvar kawacode--active-project nil
  "Currently active project data.")

(defvar kawacode--active-buffer nil
  "Currently active buffer.")

(defvar kawacode--update-timer nil
  "Timer for debounced updates.")

(defvar kawacode--connected nil
  "Whether we're connected to the Kawa Code IPC.")

(defvar kawacode--config nil
  "Configuration data.")

(defvar kawacode--mode-line-string " Kawa"
  "Current mode-line string reflecting connection/auth state.")

(defvar kawacode--selection-timer nil
  "Timer for debounced cursor position updates.")

(defvar kawacode--last-cursor-line nil
  "Last cursor line number sent to Muninn.")

(defvar kawacode--is-cycling nil
  "Non-nil when actively cycling through peer diff blocks.")

;;; Logging Variables

(defvar kawacode--log-buffer "*Kawa Code Log*"
  "Buffer name for Kawa Code logs.")

(defvar kawacode--log-level 'info
  "Current log level.")

;;; Log Levels

(defconst kawacode--log-levels
  '((error . 0)
    (warn . 1)
    (info . 2)
    (log . 3)
    (debug . 4))
  "Log levels with their numeric values.")

;;; Logging Utility Functions

(defun kawacode--get-log-level-value (level)
  "Get the numeric value for a log LEVEL."
  (cdr (assoc level kawacode--log-levels)))

(defun kawacode--should-log (level)
  "Check if the given LEVEL should be logged."
  (<= (kawacode--get-log-level-value level)
      (kawacode--get-log-level-value kawacode--log-level)))

(defun kawacode--write-to-log-buffer (message)
  "Write a MESSAGE to the log buffer."
  (let ((buffer (get-buffer-create kawacode--log-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert message "\n")
      (when (and kawacode-debug (get-buffer-window buffer))
        (recenter -1)))))

;;; Logging Public API

;;;###autoload
(defun kawacode-log-error (message &rest args)
  "Log an error MESSAGE.
Optional argument ARGS optional formatting ."
  (when (kawacode--should-log 'error)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [ERROR] %s" timestamp formatted-message)))
      (kawacode--write-to-log-buffer log-entry)
      (message "Kawa Code Error: %s" formatted-message))))

;;;###autoload
(defun kawacode-log-warn (message &rest args)
  "Log a warning MESSAGE.
Optional argument ARGS optional formatting."
  (when (kawacode--should-log 'warn)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [WARN] %s" timestamp formatted-message)))
      (kawacode--write-to-log-buffer log-entry))))

;;;###autoload
(defun kawacode-log-info (message &rest args)
  "Log an info MESSAGE.
Optional argument ARGS formatting."
  (when (kawacode--should-log 'info)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [INFO] %s" timestamp formatted-message)))
      (kawacode--write-to-log-buffer log-entry))))

;;;###autoload
(defun kawacode-log (message &rest args)
  "Log a general MESSAGE.
Optional argument ARGS formatting."
  (when (kawacode--should-log 'log)
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [LOG] %s" timestamp formatted-message)))
      (kawacode--write-to-log-buffer log-entry))))

;;;###autoload
(defun kawacode-log-debug (message &rest args)
  "Log a debug MESSAGE.
Optional argument ARGS formatting."
  (when (and kawacode-debug (kawacode--should-log 'debug))
    (let* ((timestamp (format-time-string "%F %T"))
           (formatted-message (if args
                                  (apply #'format message args)
                                message))
           (log-entry (format "[%s] [DEBUG] %s" timestamp formatted-message)))
      (kawacode--write-to-log-buffer log-entry))))

;;;###autoload
(defun kawacode-show-log-buffer ()
  "Show the Kawa Code log buffer."
  (interactive)
  (let ((buffer (get-buffer-create kawacode--log-buffer)))
    (switch-to-buffer buffer)
    (goto-char (point-max))))

;;;###autoload
(defun kawacode-clear-log-buffer ()
  "Clear the Kawa Code log buffer."
  (interactive)
  (let ((buffer (get-buffer kawacode--log-buffer)))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)))))

;;; Store/State Management

(defvar kawacode--store nil
  "Central store for Kawa Code state.")

(defvar kawacode--projects nil
  "List of all projects.")

(defvar kawacode--active-selections nil
  "Currently active selections.")

(defvar kawacode--selected-peer nil
  "Currently selected peer.")

(defvar kawacode--color-theme 1
  "Current color theme (1=Light, 2=Dark, 3=High Contrast).")

(defvar kawacode--tmp-dir (expand-file-name "caw.emacs" (temporary-file-directory))
  "Temporary directory for Kawa Code.")

(defvar kawacode--peer-fs (make-hash-table :test 'equal)
  "Peer file system tree structure.")

(defvar kawacode--events-table (make-hash-table :test 'equal)
  "Hash table mapping event names to handler functions.")

(defvar kawacode--user nil
  "Current user data.")

(defvar kawacode--tokens nil
  "Authentication tokens.")

(defvar kawacode--authenticated nil
  "Whether the client is authenticated with the local service.")

;;; Highlighting System

(defvar kawacode--highlights (make-hash-table :test 'equal)
  "Hash table of highlights by buffer, tracking line numbers and overlay objects.")

(defvar kawacode--highlight-faces nil
  "Predefined faces for different types of highlights.")

(defvar kawacode--highlight-timer nil
  "Timer for debounced highlight refresh.")

;;; HL-Line Integration

(defvar kawacode--hl-line-overlays (make-hash-table :test 'equal)
  "Hash table of hl-line overlays by buffer and line number.")

(defvar kawacode--hl-line-faces (make-hash-table :test 'equal)
  "Hash table of custom hl-line faces by highlight type.")

;;; Configuration

(defun kawacode--init-config ()
  "Initialize configuration."
  (setq kawacode--config
        `((update-delay . ,kawacode-update-delay)))
  (kawacode-log-info "Configuration initialized"))

(defun kawacode--init-store ()
  "Initialize the central store."
  (setq kawacode--store
        `((active-project . ,kawacode--active-project)
          (projects . ,kawacode--projects)
          (active-buffer . ,kawacode--active-buffer)
          (active-selections . ,kawacode--active-selections)
          (selected-peer . ,kawacode--selected-peer)
          (color-theme . ,kawacode--color-theme)
          (tmp-dir . ,kawacode--tmp-dir)
          (peer-fs . ,kawacode--peer-fs)
          (user . ,kawacode--user)
          (tokens . ,kawacode--tokens)))
  (kawacode-log-info "Store initialized"))

(defun kawacode--register-event-handler (event-name handler-function)
  "Register an event handler function for the given event name.
Argument EVENT-NAME string in the format category:action, e.g. peer:select.
Argument HANDLER-FUNCTION a ref to the function that should handle the event."
  (puthash event-name handler-function kawacode--events-table)
  (kawacode-log-info "Registered event handler for: %s" event-name))

(defun kawacode--init-event-handlers ()
  "Initialize all event handlers."
  ;; Clear existing handlers
  (clrhash kawacode--events-table)

  ;; Register event handlers
  (kawacode--register-event-handler "peer:select" #'kawacode--handle-peer-select)
  (kawacode--register-event-handler "peer:unselect" #'kawacode--handle-peer-unselect)
  (kawacode--register-event-handler "branch:select" #'kawacode--handle-branch-select)
  (kawacode--register-event-handler "branch:unselect" #'kawacode--handle-branch-unselect)
  (kawacode--register-event-handler "branch:refresh" #'kawacode--handle-branch-refresh)
  (kawacode--register-event-handler "auth:logout" #'kawacode--handle-auth-logout)
  (kawacode--register-event-handler "context:add" #'kawacode--handle-context-add)
  (kawacode--register-event-handler "context:del" #'kawacode--handle-context-del)
  (kawacode--register-event-handler "context:open-rel" #'kawacode--handle-context-open-rel)
  (kawacode--register-event-handler "sync:setup" #'kawacode--handle-sync-setup-broadcast)

  (kawacode-log-info "Event handlers initialized"))

(defun kawacode--clear-store ()
  "Clear the store and reset all state."
  (kawacode-log-info "Clearing store")
  (setq kawacode--tokens nil
        kawacode--user nil
        kawacode--authenticated nil
        kawacode--active-project nil
        kawacode--active-buffer nil
        kawacode--active-selections nil
        kawacode--selected-peer nil
        kawacode--color-theme 1
        kawacode--tmp-dir (expand-file-name "caw.emacs" (temporary-file-directory))
        kawacode--peer-fs (make-hash-table :test 'equal))
  (kawacode--init-store))

(defun kawacode--reset-store ()
  "Reset store state (keep user/tokens)."
  (kawacode-log-info "Resetting store")
  (setq kawacode--peer-fs (make-hash-table :test 'equal)
        kawacode--active-buffer nil
        kawacode--active-selections nil)
  (kawacode--init-store))

;;; Project Management

(defun kawacode--add-project (project)
  "Add a PROJECT to the store."
  (kawacode-log-info "Adding project %s" (alist-get 'root project))
  (setq kawacode--active-project project)
  ;; Add to projects list if not already present
  (unless (cl-find (alist-get 'root project) kawacode--projects
                   :key (lambda (p) (alist-get 'root p)) :test 'string=)
    (push project kawacode--projects))
  (kawacode--init-store)
  project)

(defun kawacode--get-active-file-path ()
  "Get the path of the currently active file."
  (when (and kawacode--active-buffer
             (buffer-live-p kawacode--active-buffer))
    (buffer-file-name kawacode--active-buffer)))

(defun kawacode--get-active-file-content ()
  "Get the content of the currently active file."
  (when (and kawacode--active-buffer
             (buffer-live-p kawacode--active-buffer))
    (with-current-buffer kawacode--active-buffer
      (buffer-string))))

(defun kawacode--cross-platform-path (path)
  "Convert PATH to cross-platform format (forward slashes)."
  (when path
    (replace-regexp-in-string "\\\\" "/" path)))

;;; Workspace Management

;; The refreshActiveFile hook implementation follows the VSCode pattern:
;; 1. Called immediately after authentication is successful
;; 2. Called whenever the active buffer changes
;; 3. Sends a code:active-path message to Kawa Code app with the current file path and content
;; 4. Updates highlights and project data based on the response

(defun kawacode--refresh-active-file ()
  "Refresh the currently active file by sending code:active-path message."

  ;; Check if we have the necessary components to send a refresh request
  (let ((fpath (kawacode--get-active-file-path))
        (doc (kawacode--get-active-file-content)))
    (if (not fpath)
        (kawacode-log-info "No active file to refresh")
      (if (not kawacode--authenticated)
          (kawacode-log-warn "Not authenticated, skipping file refresh")
        (if (not (and kawacode--ipc-process
                      (eq (process-status kawacode--ipc-process) 'open)))
            (kawacode-log-warn "Kawa Code IPC process not ready, skipping file refresh")
          (kawacode-log-info "Refreshing active file %s" fpath)
          (let ((message-data `((fpath . ,(kawacode--cross-platform-path fpath))
                                (doc . ,doc)
                                (caw . ,kawacode--caw))))
            (kawacode--transmit "active-path" message-data)
            (kawacode--setup-response-handler "code" "active-path" fpath)))))))

;;; Highlighting System

(defun kawacode--init-highlight-faces ()
  "Initialize predefined faces for different highlight types."
  (setq kawacode--highlight-faces
        `((conflict . ,(make-face 'kawacode-conflict-face))
          (overlap . ,(make-face 'kawacode-overlap-face))
          (peer . ,(make-face 'kawacode-peer-face))
          (modified . ,(make-face 'kawacode-modified-face))))

  ;; Set face attributes based on color theme
  (let ((conflict-face (alist-get 'conflict kawacode--highlight-faces))
        (overlap-face (alist-get 'overlap kawacode--highlight-faces))
        (peer-face (alist-get 'peer kawacode--highlight-faces))
        (modified-face (alist-get 'modified kawacode--highlight-faces)))

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
    (kawacode--init-hl-line-faces))

  (kawacode-log-info "Highlight faces initialized"))

(defun kawacode--get-highlight-face (type)
  "Get the face for the given highlight TYPE."
  (alist-get type kawacode--highlight-faces))

(defun kawacode--create-line-overlay (buffer line-number face &optional properties)
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
            (overlay-put overlay 'kawacode-type 'line-highlight)
            (overlay-put overlay 'kawacode-line line-number)
            ;; Add any additional properties
            (when properties
              (dolist (prop properties)
                (overlay-put overlay (car prop) (cdr prop))))
            overlay))))))

(defun kawacode--clear-buffer-highlights (buffer)
  "Clear all Kawa Code highlight from the given BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (overlay-get overlay 'kawacode-type)
          (delete-overlay overlay))))
    ;; Remove from highlights hash table
    (remhash buffer kawacode--highlights)
    ;; Also clear hl-line highlights if using that mode
    (kawacode--clear-buffer-hl-line-highlights buffer)
    (kawacode-log-info "Cleared highlights for buffer %s" buffer)))

(defun kawacode--clear-all-highlights ()
  "Clear all Kawa Code highlight from all buffers."
  (dolist (buffer (buffer-list))
    (kawacode--clear-buffer-highlights buffer))
  (clrhash kawacode--highlights)
  ;; Also clear hl-line highlights if using that mode
  (dolist (buffer (buffer-list))
    (kawacode--clear-buffer-hl-line-highlights buffer))
  (clrhash kawacode--hl-line-overlays)
  (kawacode-log-info "Cleared all highlights"))

(defun kawacode--apply-highlights-from-data (buffer highlight-data)
  "Apply highlight to BUFFER based on data from the local service.
Argument HIGHLIGHT-DATA the array of lines to highlight."
  (when (and buffer (buffer-live-p buffer) highlight-data)
    ;; Use hl-line mode if configured, otherwise use custom overlays
    (kawacode--apply-hl-line-highlights-from-data buffer highlight-data)))

(defun kawacode--convert-hl-to-highlights (hl-data)
  "Convert hl data structure to highlight format.
HL-DATA should be an array
of line numbers.  Returns a list of highlight alists with \\='line and \\='type keys."
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

(defun kawacode--init-hl-line-faces ()
  "Initialize hl-line faces for different highlight types."
  (when (featurep 'hl-line)
    (setq kawacode--hl-line-faces
          `((conflict . ,(make-face 'kawacode-hl-line-conflict))
            (overlap . ,(make-face 'kawacode-hl-line-overlap))
            (peer . ,(make-face 'kawacode-hl-line-peer))
            (modified . ,(make-face 'kawacode-hl-line-modified))))
    ;; Set face properties based on theme
    (let ((conflict-face (alist-get 'conflict kawacode--hl-line-faces))
          (overlap-face (alist-get 'overlap kawacode--hl-line-faces))
          (peer-face (alist-get 'peer kawacode--hl-line-faces))
          (modified-face (alist-get 'modified kawacode--hl-line-faces)))
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
    (kawacode-log-info "HL-line faces initialized")))

(defun kawacode--get-hl-line-face (type)
  "Get the hl-line face for the given highlight TYPE."
  (or (alist-get type kawacode--hl-line-faces)
      ;; Fallback to default hl-line face if not found
      'hl-line))

(defun kawacode--add-hl-line-highlight (buffer line-number type &optional properties)
  "Add a highlight using hl-line mode to the specified line in the given BUFFER.
Argument LINE-NUMBER the line number to highlight.
Argument TYPE the type of highlight to use (one of the hl-lines faces).
Optional argument PROPERTIES optional properties for hl-lines overlay."
  (when (and buffer line-number type (featurep 'hl-line))
    ;; Ensure hl-line faces are initialized
    (unless kawacode--hl-line-faces
      (kawacode--init-hl-line-faces))
    (let* ((face (kawacode--get-hl-line-face type))
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
      (overlay-put overlay 'kawacode-type 'hl-line-highlight)
      (overlay-put overlay 'kawacode-line line-number)
      (overlay-put overlay 'kawacode-highlight-type type)
      (overlay-put overlay 'kawacode-properties properties)
      ;; Store highlight information
      (let ((buffer-highlights (gethash buffer kawacode--hl-line-overlays)))
        (unless buffer-highlights
          (setq buffer-highlights (make-hash-table :test 'equal))
          (puthash buffer buffer-highlights kawacode--hl-line-overlays))
        (puthash line-number overlay buffer-highlights))
      overlay)))

(defun kawacode--clear-buffer-hl-line-highlights (buffer)
  "Clear all Kawa Code hl-line highlight from the given BUFFER."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (eq (overlay-get overlay 'kawacode-type) 'hl-line-highlight)
          (delete-overlay overlay))))
    ;; Remove from highlights hash table
    (remhash buffer kawacode--hl-line-overlays)))

(defun kawacode--apply-hl-line-highlights-from-data (buffer highlight-data)
  "Apply hl-line highlight to BUFFER based on data from the local service.
Argument HIGHLIGHT-DATA the array of lines to highlight."
  (when (and buffer (buffer-live-p buffer) highlight-data (featurep 'hl-line))
    ;; Clear existing highlights first
    (kawacode--clear-buffer-hl-line-highlights buffer)
    ;; Apply new highlights
    (dolist (highlight highlight-data)
      (let ((line (alist-get 'line highlight))
            (type (alist-get 'type highlight))
            (properties (alist-get 'properties highlight)))
        (when (and line type)
          (run-with-timer 1.0 nil
                          (lambda ()
                            (kawacode--add-hl-line-highlight buffer line type properties))))))))

;;; Mode-Line

(defun kawacode--update-mode-line ()
  "Update the mode-line string based on current state."
  (setq kawacode--mode-line-string
        (cond
         ((not kawacode--connected) " Kawa[off]")
         (kawacode--authenticated
          (let ((name (alist-get 'name kawacode--user)))
            (if name (format " Kawa[%s]" name) " Kawa")))
         (t " Kawa")))
  (force-mode-line-update t))

;;; IPC Communication

(defun kawacode--generate-msg-id ()
  "Generate a UUID-like message ID for request-response correlation."
  (format "%s-%04x-%04x"
          (format-time-string "%s%3N")
          (random 65535)
          (random 65535)))

(defun kawacode--get-muninn-socket-path ()
  "Get the muninn socket path for direct connection.
On macOS, checks the App Sandbox container first (for App Store
builds), then falls back to the non-sandboxed path."
  (cond
   ((eq system-type 'windows-nt)
    "\\\\.\\pipe\\muninn")
   ((eq system-type 'darwin)
    (let ((container-dir (expand-file-name
                          "Library/Containers/com.codeawareness.muninn/Data/Library/Application Support/Kawa Code/sockets"
                          (getenv "HOME"))))
      (if (file-directory-p container-dir)
          (expand-file-name "muninn" container-dir)
        (expand-file-name "muninn"
                          (expand-file-name "Library/Application Support/Kawa Code/sockets"
                                            (getenv "HOME"))))))
   (t
    (format "%s/sockets/muninn" (expand-file-name "~/.kawacode")))))

(defun kawacode--ipc-sentinel (_process event)
  "Handle IPC process sentinel EVENTs."
  (kawacode-log-info "Muninn IPC: %s" event)
  (cond
   ((string-match "failed" event)
    (kawacode-log-error "Muninn connection failed")
    (setq kawacode--connected nil)
    (kawacode--update-mode-line)
    ;; Retry connection
    (run-with-timer 2.0 nil #'kawacode--connect-to-muninn))
   ((string-match "exited" event)
    (kawacode-log-warn "Muninn connection closed")
    (setq kawacode--connected nil)
    (kawacode--update-mode-line))
   ((string-match "connection broken by remote peer" event)
    (kawacode-log-warn "Muninn rejected connection")
    (setq kawacode--connected nil)
    (kawacode--update-mode-line)
    ;; Retry connection after a delay
    (run-with-timer 2.0 nil #'kawacode--connect-to-muninn))
   ((string-match "open" event)
    (kawacode-log-info "Successfully connected to Muninn")
    (setq kawacode--connected t)
    (kawacode--update-mode-line)
    ;; Send handshake to receive CAW ID from Muninn
    (kawacode--send-handshake))
   (t
    (kawacode-log-warn "Unknown IPC sentinel event: %s" event))))

(defun kawacode--ipc-filter (process data)
  "Handle IPC PROCESS DATA."
  (let ((buffer (process-buffer process)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert data)
        (kawacode--process-ipc-messages)))))

(defun kawacode--process-ipc-messages ()
  "Process complete IPC messages from the buffer."
  (let ((delimiter "\n"))
    (goto-char (point-min))
    (while (search-forward delimiter nil t)
      (let* ((end-pos (point))
             (start-pos (point-min))
             (message (buffer-substring-no-properties start-pos (1- end-pos))))
        (delete-region start-pos end-pos)
        ;; Skip empty messages
        (unless (string-empty-p (string-trim message))
          (kawacode--handle-ipc-message message))))))

(defun kawacode--handle-ipc-message (message)
  "Handle a single IPC MESSAGE."
  (condition-case err
      (let* ((data (json-read-from-string message))
             (domain (alist-get 'domain data))
             (action (alist-get 'action data))
             (response-data (alist-get 'data data))
             (error-data (alist-get 'err data))
             (msg-id (alist-get '_msgId data)))
        ;; Normalize JSON null → nil for msg-id (Emacs json-read may
        ;; yield :json-null for JSON null values).
        (when (eq msg-id :json-null) (setq msg-id nil))
        (kawacode-log-info "%s:%s (msgId: %s)" domain action msg-id)

        ;; First check for _msgId correlation (new pattern)
        (if (and msg-id (gethash msg-id kawacode--pending-requests))
            ;; Found pending request by _msgId - call its handler
            (let ((handler (gethash msg-id kawacode--pending-requests)))
              (remhash msg-id kawacode--pending-requests)
              (if error-data
                  (kawacode-log-error "Request %s failed: %s" msg-id error-data)
                (funcall handler response-data)))

          ;; Fallback: no _msgId match
          (if (and error-data action)
              ;; Has 'err' field - this is an error response
              (kawacode--handle-error domain action error-data)
            (when action
              ;; Try registered response handlers first
              (let ((handled (kawacode--handle-response domain action response-data)))
                (unless handled
                  ;; Not a known response — try events table
                  ;; (broadcasts with data end up here, e.g. peer:select)
                  (let ((handler (gethash action kawacode--events-table)))
                    (if handler
                        (progn
                          (kawacode-log-info "Dispatching %s:%s via events table"
                                                  domain action)
                          (funcall handler response-data))
                      (kawacode-log-info "Unhandled message: %s:%s"
                                              domain action)))))))))
    (error
     (kawacode-log-error "Error parsing IPC message: %s" err))))

(defun kawacode--handle-response (domain action data)
  "Handle an IPC response.  Return non-nil if handled.
Argument DOMAIN string describing the event domain, e.g. code, auth, etc.
Argument ACTION string describing the event action, e.g. auth:info.
Argument DATA additional data received from Kawa Code (JSON)."
  (let* ((key (format "res:%s:%s" domain action))
         (handler (gethash key kawacode--response-handlers)))
    (cond
     ;; Handshake response (Muninn doesn't echo _msgId)
     ((and (string= domain "system") (string= action "handshake"))
      (kawacode--handle-handshake-response data) t)
     ;; Auth responses — domain "auth" from Gardener
     ((and (string= domain "auth") (or (string= action "info") (string= action "login")))
      (kawacode--handle-auth-info-response data) t)
     ;; Open peer file broadcast from Muninn (domain "code")
     ((and (string= domain "code") (string= action "open-peer-file"))
      (kawacode--handle-open-peer-file-response data) t)
     ;; Sync setup broadcast from Muninn (domain "code")
     ((and (string= domain "code") (string= action "sync:setup"))
      (kawacode--handle-sync-setup-broadcast data) t)
     ;; Branch diff broadcast from Muninn (domain "code")
     ((and (string= domain "code") (string= action "branch:select"))
      (kawacode--handle-branch-diff-response data) t)
     ;; Other responses with registered handlers
     (handler
      (remhash key kawacode--response-handlers)
      (funcall handler data) t))))

(defun kawacode--handle-repo-active-path-response (data &optional expected-file-path)
  "Handle response from code:active-path request.
EXPECTED-FILE-PATH is the
file path that was originally requested (for validation).
Argument DATA the data received from Kawa Code application."
  (kawacode-log-info "Received code:active-path response")
  ;; Unwrap project envelope (Muninn may wrap in { project: {...} })
  (let* ((project (or (alist-get 'project data) data))
         ;; Map highlights→hl (Muninn may use either field name)
         (hl-data (or (alist-get 'hl project) (alist-get 'highlights project)))
         (buffer kawacode--active-buffer))
    ;; Add the unwrapped project to our store
    (kawacode--add-project project)
    ;; Debug logging for highlight data
    (kawacode-log-info "hl-data received: %s" (prin1-to-string hl-data))
    (kawacode-log-info "hl-data type: %s, length: %s"
                            (type-of hl-data)
                            (if hl-data (length hl-data) "nil"))
    (if (and hl-data buffer (buffer-live-p buffer))
        ;; Validate that the buffer still corresponds to the expected file
        (let ((current-file-path (buffer-file-name buffer)))
          (if (and expected-file-path current-file-path
                   (string= (kawacode--cross-platform-path expected-file-path)
                            (kawacode--cross-platform-path current-file-path)))
              ;; File paths match, apply highlights
              (progn
                ;; Convert hl data to highlight format
                (let ((highlights (kawacode--convert-hl-to-highlights hl-data)))
                  (kawacode-log-info "Number of highlights: %s" (length highlights))
                  (when highlights
                    (kawacode--apply-highlights-from-data buffer highlights)))))))))

(defun kawacode--handle-auth-info-response (data)
  "Handle response from auth:info request.
Argument DATA the data received from Kawa Code application."
  (if (and data (listp data) (alist-get 'user data))
      (progn
        (setq kawacode--user (alist-get 'user data))
        (setq kawacode--tokens (alist-get 'tokens data))
        (setq kawacode--authenticated t)
        (kawacode--update-mode-line)
        (kawacode-log-info "Authentication successful")
        (message "Authenticated as %s" (alist-get 'name kawacode--user))
        ;; Refresh active file immediately after authentication (like VSCode's init function)
        (kawacode--refresh-active-file))
    (setq kawacode--authenticated nil)
    (kawacode--update-mode-line)
    (kawacode-log-warn "No authentication data received - user needs to authenticate")))

(defun kawacode--handle-peer-select (peer-data)
  "Handle peer selection event from Muninn app.
Argument PEER-DATA the data received from Kawa Code (peer info)."
  (kawacode-log-info "Peer selected: %s (data keys: %s)"
                          (alist-get 'name peer-data)
                          (mapcar #'car (when (listp peer-data) peer-data)))
  (setq kawacode--selected-peer peer-data)

  ;; Get active project information
  (let* ((active-project kawacode--active-project)
         (origin (alist-get 'origin active-project))
         (fpath (alist-get 'activePath active-project))
         ;; If broadcast includes origin, only respond if it matches
         (broadcast-origin (alist-get 'origin peer-data)))
    (kawacode-log-info "Active project: origin=%s fpath=%s (project keys: %s)"
                            origin fpath
                            (mapcar #'car (when (listp active-project) active-project)))
    (cond
     ((not fpath)
      (kawacode-log-warn "No active file path for peer diff"))
     ((and broadcast-origin origin
           (not (string= broadcast-origin origin)))
      (kawacode-log-info "Ignoring peer:select for %s (active: %s)"
                               broadcast-origin origin))
     (t
      (let ((message-data `((origin . ,origin)
                            (fpath . ,fpath)
                            (caw . ,kawacode--caw)
                            (peer . ,peer-data))))
        (kawacode-log-info "Requesting peer diff for %s" fpath)
        (kawacode--transmit "diff-peer" message-data))))))

(defun kawacode--handle-peer-unselect ()
  "Handle peer unselection event from Muninn app."
  (kawacode-log-info "Peer unselected")
  (setq kawacode--selected-peer nil)
  ;; Close any open diff buffers
  (kawacode--close-diff-buffers))

(defun kawacode--handle-peer-diff-response (data)
  "Handle response from code:diff-peer request.
Argument DATA the data received from Kawa Code (peer file info)."
  (kawacode-log-info "Received peer diff response")
  (let* ((peer-file (alist-get 'peerFile data))
         (title (alist-get 'title data))
         (active-project kawacode--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (user-file (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (and peer-file user-file)
        (progn
          (kawacode-log-info "Opening diff: %s vs %s" user-file peer-file)
          (kawacode--open-diff-view user-file peer-file title))
      (kawacode-log-error "Missing file paths for diff: peer-file=%s, user-file=%s"
                               peer-file user-file))))

(defun kawacode--handle-open-peer-file-response (data)
  "Handle response from code:open-peer-file request.
The local service has downloaded/extracted the file and provides the full path.
Argument DATA the data received from Kawa Code local service."
  (kawacode-log-info "Received open-peer-file response")
  (let* ((file-path (alist-get 'filePath data))
         (file-paths (alist-get 'filePaths data))
         (exists (alist-get 'exists data)))
    (cond
     ;; Case 1: Single file path provided
     (file-path
      (kawacode-log-info "Opening peer file: %s (exists locally: %s)" file-path exists)
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
        (kawacode-log-error "File does not exist: %s" file-path)
        (message "Error: File does not exist: %s" file-path)))

     ;; Case 2: Two file paths provided (diff mode)
     ((and file-paths (vectorp file-paths) (>= (length file-paths) 2))
      (let ((file1 (aref file-paths 0))
            (file2 (aref file-paths 1)))
        (kawacode-log-info "Opening diff between: %s and %s" file1 file2)
        (if (and (file-exists-p file1) (file-exists-p file2))
            (progn
              ;; Open diff view using ediff or fallback to diff-mode
              (kawacode--open-diff-view file1 file2 "Peer File Comparison"))
          (kawacode-log-error "One or both files do not exist: %s, %s" file1 file2)
          (message "Error: One or both files do not exist"))))

     ;; Case 3: No valid data
     (t
      (kawacode-log-error "Invalid code:open-peer-file response: %s" data)
      (message "Error: Invalid file path data in code:open-peer-file response")))))

(defun kawacode--open-diff-view (peer-file user-file title)
  "Open a diff view comparing peer file with user file.
Argument PEER-FILE the path of the peer file that was extracted (in tmp folder).
Argument USER-FILE the path of the existing file in the buffer.
Argument TITLE title of the diff buffer."
  ;; Close any existing EDiff session first
  (when (and (boundp 'ediff-control-buffer) ediff-control-buffer
             (buffer-live-p ediff-control-buffer))
    (kawacode-log-info "Closing existing EDiff session")
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
          (kawacode-log-info "Using ediff for diff view")
          (ediff-buffers peer-buffer user-buffer))
      ;; Fallback to diff-mode in a separate buffer
      (let ((diff-buffer-name (format "*Kawa Code Diff: %s*" title)))
        (let ((diff-buffer (get-buffer-create diff-buffer-name)))
          (with-current-buffer diff-buffer
            ;; Clear the buffer
            (erase-buffer)
            ;; Insert diff content
            (let ((diff-output (kawacode--generate-diff peer-file user-file)))
              (insert diff-output)
              ;; Set up the buffer for diff viewing
              (diff-mode)
              ;; Make the buffer read-only
              (setq buffer-read-only t)
              ;; Display the buffer
              (switch-to-buffer diff-buffer)
              (message "Opened diff view: %s" title))))))))

(defun kawacode--generate-diff (file1 file2)
  "Generate diff output between two files.
Argument FILE1 the first file in the diff command.
Argument FILE2 the second file in the diff command."
  (let ((diff-command (format "diff -u %s %s" file1 file2)))
    (with-temp-buffer
      (let ((exit-code (call-process-shell-command diff-command nil t)))
        (if (= exit-code 0)
            "Files are identical"
          (buffer-string))))))

(defun kawacode--close-diff-buffers ()
  "Close all Kawa Code diff buffers."
  (dolist (buffer (buffer-list))
    (when (and (string-match "\\*Kawa Code Diff:" (buffer-name buffer))
               (buffer-live-p buffer))
      (kill-buffer buffer)))
  (message "Closed Kawa Code diff buffers"))

;;; Additional Event Handlers

(defun kawacode--handle-branch-select (branch-or-data)
  "Handle BRANCH selection event.
Two cases:
1. String branch name from webview - transmit to Gardener for processing
2. Response data from Gardener/Muninn with peerFile/userFile
   - open diff directly
Argument BRANCH-OR-DATA either a string branch name or an alist with diff data."
  (cond
   ;; Case 1: Branch name string - need to request diff from Gardener
   ((stringp branch-or-data)
    (kawacode-log-info "Branch selected: %s (requesting diff)" branch-or-data)
    (let ((message-data `((branch . ,branch-or-data)
                          (caw . ,kawacode--caw))))
      (kawacode--transmit "branch:select" message-data)
      (kawacode--setup-response-handler "code" "branch:select")))

   ;; Case 2: Branch name in data alist - extract and process
   ((and (listp branch-or-data) (alist-get 'branch branch-or-data))
    (let ((branch-name (alist-get 'branch branch-or-data)))
      (kawacode-log-info "Branch selected: %s (requesting diff)" branch-name)
      (when branch-name
        (let ((message-data `((branch . ,branch-name)
                              (caw . ,kawacode--caw))))
          (kawacode--transmit "branch:select" message-data)
          (kawacode--setup-response-handler "code" "branch:select")))))

   ;; Case 3: Already-processed diff data from Muninn with peerFile and userFile
   ((and (listp branch-or-data)
         (alist-get 'peerFile branch-or-data)
         (alist-get 'userFile branch-or-data))
    (let ((peer-file (alist-get 'peerFile branch-or-data))
          (user-file (alist-get 'userFile branch-or-data))
          (title (alist-get 'title branch-or-data)))
      (kawacode-log-info "Opening pre-processed branch diff: %s vs %s" user-file peer-file)
      (kawacode--open-diff-view user-file peer-file title)))

   ;; Case 4: Invalid data
   (t
    (kawacode-log-error "branch:select: Invalid data format %s" branch-or-data)
    (message "Invalid branch selection data"))))

(defun kawacode--handle-branch-unselect ()
  "Handle branch unselection event."
  (kawacode-log-info "Branch unselected")
  (kawacode--close-diff-buffers))

(defun kawacode--handle-branch-refresh (_data)
  "Handle branch refresh event."
  (kawacode-log-info "Branch refresh requested")
  ;; TODO: Implement branch refresh using git and display in panel
  (message "Branch refresh not yet implemented"))

(defun kawacode--handle-auth-logout ()
  "Handle auth logout event."
  (kawacode-log-info "Auth logout requested")
  (kawacode--clear-store)
  (kawacode--clear-all-highlights)
  (message "Logged out"))

(defun kawacode--handle-context-add (context)
  "Handle CONTEXT add event.  TODO: work in progress."
  (kawacode-log-info "Context add requested: %s" context)
  (let* ((active-project kawacode--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (full-path (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (not full-path)
        (kawacode-log-warn "No active file path for context add")
      (let ((message-data `((fpath . ,full-path)
                            (selections . ,kawacode--active-selections)
                            (context . ,context)
                            (op . "add")
                            (caw . ,kawacode--caw))))
        (kawacode--transmit "context:apply" message-data)
        (kawacode--setup-response-handler "code" "context:apply")))))

(defun kawacode--handle-context-del (context)
  "Handle CONTEXT delete event."
  (kawacode-log-info "Context delete requested: %s" context)
  (let* ((active-project kawacode--active-project)
         (root (alist-get 'root active-project))
         (fpath (alist-get 'activePath active-project))
         (full-path (when (and root fpath)
                      (expand-file-name fpath root))))
    (if (not full-path)
        (kawacode-log-warn "No active file path for context delete")
      (let ((message-data `((fpath . ,full-path)
                            (selections . ,kawacode--active-selections)
                            (context . ,context)
                            (op . "del")
                            (caw . ,kawacode--caw))))
        (kawacode--transmit "context:apply" message-data)
        (kawacode--setup-response-handler "code" "context:apply")))))

(defun kawacode--handle-context-open-rel (data)
  "Handle context open relative event.
Argument DATA the data received from Kawa Code application."
  (kawacode-log-info "Context open relative requested: %s" (alist-get 'sourceFile data))
  (let ((source-file (alist-get 'sourceFile data)))
    (when source-file
      (find-file source-file))))


;;; Response Handlers

(defun kawacode--handle-branch-diff-response (data)
  "Handle response from code:branch:select request.
Argument DATA the data received from Kawa Code application."
  (kawacode-log-info "Received branch diff response")
  (let* ((peer-file (alist-get 'peerFile data))
         (user-file (alist-get 'userFile data))
         (title (alist-get 'title data))
         (broadcast-origin (alist-get 'origin data))
         (active-origin (alist-get 'origin kawacode--active-project)))
    ;; Filter: if broadcast includes origin, only respond if it matches
    (cond
     ((and broadcast-origin active-origin
           (not (string= broadcast-origin active-origin)))
      (kawacode-log-info "Ignoring branch:select for %s (active: %s)"
                               broadcast-origin active-origin))
     ((and peer-file user-file)
      (kawacode-log-info "Opening branch diff: %s vs %s" user-file peer-file)
      (kawacode--open-diff-view user-file peer-file title))
     (t
      (kawacode-log-error "Missing file paths for branch diff: peer-file=%s, user-file=%s"
                               peer-file user-file)))))

(defun kawacode--handle-context-apply-response (_data)
  "Handle response from context:apply request."
  (kawacode-log-info "Received context apply response")
  ;; TODO: Handle context update response
  (message "Context applied successfully"))

(defun kawacode--handle-error (domain action error-data)
  "Handle an IPC error.
Argument DOMAIN the request domain, e.g. auth, code, etc.
Argument ACTION the request action, e.g. auth:info.
Argument ERROR-DATA incoming error message."
  (let* ((key (format "err:%s:%s" domain action))
         (handler (gethash key kawacode--response-handlers)))
    (when handler
      (remhash key kawacode--response-handlers)
      (funcall handler error-data))))

(defun kawacode--transmit (action data &optional callback)
  "Transmit a message to the Kawa Code IPC.
ACTION is parsed like VSCode: if it contains \\=`:' the prefix
becomes the domain and the rest becomes the action.  E.g.
\\='auth:info' → domain=auth action=info.  Otherwise domain
defaults to \\='code'.
Argument DATA data to send to Kawa Code application.
Optional argument CALLBACK function to call when response is received."
  (let* ((parts (split-string action ":" t))
         (domain (if (> (length parts) 1) (car parts) "code"))
         (actual-action (if (> (length parts) 1)
                            (mapconcat #'identity (cdr parts) ":")
                          action))
         (msg-id (kawacode--generate-msg-id))
         (message (json-encode `((flow . "req")
                                 (domain . ,domain)
                                 (action . ,actual-action)
                                 (data . ,data)
                                 (caw . ,kawacode--caw)
                                 (_msgId . ,msg-id)))))
    (if kawacode--ipc-process
        (if (eq (process-status kawacode--ipc-process) 'open)
            (progn
              (kawacode-log-info "Sending %s:%s (msgId: %s)" domain actual-action msg-id)
              ;; Store callback for this message ID if provided
              (when callback
                (puthash msg-id callback kawacode--pending-requests))
              (process-send-string kawacode--ipc-process (concat message "\n"))
              (kawacode--setup-response-handler domain actual-action))
          (kawacode-log-error "IPC process exists but is not open (status: %s)"
                                   (process-status kawacode--ipc-process)))
      (kawacode-log-error "No IPC process available for transmission"))))

(defun kawacode--setup-response-handler (domain action &optional file-path)
  "Setup response handlers for the given DOMAIN and ACTION.
DOMAIN and ACTION are already split (e.g. \"auth\" \"info\").
FILE-PATH is the file path associated with this request (for validation)."
  (let* ((key (format "%s:%s" domain action))
         (res-key (format "res:%s" key))
         (err-key (format "err:%s" key)))
    ;; Set up specific handlers for known actions
    (cond
     ((string= key "code:active-path")
      (puthash res-key (lambda (data) (kawacode--handle-repo-active-path-response data file-path)) kawacode--response-handlers))
     ((string= key "code:diff-peer")
      (puthash res-key #'kawacode--handle-peer-diff-response kawacode--response-handlers))
     ((string= key "branch:select")
      (puthash res-key #'kawacode--handle-branch-diff-response kawacode--response-handlers)
      (puthash err-key (lambda (err)
                         (kawacode-log-error "Branch diff error: %s" err)
                         (message "Branch diff failed: %s" (or (alist-get 'message err) err)))
               kawacode--response-handlers))
     ((string= key "code:open-peer-file")
      (puthash res-key #'kawacode--handle-open-peer-file-response kawacode--response-handlers)
      (puthash err-key (lambda (err)
                         (kawacode-log-error "Open peer file error: %s" err)
                         (message "Failed to open peer file: %s" (or (alist-get 'message err) err)))
               kawacode--response-handlers))
     ((string= key "code:get-tmp-dir")
      (puthash res-key #'kawacode--handle-get-tmp-dir-response kawacode--response-handlers))
     ((string= key "context:apply")
      (puthash res-key #'kawacode--handle-context-apply-response kawacode--response-handlers))
     ((or (string= key "auth:info") (string= key "auth:login"))
      (puthash res-key #'kawacode--handle-auth-info-response kawacode--response-handlers))
     (t
      (puthash res-key #'kawacode--handle-success kawacode--response-handlers)))
    ;; Set up generic error handler for actions without specific error handlers
    (unless (or (string= key "branch:select")
                (string= key "code:open-peer-file"))
      (puthash err-key #'kawacode--handle-failure kawacode--response-handlers))))

(defun kawacode--handle-success (data)
  "Handle successful IPC response.
Argument DATA data received from the request."
  (kawacode-log-info "Success - %s" (format "%s" data)))

(defun kawacode--handle-failure (error-data)
  "Handle failed IPC response for unknown actions.
Argument ERROR-DATA error message received from the request."
  (kawacode-log-error "Error handle for unknown action - %s" (format "%s" error-data)))

;;; Connection Management

(defun kawacode--init-ipc ()
  "Initialize IPC by connecting directly to Muninn socket."
  (kawacode-log-info "Initializing IPC - connecting to Muninn")
  (kawacode--connect-to-muninn))

(defun kawacode--connect-to-muninn ()
  "Connect directly to Muninn socket and perform handshake."
  (let* ((socket-path (kawacode--get-muninn-socket-path))
         (process-name "kawacode-muninn")
         (buffer-name "*kawacode-muninn*"))
    (kawacode-log-info "Connecting to Muninn at %s" socket-path)
    (condition-case err
        (progn
          (setq kawacode--ipc-process
                (make-network-process
                 :name process-name
                 :buffer buffer-name
                 :family 'local
                 :service socket-path
                 :sentinel #'kawacode--ipc-sentinel
                 :filter #'kawacode--ipc-filter
                 :noquery t))
          (kawacode-log-info "Muninn connection initiated (status: %s)"
                                  (process-status kawacode--ipc-process))
          ;; For local sockets the connection is synchronous — the
          ;; process may already be open before the sentinel fires.
          ;; Handle that explicitly so the handshake isn't missed.
          (when (eq (process-status kawacode--ipc-process) 'open)
            (kawacode-log-info "Connection already open, initiating handshake")
            (setq kawacode--connected t)
            (kawacode--update-mode-line)
            (kawacode--send-handshake)))
      (error
       (kawacode-log-error "Failed to connect to Muninn: %s" err)
       (message "Failed to connect to Muninn at %s. Is Kawa Code running? Error: %s"
                socket-path err)
       ;; Retry connection after delay
       (run-with-timer 5.0 nil #'kawacode--connect-to-muninn)))))

(defun kawacode--send-handshake ()
  "Send handshake message to Muninn to receive CAW ID."
  (when (and kawacode--ipc-process
             (eq (process-status kawacode--ipc-process) 'open))
    (let* ((msg-id (kawacode--generate-msg-id))
           (message (json-encode `((flow . "req")
                                   (domain . "system")
                                   (action . "handshake")
                                   (data . ((clientType . "emacs")))
                                   (_msgId . ,msg-id)))))
      (kawacode-log-info "Sending handshake to Muninn")
      (process-send-string kawacode--ipc-process (concat message "\n"))
      ;; Store handler for handshake response
      (puthash msg-id #'kawacode--handle-handshake-response kawacode--pending-requests))))

(defun kawacode--handle-handshake-response (data)
  "Handle handshake response from Muninn.
Argument DATA the response data containing caw ID."
  (let ((caw (alist-get 'caw data)))
    (if caw
        (progn
          (setq kawacode--caw caw)
          (kawacode-log-info "Received CAW ID from Muninn: %s" caw)
          (message "Connected to Kawa Code (CAW: %s)" caw)
          ;; Now initialize workspace
          (kawacode--init-workspace))
      (kawacode-log-error "Handshake response missing CAW ID: %s" data)
      (message "Handshake failed - no CAW ID received"))))

(defun kawacode--send-sync-setup ()
  "Send sync:setup to register for push notifications from Muninn.
Uses raw emit (not transmit) to match VSCode pattern."
  (when (and kawacode--ipc-process
             (eq (process-status kawacode--ipc-process) 'open)
             kawacode--caw)
    (let ((message (json-encode `((flow . "req")
                                  (domain . "code")
                                  (action . "sync:setup")
                                  (caw . ,kawacode--caw)))))
      (kawacode-log-info "Sending sync:setup")
      (process-send-string kawacode--ipc-process (concat message "\n")))))

(defun kawacode--handle-sync-setup-broadcast (_data)
  "Handle sync:setup broadcast from Muninn indicating sync completed."
  (kawacode-log-info "Received sync:setup broadcast, refreshing active file")
  (kawacode--refresh-active-file))

(defun kawacode--init-workspace ()
  "Initialize workspace."
  (kawacode-log-info "Workspace initialized")
  ;; Request temp directory from local service
  (kawacode--request-tmp-dir)
  ;; Register for sync push notifications
  (kawacode--send-sync-setup)
  ;; Send auth:info request after a short delay to ensure connection is ready
  (run-with-timer 0.1 nil #'kawacode--send-auth-info))

(defun kawacode--request-tmp-dir ()
  "Request temp directory from Muninn."
  (kawacode-log-info "Requesting temp directory from Muninn")
  (if (and kawacode--ipc-process
           (eq (process-status kawacode--ipc-process) 'open))
      (progn
        (kawacode--transmit "get-tmp-dir" kawacode--caw)
        (kawacode--setup-response-handler "code" "get-tmp-dir"))
    (kawacode-log-error "IPC process not ready for get-tmp-dir request")))

(defun kawacode--handle-get-tmp-dir-response (data)
  "Handle response from code:get-tmp-dir request.
Argument DATA the data received from Kawa Code application."
  (let ((tmp-dir (alist-get 'tmpDir data)))
    (when tmp-dir
      (setq kawacode--tmp-dir tmp-dir)
      (kawacode-log-info "Received temp directory from local service: %s" tmp-dir)
      ;; Update LSP blocklist with the actual temp directory
      (when (and (featurep 'lsp-mode) (boundp 'lsp-session-folders-blocklist))
        (add-to-list 'lsp-session-folders-blocklist tmp-dir)
        (kawacode-log-info "Added actual temp directory to LSP blocklist: %s" tmp-dir))
      ;; Update recentf exclude with the actual temp directory
      (when (and (featurep 'recentf) (boundp 'recentf-exclude))
        (add-to-list 'recentf-exclude
                     (concat "^" (regexp-quote (expand-file-name tmp-dir))))
        (kawacode-log-info "Added actual temp directory to recentf-exclude: %s" tmp-dir)))))

(defun kawacode--send-auth-info ()
  "Send auth:info request to the Kawa Code IPC."
  (kawacode-log-info "Sending auth:info request")
  (if (and kawacode--ipc-process
           (eq (process-status kawacode--ipc-process) 'open))
      (kawacode--transmit "auth:info" nil)
    (kawacode-log-error "IPC process not ready for auth:info request")))

(defun kawacode--check-connection-timeout ()
  "Check if the connection is stuck and handle timeout."
  (when (and kawacode--ipc-process
             (not kawacode--connected))
    (let ((status (process-status kawacode--ipc-process)))
      (when (eq status 'connect)
        (kawacode-log-error "Connection stuck, retrying")
        (delete-process kawacode--ipc-process)
        (setq kawacode--ipc-process nil)
        (run-with-timer 1.0 nil #'kawacode--connect-to-muninn)))))

(defun kawacode--fallback-handshake ()
  "Fallback handshake if sentinel doesn't fire properly."
  (when (and kawacode--ipc-process
             (eq (process-status kawacode--ipc-process) 'open)
             (not kawacode--caw))
    (kawacode-log-info "Using fallback handshake")
    (kawacode--send-handshake)))

(defun kawacode--force-cleanup ()
  "Force cleanup of all Kawa Code processes and state."
  (kawacode-log-info "Force cleaning up all processes")

  ;; Cancel any pending timers
  (when kawacode--update-timer
    (cancel-timer kawacode--update-timer)
    (setq kawacode--update-timer nil))
  (when kawacode--highlight-timer
    (cancel-timer kawacode--highlight-timer)
    (setq kawacode--highlight-timer nil))
  (when kawacode--selection-timer
    (cancel-timer kawacode--selection-timer)
    (setq kawacode--selection-timer nil))

  ;; Force delete IPC process
  (when kawacode--ipc-process
    (condition-case err
        (progn
          ;; Try to close the process gracefully first
          (when (eq (process-status kawacode--ipc-process) 'open)
            (process-send-eof kawacode--ipc-process))
          ;; Force delete the process
          (delete-process kawacode--ipc-process)
          (kawacode-log-info "Force deleted Muninn IPC process"))
      (error
       (kawacode-log-error "Error deleting IPC process: %s" err)))
    (setq kawacode--ipc-process nil))

  ;; Remove hooks
  (remove-hook 'after-save-hook #'kawacode--after-save-hook)
  (remove-hook 'post-command-hook #'kawacode--post-command-hook)

  ;; Reset all state
  (setq kawacode--connected nil
        kawacode--authenticated nil
        kawacode--active-buffer nil
        kawacode--active-project nil
        kawacode--caw nil
        kawacode--last-cursor-line nil
        kawacode--is-cycling nil)

  ;; Clear pending requests
  (clrhash kawacode--pending-requests)

  (kawacode-log-info "Force cleanup completed"))

(defun kawacode--send-disconnect-messages ()
  "Send disconnect message to Muninn."
  (kawacode-log-info "Sending disconnect messages")

  ;; Send disconnect message to Muninn
  (when (and kawacode--ipc-process
             (eq (process-status kawacode--ipc-process) 'open)
             kawacode--caw)
    (let ((message (json-encode `((flow . "req")
                                  (domain . "system")
                                  (action . "disconnect")
                                  (data . ((caw . ,kawacode--caw)))
                                  (caw . ,kawacode--caw)))))
      (kawacode-log-info "Sending disconnect to Muninn")
      (condition-case err
          (process-send-string kawacode--ipc-process (concat message "\n"))
        (error
         (kawacode-log-error "Failed to send disconnect to Muninn: %s" err))))))

;;; Cursor Tracking

(defun kawacode--selection-changed ()
  "Send cursor position and symbol context to Muninn."
  (setq kawacode--selection-timer nil)
  (condition-case nil
      (when (and kawacode--authenticated
                 kawacode--active-buffer
                 (buffer-live-p kawacode--active-buffer)
                 (eq (current-buffer) kawacode--active-buffer)
                 (buffer-file-name kawacode--active-buffer))
        (let* ((fpath (kawacode--cross-platform-path
                       (buffer-file-name kawacode--active-buffer)))
               (line-number (line-number-at-pos))
               (rel (ignore-errors
                      (when (fboundp 'which-function)
                        (which-function)))))
          (kawacode--transmit "context:select-lines"
                                   `((fpath . ,fpath)
                                     (selections . [])
                                     (lineNumber . ,line-number)
                                     ,@(when rel `((rel . ,rel)))
                                     (caw . ,kawacode--caw)))))
    (error nil)))

;;; Buffer Management

(defun kawacode--schedule-update ()
  "Schedule a debounced update."
  (when kawacode--update-timer
    (cancel-timer kawacode--update-timer))
  (setq kawacode--update-timer
        (run-with-timer kawacode-update-delay nil #'kawacode--update)))

(defun kawacode--update ()
  "Update Kawa Code for the current buffer."
  (setq kawacode--update-timer nil)
  (when (and kawacode--active-buffer
             (buffer-live-p kawacode--active-buffer)
             (buffer-file-name kawacode--active-buffer))
    (let ((filename (buffer-file-name kawacode--active-buffer)))
      (kawacode--transmit "file-saved"
                               `((fpath . ,(kawacode--cross-platform-path filename))
                                 (doc . ,(with-current-buffer kawacode--active-buffer
                                           (buffer-string)))
                                 (caw . ,kawacode--caw))
                               (lambda (response-data)
                                 (kawacode--handle-repo-active-path-response
                                  response-data filename))))))

;;; Hooks and Event Handling

(defun kawacode--after-save-hook ()
  "Hook function for `after-save-hook'."
  (kawacode--update))

(defun kawacode--post-command-hook ()
  "Hook function for `post-command-hook'."
  (let ((current-buffer (current-buffer)))
    ;; Buffer switch detection
    (when (and current-buffer
               (not (eq current-buffer kawacode--active-buffer)))
      (if (buffer-file-name current-buffer)
          ;; Only update active buffer if switching to a different file
          (let ((current-file (buffer-file-name current-buffer))
                (active-file (when kawacode--active-buffer
                               (buffer-file-name kawacode--active-buffer))))
            (unless (and kawacode--active-buffer active-file
                         (string= current-file active-file))
              ;; Skip updates during EDiff sessions to avoid triggering events on every hunk navigation
              (unless (or (string-prefix-p (expand-file-name kawacode--tmp-dir) current-file)
                          (bound-and-true-p ediff-this-buffer-ediff-sessions))
                ;; Different file or no active buffer, update and refresh
                (setq kawacode--active-buffer current-buffer)
                (setq kawacode--last-cursor-line nil)
                (kawacode--refresh-active-file))))))
    ;; Cursor line change detection (debounced context:select-lines)
    (when (and kawacode--active-buffer
               (eq current-buffer kawacode--active-buffer)
               (buffer-file-name current-buffer))
      (let ((current-line (line-number-at-pos)))
        (unless (eq current-line kawacode--last-cursor-line)
          (setq kawacode--last-cursor-line current-line)
          (when kawacode--selection-timer
            (cancel-timer kawacode--selection-timer))
          (setq kawacode--selection-timer
                (run-with-timer kawacode-selection-delay nil
                                #'kawacode--selection-changed)))))))

;;; Diff Block Cycling

(defun kawacode--cycle-block (direction)
  "Cycle through peer diff blocks with DIRECTION (1=next, -1=prev)."
  (when (and kawacode--authenticated
             kawacode--active-buffer
             (buffer-live-p kawacode--active-buffer)
             (buffer-file-name kawacode--active-buffer)
             kawacode--active-project)
    ;; Undo previous cycle if still cycling
    (when kawacode--is-cycling
      (with-current-buffer kawacode--active-buffer
        (let ((inhibit-modification-hooks t))
          (undo))))
    (let* ((fpath (kawacode--cross-platform-path
                   (buffer-file-name kawacode--active-buffer)))
           (origin (alist-get 'origin kawacode--active-project))
           (doc (with-current-buffer kawacode--active-buffer
                  (buffer-string)))
           (line (with-current-buffer kawacode--active-buffer
                   (line-number-at-pos))))
      (kawacode--transmit "cycle-block"
                               `((caw . ,kawacode--caw)
                                 (origin . ,origin)
                                 (fpath . ,fpath)
                                 (doc . ,doc)
                                 (line . ,line)
                                 (direction . ,direction))
                               #'kawacode--handle-cycle-block-response))))

(defun kawacode--handle-cycle-block-response (data)
  "Handle response from cycle-block request.
DATA contains block info with range and replaceLen."
  (when data
    (let* ((range (alist-get 'range data))
           (line (alist-get 'line range))
           (len (alist-get 'len range))
           (content-vec (alist-get 'content range))
           (replace-len (alist-get 'replaceLen data))
           (content (when content-vec
                      (mapconcat #'identity
                                 (if (vectorp content-vec)
                                     (append content-vec nil)
                                   content-vec)
                                 "\n"))))
      (when (and line kawacode--active-buffer
                 (buffer-live-p kawacode--active-buffer))
        (with-current-buffer kawacode--active-buffer
          (let ((inhibit-modification-hooks t))
            (save-excursion
              (cond
               ;; INSERT: replaceLen > 0 but len = 0
               ((and replace-len (> replace-len 0) (or (not len) (= len 0)))
                (with-suppressed-warnings ((interactive-only goto-line))
                  (goto-line line))
                (beginning-of-line)
                (insert content "\n"))
               ;; DELETE: replaceLen = 0 or nil
               ((or (not replace-len) (= replace-len 0))
                (with-suppressed-warnings ((interactive-only goto-line))
                  (goto-line line))
                (let ((start (line-beginning-position)))
                  (with-suppressed-warnings ((interactive-only goto-line))
                    (goto-line (+ line (or len 1))))
                  (delete-region start (line-beginning-position))))
               ;; REPLACE: both replaceLen and len > 0
               (t
                (with-suppressed-warnings ((interactive-only goto-line))
                  (goto-line line))
                (let ((start (line-beginning-position)))
                  (with-suppressed-warnings ((interactive-only goto-line))
                    (goto-line (+ line len)))
                  (delete-region start (line-beginning-position))
                  (goto-char start)
                  (insert content "\n")))))))
        (setq kawacode--is-cycling t)
        (kawacode--refresh-active-file)))))

(defun kawacode--after-change-hook (_beg _end _len)
  "Reset cycling state when buffer changes outside of cycling.
BEG, END, LEN are standard `after-change-functions' arguments."
  (unless inhibit-modification-hooks
    (setq kawacode--is-cycling nil)))

;;;###autoload
(defun kawacode-next-peer ()
  "Cycle to the next peer's diff block at the current position."
  (interactive)
  (kawacode--cycle-block 1))

;;;###autoload
(defun kawacode-prev-peer ()
  "Cycle to the previous peer's diff block at the current position."
  (interactive)
  (kawacode--cycle-block -1))

;;; Public API

;;;###autoload
(defun kawacode-refresh ()
  "Refresh Kawa Code data."
  (interactive)
  (kawacode--refresh-active-file))

;;;###autoload
(defun kawacode-clear-all-highlights ()
  "Clear all Kawa Code highlight from all buffers."
  (interactive)
  (kawacode--clear-all-highlights)
  (message "Cleared all highlights"))

;;;###autoload
(defun kawacode-auth-status ()
  "Show the current authentication status."
  (interactive)
  (if kawacode--authenticated
      (message "Authenticated as %s" (alist-get 'name kawacode--user))
    (message "Not authenticated")))

;;;###autoload
(defun kawacode-connection-status ()
  "Show the current connection status."
  (interactive)
  (message "Muninn connected: %s, CAW ID: %s, Authenticated: %s"
           (if (and kawacode--ipc-process
                    (eq (process-status kawacode--ipc-process) 'open)) "yes" "no")
           (or kawacode--caw "none")
           (if kawacode--authenticated "yes" "no")))

;;; Reinit on Emacs restart

;;;###autoload
(defun kawacode-reinit-faces ()
  "Force reinitialize hl-line faces."
  (interactive)
  (when (featurep 'hl-line)
    (setq kawacode--hl-line-faces nil) ; Clear existing faces
    (kawacode--init-hl-line-faces)
    (message "HL-line faces reinitialized")))

;;; Minor Mode

(defvar kawacode-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-]") #'kawacode-next-peer)
    (define-key map (kbd "C-c C-[") #'kawacode-prev-peer)
    map)
  "Keymap for `kawacode-mode'.")

;;;###autoload
(define-minor-mode kawacode-mode
  "Toggle Kawa Code mode.
Enable Kawa Code functionality for collaborative development."
  :init-value nil
  :global t
  :lighter (:eval kawacode--mode-line-string)
  :group 'kawacode
  :require 'kawacode
  (if kawacode-mode
      (kawacode--enable)
    (kawacode--disable)))

(defun kawacode--enable ()
  "Enable Kawa Code."
  (kawacode--init-config)
  (kawacode--init-store)
  (kawacode--init-event-handlers)
  (kawacode--init-ipc)
  (kawacode--init-highlight-faces)
  (add-hook 'after-save-hook #'kawacode--after-save-hook)
  (add-hook 'post-command-hook #'kawacode--post-command-hook)
  (add-hook 'buffer-list-update-hook #'kawacode--buffer-list-update-hook)
  (add-hook 'after-change-functions #'kawacode--after-change-hook)
  (add-hook 'kill-emacs-hook #'kawacode--cleanup-on-exit)
  ;; Set the current buffer as active if it has a file (like VSCode's activeTextEditor)
  (when (and (current-buffer) (buffer-file-name (current-buffer)))
    (setq kawacode--active-buffer (current-buffer)))
  ;; Add temp directory to LSP blocklist to prevent LSP from treating temp files as projects
  (when (and (featurep 'lsp-mode) (boundp 'lsp-session-folders-blocklist))
    (add-to-list 'lsp-session-folders-blocklist kawacode--tmp-dir)
    (kawacode-log-info "Added temp directory to LSP blocklist: %s" kawacode--tmp-dir))
  ;; Add temp directory to recentf exclude list to prevent temp files from appearing in recent files
  (when (and (featurep 'recentf) (boundp 'recentf-exclude))
    (add-to-list 'recentf-exclude
                 (concat "^" (regexp-quote (expand-file-name kawacode--tmp-dir))))
    (kawacode-log-info "Added temp directory to recentf-exclude: %s" kawacode--tmp-dir))
  (kawacode-log-info "Kawa Code enabled"))

(defun kawacode--disable ()
  "Disable Kawa Code."
  (kawacode-log-info "Disabling and disconnecting")

  ;; Reset mode-line
  (setq kawacode--mode-line-string " Kawa")

  ;; Remove hooks
  (remove-hook 'after-save-hook #'kawacode--after-save-hook)
  (remove-hook 'post-command-hook #'kawacode--post-command-hook)
  (remove-hook 'buffer-list-update-hook #'kawacode--buffer-list-update-hook)
  (remove-hook 'kill-emacs-hook #'kawacode--cleanup-on-exit)
  (remove-hook 'after-change-functions #'kawacode--after-change-hook)

  ;; Clear all highlights
  (kawacode--clear-all-highlights)

  ;; Send disconnect messages before closing connections
  (kawacode--send-disconnect-messages)

  ;; Use force cleanup to ensure all processes are properly deleted
  (kawacode--force-cleanup)

  ;; Clear the store
  (kawacode--clear-store)

  (kawacode-log-info "Kawa Code disabled"))

;;; Cleanup on Emacs exit

(defun kawacode--buffer-list-update-hook ()
  "Hook function to detect when buffers are displayed."
  (let ((current-buffer (current-buffer)))
    (when (and current-buffer
               (buffer-file-name current-buffer)
               (not (eq current-buffer kawacode--active-buffer)))
      (let ((current-file (buffer-file-name current-buffer))
            (active-file (when kawacode--active-buffer
                           (buffer-file-name kawacode--active-buffer))))
        (unless (and kawacode--active-buffer active-file
                     (string= current-file active-file))
          ;; Skip updates during EDiff sessions to avoid triggering events on every hunk navigation
          ;; Check if the file is in the temp directory (peer file) or if we're in an EDiff session
          (unless (or (string-prefix-p (expand-file-name kawacode--tmp-dir) current-file)
                      (bound-and-true-p ediff-this-buffer-ediff-sessions))
            ;; Different file or no active buffer, update and refresh
            (setq kawacode--active-buffer current-buffer)
            (kawacode--refresh-active-file)))))))

(defun kawacode--cleanup-on-exit ()
  "Cleanup Kawa Code when Emacs is about to exit."
  (when kawacode-mode
    (kawacode-log-info "Emacs exiting, disabling kawacode-mode")
    ;; Disable the mode which will trigger proper cleanup including disconnect messages
    (kawacode-mode -1)))

;; Note: kill-emacs-hook is added in kawacode--enable, not at load time

;;; Provide

(provide 'kawacode)
;;; kawacode.el ends here
