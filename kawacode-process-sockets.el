;;; kawacode-process-sockets.el --- Process sockets for Kawa Code  -*- lexical-binding: t -*-

;; This is an embedded version of process-sockets functionality
;; to avoid external dependencies. Original: https://github.com/IkeLewis/process-sockets

;; Copyright (C) 2018 Isaac Lewis

;; Author: Isaac Lewis <isaac.b.lewis@gmail.com>
;; Maintainer Isaac Lewis <isaac.b.lewis@gmail.com>
;; Homepage: https://github.com/IkeLewis/process-sockets
;; Version: 1.0.0
;; Keywords: comm

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; API Functions

;;; Constructor(s)

;; For 'cl-defun'
(require 'cl-lib)

;;; Code:

;;;###autoload
(cl-defun kawacode-process-socket-create (process &optional (pipe-buf-size kawacode-pipe-default-buf-size))
  "Create a socket for communicating with PROCESS.
Optional argument PIPE-BUF-SIZE buffer size for pipe read-write."
  (unless (>= emacs-major-version 27)
    (error "Emacs 27+ is required"))
  (let* ((process process)
         (sock-mutex (make-mutex))
         (sock-output-ready nil)
         (sock-cv-output-ready
          (make-condition-variable sock-mutex "sock-cv-output-ready"))
         (input-pipe (kawacode-pipe-make-pipe
                      pipe-buf-size
                      (lambda ()
                        (if (equal (current-thread)
                                   (process-thread process))
                            (catch 'done
                              (while t
                                (with-mutex sock-mutex
                                  (when sock-output-ready
                                    (setq sock-output-ready nil)
                                    (throw 'done t)))
                                (sleep-for 0.001)))
                          (with-mutex sock-mutex
                            (while (not sock-output-ready)
                              (condition-wait sock-cv-output-ready))
                            (setq sock-output-ready nil))))))
         (output-pipe (kawacode-pipe-make-pipe pipe-buf-size))
         (auto-flush nil))
    ;; Write output from the process to the socket's input stream.
    (set-process-filter process (lambda (_process string)
                                  (with-mutex sock-mutex
                                    (kawacode-pipe-write! input-pipe string)
                                    (setq sock-output-ready t)
                                    (condition-notify sock-cv-output-ready t))))
    (lambda (fn-or-var &rest args)
      (cl-case fn-or-var
            ;; Accessors
            ((process)
             process)
            ((input-pipe)
             input-pipe)
            ((output-pipe)
             output-pipe)
            ((auto-flush)
             auto-flush)
            ;; Mutators
            ((set-auto-flush!)
             (setf auto-flush (car args)))
            (t
             (error "Invalid arguments"))))))

;;; Accessors

(defun kawacode-ps-input-stream (ps)
  "Call the pipe-input-stream with PS."
  (kawacode-pipe-input-stream (funcall ps 'input-pipe)))

(defun kawacode-ps-output-stream (ps)
  "Call the pipe-output-stream with PS."
  (kawacode-pipe-output-stream (funcall ps 'output-pipe)))

(defun kawacode-ps-auto-flush (ps)
  "Call the auto-flush with PS."
  (funcall ps 'auto-flush))

;;; Mutators

(defun kawacode-ps-set-auto-flush! (ps val)
  "Call set-auto-flush for PS with VAL."
  (funcall ps 'set-auto-flush! val))

;;; Reading Functions

(defun kawacode-ps-read! (ps)
  "Call the pipe-read for PS."
  (kawacode-pipe-read! (funcall ps 'input-pipe)))

(defun kawacode-ps-read-ln! (ps)
  "Call the pipe-read-ln for PS."
  (kawacode-pipe-read-ln! (funcall ps 'input-pipe)))

(defun kawacode-ps-read-all! (ps)
  "Call the pipe-read-all for PS."
  (kawacode-pipe-read-all! (funcall ps 'input-pipe)))

(defun kawacode-ps-read-sexp! (ps)
  "Call the pipe-read-sexp for PS."
  (kawacode-pipe-read-sexp! (funcall ps 'input-pipe)))

;;; Writing Functions

(defun kawacode-ps-write! (ps char-or-str)
  "Write char or string to PS.
Argument CHAR-OR-STR char or string to write."
  (kawacode-pipe-write! (funcall ps 'output-pipe) char-or-str)
  (when (kawacode-ps-auto-flush ps)
    (kawacode-ps-flush! ps)))

(defun kawacode-ps-write-ln! (ps &optional char-or-str)
  "Write char or string to PS using write-ln.
Optional argument CHAR-OR-STR char or string to write."
  (kawacode-pipe-write-ln! (funcall ps 'output-pipe) char-or-str)
  (when (kawacode-ps-auto-flush ps)
    (kawacode-ps-flush! ps)))

(defun kawacode-ps-write-sexp! (ps sexp)
  "Write char or string to PS using SEXP."
  (kawacode-pipe-write-sexp! (funcall ps 'output-pipe) sexp)
  (when (kawacode-ps-auto-flush ps)
    (kawacode-ps-flush! ps)))

(defun kawacode-ps-flush! (ps)
  "Flush the pipe stream.
Argument PS the pipe stream."
  (process-send-string (funcall ps 'process)
                       (kawacode-pipe-read-all! (funcall ps 'output-pipe))))

;;; Misc Functions

(defun kawacode-ps-close! (ps)
  "Close the pipe stream.
Argument PS the pipe stream."
  (kawacode-ps-flush! ps)
  (delete-process (funcall ps 'process)))

(provide 'kawacode-process-sockets)
;;; kawacode-process-sockets.el ends here
