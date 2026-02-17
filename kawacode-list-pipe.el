;;; kawacode-list-pipe.el --- List-Pipes for Kawa Code -*- lexical-binding: t -*-

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

;;{{{
;; Code:

;;; Required Libraries
;; For 'kawacode-pipe-default-newline-delim'
(require 'kawacode-pipe)

;;{{{
;;; Customization Variables

;;; Code:

(defvar kawacode-list-pipe-debug nil
  "Whether or not to print debugging messages.")

(defvar kawacode-list-pipe-default-underflow-handler (lambda () (error "Underflow")))

;;}}}

;;{{{
;;; List Pipe Functions & Macros

(defun kawacode-list-pipe-make-list-pipe ()
  "Make a list-based pipe."
  nil)

;;{{{
;;; Peeking Functions

(defun kawacode-list-pipe-peek (list-pipe)
  "Return the next character to be read from pipe, but don't modify the pipe.
Argument LIST-PIPE the unix pipe."
  (car list-pipe))

(defun kawacode-list-pipe-ln (list-pipe)
  "Return the next line to be read from pipe, but don't modify the pipe.
Argument LIST-PIPE the unix pipe."
  (let ((line (kawacode-list-pipe-read-ln! list-pipe)))
    (dolist (char (reverse line))
      ;; unread the character
      (kawacode-list-pipe-read! list-pipe char))))

(defun kawacode-list-pipe-sexp (list-pipe)
  "Return the next sexp to be read from pipe, but don't modify the pipe.
Argument LIST-PIPE the unix pipe."
  (let ((sexp (kawacode-list-pipe-read-sexp! list-pipe)))
    (dolist (char (reverse sexp))
      ;; unread the character
      (kawacode-list-pipe-read! list-pipe char))))

(defun kawacode-list-pipe-peek-all (list-pipe)
  "Return a string containing all of `LIST-PIPE's currently available input.
But don't modify `list-pipe'."
  (concat list-pipe))

;;}}}

;;{{{
;;; Reading Macros

(defmacro kawacode-list-pipe-read! (list-pipe &optional unread underflow-handler)
  "Read a character from `LIST-PIPE'.
Optional argument UNREAD the character or string.
Optional argument UNDERFLOW-HANDLER handler in case of underflow"
  `(progn
     (unless (listp ,list-pipe)
       (error "List-pipe must be a list-pipe"))
     (let ((unread2
            (cond ((and (stringp ,unread)
                        (equal (length ,unread) 1))
                   (string-to-char ,unread))
                  ((or (not ,unread)
                       (characterp ,unread))
                   ,unread)
                  (t
                   (error "Unread must be a character or a string
     containing a single character"))))  )
       (if unread2
           (push unread2 ,list-pipe)
         (or (pop ,list-pipe)
             (funcall (or ,underflow-handler
                          kawacode-list-pipe-default-underflow-handler)))))))

(defmacro kawacode-list-pipe-read-ln! (list-pipe)
  "Read a line from `LIST-PIPE'."
  `(let ((chars '()))
     (while (not
             (funcall (lambda (chars)
                        (string-suffix-p kawacode-pipe-default-newline-delim
                                         (concat chars)))
                      (setf chars (append chars
                                          (list (kawacode-list-pipe-read! ,list-pipe)))))))
     (concat chars)))

(defmacro kawacode-list-pipe-read-sexp! (list-pipe)
  "Read an sexp from `LIST-PIPE'."
  `(read (lambda (&optional unread)
           (kawacode-list-pipe-read! ,list-pipe unread))))


(defmacro kawacode-list-pipe-read-all! (list-pipe)
  "Read all available characters from `LIST-PIPE'."
  `(prog1
       (concat ,list-pipe)
     (setf ,list-pipe nil)))

;;}}}

;;{{{
;;; Writing Macros

(defmacro kawacode-list-pipe-write! (list-pipe str-or-char)
  "Write a character to `LIST-PIPE'.
Argument STR-OR-CHAR char or string."
  `(progn
     (unless (listp ,list-pipe)
       (error "List-pipe must be a list-pipe"))
     (cond ((characterp ,str-or-char)
            (setf ,list-pipe (append ,list-pipe ,str-or-char)))
           ((stringp ,str-or-char)
            (setf ,list-pipe (append ,list-pipe (string-to-list ,str-or-char))))
           (t
            (error "Str-or-char must be a string or character")))))

(defmacro kawacode-list-pipe-write-ln! (list-pipe &optional str-or-char)
  "Write `string' followed by a new line delimiter to `pipe'.
Argument LIST-PIPE the unix pipe.
Optional argument STR-OR-CHAR char or string to write."
  `(kawacode-list-pipe-write! ,list-pipe (concat ,(or str-or-char "")
                                                      kawacode-pipe-default-newline-delim)))

(defmacro kawacode-list-pipe-write-sexp! (list-pipe sexp)
  "Write `string' followed by a new line delimiter to `pipe'.
Argument LIST-PIPE the unix pipe.
Argument SEXP the symbolic expression to write."
  `(progn (kawacode-list-pipe-write! ,list-pipe (prin1-to-string ,sexp))
          (kawacode-list-pipe-write! ,list-pipe " ")))

;;}}}

;;}}}

;;}}}

(provide 'kawacode-list-pipe)
;;; kawacode-list-pipe.el ends here
