;;; code-awareness-pipe.el --- Pipes for Code Awareness -*- lexical-binding: t -*-

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

;; Definitions
;; -----------
;;
;; An (Emacs) pipe is a buffer together with several operations
;; (code-awareness-pipe-read!, code-awareness-pipe-write!, etc) and has the following properties:
;;
;;     buf_size: the size of the pipe's buffer
;;
;;     num_writ: the number of characters written to the buffer that
;;     have yet to be read
;;
;;     num_read: the number of characters read from the buffer that
;;     have yet to be written over
;;
;;     read_pos: the position of the next character to be read; it
;;     must be an integer from 0 to buf_size-1
;;
;;     write_pos: the position of the next character to be written; it
;;     must be an integer from 0 to buf_size-1
;;
;; Buffer State
;; ------------
;;
;; Each position in the buffer may be in one of two states; that is,
;; at any point in time, each position in the buffer has either been
;; read from but not yet written over, or written over but not yet
;; read from.  Thus the following invariant holds:
;;
;;     num_read + num_writ = buf_size
;;
;; Initially, all values in the buffer are said to be read from but
;; not written to.
;;
;; Changes in Buffer State
;; -----------------------
;;
;; The state of the buffer can be changed in precisely two ways: by
;; writing characters to it or by reading characters from it.
;;
;; Initially, read_pos = write_pos = num_write = 0, and num_read =
;; buf_size.
;;
;; If c chars are to be read and c > num_writ, then an underflow
;; occurs.  Otherwise, num_read is incremented by c, num_writ is
;; decremented by c, and read_pos is incremented by c modulo buf_size.
;;
;; If a character is unread and 1 + num_writ > buf_size, then an
;; overflow occurs.  Otherwise, num_read is decremented by 1, num_writ
;; is incremented by 1, and read_pos is decremented by one modulo
;; buf_size.
;;
;; If c chars are to be written over and c + num_writ > buf_size, then
;; an overflow occurs.  Otherwise, num_writ is incremented by c,
;; num_read is decremented by c, and write_pos is incremented by c
;; modulo buf_size.
;;
;; Example
;; -------
;;
;; The columns below show the state of the buffer and its
;; corresponding variables immediately after the given event occurs.
;;; Keywords: code awareness, collaboration, development, convenience tools
;; Homepage: https://github.com/CodeAwareness/ca.emacs;
;; |-----------------+--------+----------+-----------+----------+----------|
;; | event           | buffer | num_writ | write_pos | num_read | read_pos |
;; |-----------------+--------+----------+-----------+----------+----------|
;; | initialization  | rrrrr  |        0 |         0 |        5 |        0 |
;; | writing 1 char  | wrrrr  |        1 |         1 |        4 |        0 |
;; | writing 1 char  | wwrrr  |        2 |         2 |        3 |        0 |
;; | reading 1 char  | rwrrr  |        1 |         2 |        4 |        1 |
;; | writing 3 chars | rwwww  |        4 |         0 |        1 |        1 |
;; |-----------------+--------+----------+-----------+----------+----------|
;;

;;{{{
;;; Code:

;; For 'cl-defun'
(require 'cl-lib)
;; For 'string-empty-p'
(require 'subr-x)

;;{{{
;;; Customizable Variables

(defvar code-awareness-pipe-debug nil "Set to true to log debugging info in the
*Messages* buffer.")

(defvar code-awareness-pipe-default-buf-size 65536 "The default buffer size for pipes.")

(defvar code-awareness-pipe-default-newline-delim "\n" "This should usually be set
to the default newline string used by the OS.")

;;}}}

;;{{{
;;; First-class variables

(defun code-awareness-pipe-make-var (val)
  (list val))

(defalias 'code-awareness-pipe-var-ref 'car)

(defmacro code-awareness-pipe-set-var! (var new-val)
  `(setf (car ,var) ,new-val))

(defmacro code-awareness-pipe-inc-var! (var amt)
  `(setf (car ,var) (+ (car ,var) ,amt)))

(defmacro code-awareness-pipe-dec-var! (var amt)
  `(setf (car ,var) (- (car ,var) ,amt)))

(defmacro code-awareness-pipe-inc-var-mod-n! (var amt n)
  `(setf (car ,var) (mod (+ (car ,var) ,amt) ,n)))

(defmacro code-awareness-pipe-dec-var-mod-n! (var amt n)
  `(setf (car ,var) (mod (- (car ,var) ,amt) ,n)))

;;}}}

;;{{{
;;; Utility functions

(defun code-awareness-pipe-memcpy! (src dest dest-offset)
  "Copy `src' to `dest' starting at offset `dest-offset'; if the
length of `src' plus `dest-offset' is greater than the length of
`dest', then the writing wraps.  If `src' is longer then `dest'
then a buffer overflow error is thrown.  If `dest-offset' is out
of range, then an invalid offset error is thrown."
  (unless (<= (length src) (length dest))
    (error "Buffer overflow"))
  (unless (< dest-offset (length dest))
    (error "Invalid offset '%s'" dest-offset))

  (if (<= (+ dest-offset (length src)) (length dest))
      (store-substring dest dest-offset src)
    (progn (store-substring dest dest-offset (substring src 0 (- (length dest)
								 dest-offset)))
	   (store-substring dest 0 (substring src (- (length dest)
						     dest-offset))))))

(defun code-awareness-pipe-clockwise-substring (str start end)
  "Return the clockwise-substring of a non-empty string `str'
that starts at `start' and ends at `end'.  Imagine that the
characters of `str' are positioned around a 0-based clock with
n-1 numbers, where n is the length of `str'.  For example,
suppose `str' is 'abcd'.  Then the characters of `str' may be
positioned around a 0-based clock with 4 numbers as shown below.

       _a_
      / 0 \
    d|3   1|b
     \\_2_/
        c

The following table gives some examples of clockwise substrings.

| start | end | clockwise-substring |
| 1     | 3   | 'bc'                |
| 3     | 1   | 'da'                |
| 1     | 1   | 'bcda'              |
| 3     | 3   | 'dabc'              |
"
  (when (string-empty-p str)
    (error "str must not be empty"))

  (cond ((>= start end)
	 (concat (substring str start)
		 (substring str 0 end)))
	(t
	 (substring str start end))))

;;}}}

;;{{{
;;; Debugging/logging functions

(defun code-awareness-pipe-debug (fmt-str &rest args)
  (when code-awareness-pipe-debug
    (apply 'message (concat "code-awareness-pipe-debug: " fmt-str "\n") args)))

;;}}}

;;{{{
;;; Private Macros and Functions

(defmacro code-awareness-pipe-with-pipe (pipe &rest body)
  "Evaluate `body' in the environment of `pipe'."
  `(let* ((env (funcall ,pipe 'env))
	  (num-writ (cdr (assoc 'num-writ env)))
	  (write-pos (cdr (assoc 'write-pos env)))
	  (num-read (cdr (assoc 'num-read env)))
	  (read-pos (cdr (assoc 'read-pos env)))
	  (buf (cdr (assoc 'buf env)))
	  (underflow-handler (cdr (assoc 'underflow-handler env))))
     ,@body))

;;}}}

;;{{{
;;; Public API

;;;###autoload
(cl-defun code-awareness-pipe-make-pipe (&optional (buf-size code-awareness-pipe-default-buf-size)
			     (underflow-handler
			      (lambda ()
				(error "Buffer underflow"))))
  "Create a new pipe."
  (let* ( ;; The environment
	 (env `((num-writ . ,(code-awareness-pipe-make-var 0))
		(write-pos . ,(code-awareness-pipe-make-var 0))
		(num-read . ,(code-awareness-pipe-make-var buf-size))
		(read-pos . ,(code-awareness-pipe-make-var 0))
		(buf . ,(make-string buf-size 0))
		(underflow-handler .,underflow-handler))))
    (lambda (fn-or-var)
      (case fn-or-var
	((env)
	 env)
	(t
	 (error "Invalid arguments"))))))

;;{{{
;;; Accessors

(defun code-awareness-pipe-input-stream (pipe)
  "Return the pipe's input stream.  See Ouput Streams in section
18.2 of the ELISP reference manual."
  (lambda (&optional unread)
    (code-awareness-pipe-read! pipe unread)))

(defun code-awareness-pipe-output-stream (pipe)
  "Return the pipe's output stream.  See Ouput Streams in section
18.4 of the ELISP reference manual."
  (lambda (char)
    (code-awareness-pipe-write! pipe char)))

;;}}}

;;{{{
;;; Peeking Functions

(defun code-awareness-pipe-peek (pipe)
  "Return the next character to be read from pipe, but don't
modify the pipe."
  (let ((char (code-awareness-pipe-read! pipe)))
    ;; Unread char from pipe
    (code-awareness-pipe-read! pipe char)
    char))

(defun code-awareness-pipe-peek-ln (pipe)
  "Return the next line to be read from pipe, but don't modify
  the pipe."
  (let ((line (code-awareness-pipe-read-ln! pipe)))
    (dolist (char (reverse line))
      ;; unread the character
      (code-awareness-pipe-read! pipe char))))

(defun code-awareness-pipe-peek-sexp (pipe)
  "Return the next sexp to be read from pipe, but don't modify
  the pipe."
  (let ((sexp (code-awareness-pipe-read-sexp! pipe)))
    (dolist (char (reverse sexp))
      ;; unread the character
      (code-awareness-pipe-read! pipe char))))

(defun code-awareness-pipe-peek-all (pipe)
  "Return a string containing all of the pipe's currently
  available input, but don't modify the pipe."
  (code-awareness-pipe-with-pipe
   pipe
   (let* ((buf-size (length buf))
	  ;; after-last-pos -- the position just after the last
	  ;; character to be read
	  (after-last-pos (mod (+ (code-awareness-pipe-var-ref read-pos)
				  (code-awareness-pipe-var-ref num-writ))
			       buf-size)))
     (if (> (code-awareness-pipe-var-ref num-writ) 0)
	 ;; The pipe's available input can only be modeled as a
	 ;; clockwise substring when the buffer is non-empty.
	 (code-awareness-pipe-clockwise-substring buf
				   (code-awareness-pipe-var-ref read-pos)
				   after-last-pos)
       ""))))

;;}}}

;;{{{
;;; Reading Functions

(defun code-awareness-pipe-read! (pipe &optional unread)
  "Reads a character from `pipe' if `unread' is nil.
Otherwise it unreads the character `unread' from `pipe'."
  ;; This function must support two kinds of calls:
  ;;
  ;; • When it is called with no arguments, it should return the next
  ;;   character.
  ;;
  ;; • When it is called with one argument (always a character), it
  ;;   should save the argument and arrange to return the argument on
  ;;   the next call.  This is called "unreading" the character; it
  ;;   happens when the Lisp reader reads one character too many and
  ;;   wants to put it back where it came from.  In this case, it
  ;;   makes no difference what value is returned.
  (code-awareness-pipe-with-pipe
   pipe
   (let ((buf-size (length buf)))
     (cond (unread (code-awareness-pipe-debug "unreading %s" unread)
		   (if (= (code-awareness-pipe-var-ref num-writ) buf-size)
		       (progn (error "Buffer overflow (unread)"))
		     (prog1 unread (code-awareness-pipe-inc-var! num-read  -1)
			    (code-awareness-pipe-inc-var! num-writ 1)
			    ;; unreading does not alter write-pos
			    (code-awareness-pipe-dec-var-mod-n! read-pos 1 buf-size))))
	   ((= (code-awareness-pipe-var-ref num-read) buf-size)
	    (code-awareness-pipe-debug "handling undeflow")
	    (code-awareness-pipe-debug "got input %s" (funcall underflow-handler))
	    (code-awareness-pipe-read! pipe))
	   (t (let ((res (prog1 (aref buf (code-awareness-pipe-var-ref read-pos))
			   (code-awareness-pipe-inc-var! num-read  1)
			   (code-awareness-pipe-inc-var! num-writ -1)
			   ;; reading does not alter write-pos
			   (code-awareness-pipe-inc-var-mod-n! read-pos 1 buf-size))))
		(code-awareness-pipe-debug "read %c" res)
		res))))))

(defun code-awareness-pipe-read-ln! (pipe)
  "Read a line from `pipe'."
  (let ((chars '()))
    (while (not (funcall (lambda (chars)
			   (string-suffix-p code-awareness-pipe-default-newline-delim
					    (concat chars)))
			 (setq chars (append chars (list (code-awareness-pipe-read! pipe)))))))
    (concat chars)))

(defun code-awareness-pipe-read-sexp! (pipe)
  "Read an sexp from `pipe'."
  (read (lambda (&optional unread)
	  (code-awareness-pipe-read! pipe unread))))

(defun code-awareness-pipe-read-all! (pipe)
  "Reads all currently available characters from `pipe' into a
string."
  (code-awareness-pipe-with-pipe
   pipe
   (let* ((buf-size (length buf))
	  (after-last-pos (mod (+ (code-awareness-pipe-var-ref read-pos)
				  (code-awareness-pipe-var-ref num-writ))
			       buf-size)))
     ;; after-last-pos -- the position just after the last
     ;; character to be read
     (prog1
	 (if (< after-last-pos (code-awareness-pipe-var-ref read-pos))
	     (concat (substring-no-properties buf (code-awareness-pipe-var-ref read-pos))
		     (substring-no-properties buf 0 after-last-pos))
	   (substring-no-properties buf (code-awareness-pipe-var-ref read-pos) after-last-pos))
       (code-awareness-pipe-inc-var-mod-n! read-pos (code-awareness-pipe-var-ref num-writ) buf-size)
       (code-awareness-pipe-set-var! num-read buf-size)
       ;; reading does not alter the write position
       (code-awareness-pipe-set-var! num-writ 0)))))
;;}}}

;;{{{
;;; Writing Functions

(defun code-awareness-pipe-write! (pipe char-or-str)
  "Writes the `char-or-str' to `pipe'."
  (let ((str (if (characterp char-or-str)
		 (char-to-string char-or-str)
	       char-or-str)))
   (code-awareness-pipe-with-pipe
    pipe
    (let ((buf-size (length buf)))
      (cond ((> (+ (code-awareness-pipe-var-ref num-writ) (length str)) buf-size)
	     (error "Buffer overflow"))
	    (t
	     (code-awareness-pipe-debug "wrote '%s'" str)
	     (prog1 (code-awareness-pipe-memcpy! str buf (code-awareness-pipe-var-ref write-pos))
	       (code-awareness-pipe-inc-var! num-writ (length str))
	       (code-awareness-pipe-dec-var! num-read (length str))
	       ;; writing does not alter the read position
	       (code-awareness-pipe-inc-var-mod-n! write-pos (length str) buf-size))))))))

(defun code-awareness-pipe-write-ln! (pipe &optional string)
  "Write `string' followed by a new line delimiter to `pipe'."
  (code-awareness-pipe-write! pipe (concat (or string "") code-awareness-pipe-default-newline-delim)))

(defun code-awareness-pipe-write-sexp! (pipe sexp)
  "Write `string' followed by a new line delimiter to `pipe'."
  (prin1 sexp (lambda (c) (code-awareness-pipe-write! pipe c)))
  (code-awareness-pipe-write! pipe " "))

;;}}}

;;}}}

(provide 'code-awareness-pipe)
;;; code-awareness-pipe.el ends here
