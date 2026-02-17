;;; kawacode-pipe.el --- Pipes for Kawa Code -*- lexical-binding: t -*-

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
;; (kawacode-pipe-read!, kawacode-pipe-write!, etc) and has
;; the following properties:
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
;;; Keywords: Kawa Code, collaboration, development, convenience tools
;; Homepage: https://github.com/codeawareness/kawa.emacs
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

(defvar kawacode-pipe-debug nil
  "Non-nil means log debugging info in the *Messages* buffer.")

(defvar kawacode-pipe-default-buf-size 65536 "The default buffer size for pipes.")

(defvar kawacode-pipe-default-newline-delim "\n"
  "Default newline delimiter string used by the OS.")

;;}}}

;;{{{
;;; First-class variables

(defun kawacode-pipe-make-var (val)
  "Create a first-class variable with value VAL."
  (list val))

(defalias 'kawacode-pipe-var-ref #'car)

(defmacro kawacode-pipe-set-var! (var new-val)
  "Set first-class variable VAR to NEW-VAL."
  `(setf (car ,var) ,new-val))

(defmacro kawacode-pipe-inc-var! (var amt)
  "Increment first-class variable VAR by AMT."
  `(setf (car ,var) (+ (car ,var) ,amt)))

(defmacro kawacode-pipe-dec-var! (var amt)
  "Decrement first-class variable VAR by AMT."
  `(setf (car ,var) (- (car ,var) ,amt)))

(defmacro kawacode-pipe-inc-var-mod-n! (var amt n)
  "Increment first-class variable VAR by AMT modulo N."
  `(setf (car ,var) (mod (+ (car ,var) ,amt) ,n)))

(defmacro kawacode-pipe-dec-var-mod-n! (var amt n)
  "Decrement first-class variable VAR by AMT modulo N."
  `(setf (car ,var) (mod (- (car ,var) ,amt) ,n)))

;;}}}

;;{{{
;;; Utility functions

(defun kawacode-pipe-memcpy! (src dest dest-offset)
  "Copy SRC to DEST starting at offset DEST-OFFSET.
If the length of SRC plus DEST-OFFSET is greater than the length of
DEST, then the writing wraps.  If SRC is longer than DEST, then a
buffer overflow error is thrown.  If DEST-OFFSET is out of range,
then an invalid offset error is thrown."
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

(defun kawacode-pipe-clockwise-substring (str start end)
  "Return the clockwise-substring of STR from START to END.
STR must be a non-empty string.  Imagine that the characters of STR
are positioned around a 0-based clock with n-1 numbers, where n is
the length of STR.  For example, suppose STR is \"abcd\".  Then the
characters of STR may be positioned around a 0-based clock with 4
numbers as shown below.

       _a_
      / 0 \
    d|3   1|b
     \\_2_/
        c

The following table gives some examples of clockwise substrings.

| start | end | clockwise-substring |
| 1     | 3   | \"bc\"                |
| 3     | 1   | \"da\"                |
| 1     | 1   | \"bcda\"              |
| 3     | 3   | \"dabc\"              |"
  (when (string-empty-p str)
    (error "STR must not be empty"))

  (cond ((>= start end)
	 (concat (substring str start)
		 (substring str 0 end)))
	(t
	 (substring str start end))))

;;}}}

;;{{{
;;; Debugging/logging functions

(defun kawacode-pipe-debug (fmt-str &rest args)
  "Log debugging message using FMT-STR and ARGS if debugging is enabled."
  (when kawacode-pipe-debug
    (apply #'message (concat "kawacode-pipe-debug: " fmt-str "\n") args)))

;;}}}

;;{{{
;;; Private Macros and Functions

(defmacro kawacode-pipe-with-pipe (pipe &rest body)
  "Evaluate BODY in the environment of PIPE."
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
(cl-defun kawacode-pipe-make-pipe (&optional (buf-size kawacode-pipe-default-buf-size)
			     (underflow-handler
			      (lambda ()
				(error "Buffer underflow"))))
  "Create a new pipe with buffer size BUF-SIZE and UNDERFLOW-HANDLER."
  (let* ( ;; The environment
	 (env `((num-writ . ,(kawacode-pipe-make-var 0))
		(write-pos . ,(kawacode-pipe-make-var 0))
		(num-read . ,(kawacode-pipe-make-var buf-size))
		(read-pos . ,(kawacode-pipe-make-var 0))
		(buf . ,(make-string buf-size 0))
		(underflow-handler .,underflow-handler))))
    (lambda (fn-or-var)
      (cl-case fn-or-var
	((env)
	 env)
	(t
	 (error "Invalid arguments"))))))

;;{{{
;;; Accessors

(defun kawacode-pipe-input-stream (pipe)
  "Return PIPE's input stream.
See Ouput Streams in section 18.2 of the ELISP reference manual."
  (lambda (&optional unread)
    (kawacode-pipe-read! pipe unread)))

(defun kawacode-pipe-output-stream (pipe)
  "Return PIPE's output stream.
See Ouput Streams in section 18.4 of the ELISP reference manual."
  (lambda (char)
    (kawacode-pipe-write! pipe char)))

;;}}}

;;{{{
;;; Peeking Functions

(defun kawacode-pipe-peek (pipe)
  "Return the next character to be read from PIPE.
The PIPE is not modified."
  (let ((char (kawacode-pipe-read! pipe)))
    ;; Unread char from pipe
    (kawacode-pipe-read! pipe char)
    char))

(defun kawacode-pipe-peek-ln (pipe)
  "Return the next line to be read from PIPE.
The PIPE is not modified."
  (let ((line (kawacode-pipe-read-ln! pipe)))
    (dolist (char (reverse line))
      ;; unread the character
      (kawacode-pipe-read! pipe char))))

(defun kawacode-pipe-peek-sexp (pipe)
  "Return the next sexp to be read from PIPE.
The PIPE is not modified."
  (let ((sexp (kawacode-pipe-read-sexp! pipe)))
    (dolist (char (reverse sexp))
      ;; unread the character
      (kawacode-pipe-read! pipe char))))

(defun kawacode-pipe-peek-all (pipe)
  "Return a string containing all of PIPE's currently available input.
The PIPE is not modified."
  (kawacode-pipe-with-pipe
   pipe
   (ignore write-pos num-read underflow-handler)
   (let* ((buf-size (length buf))
	  ;; after-last-pos -- the position just after the last
	  ;; character to be read
	  (after-last-pos (mod (+ (kawacode-pipe-var-ref read-pos)
				  (kawacode-pipe-var-ref num-writ))
			       buf-size)))
     (if (> (kawacode-pipe-var-ref num-writ) 0)
	 ;; The pipe's available input can only be modeled as a
	 ;; clockwise substring when the buffer is non-empty.
	 (kawacode-pipe-clockwise-substring buf
				   (kawacode-pipe-var-ref read-pos)
				   after-last-pos)
       ""))))

;;}}}

;;{{{
;;; Reading Functions

(defun kawacode-pipe-read! (pipe &optional unread)
  "Read a character from PIPE if UNREAD is nil.
Otherwise unread the character UNREAD from PIPE."
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
  (kawacode-pipe-with-pipe
   pipe
   (ignore write-pos)
   (let ((buf-size (length buf)))
     (cond (unread (kawacode-pipe-debug "unreading %s" unread)
		   (if (= (kawacode-pipe-var-ref num-writ) buf-size)
		       (progn (error "Buffer overflow (unread)"))
		     (prog1 unread (kawacode-pipe-inc-var! num-read  -1)
			    (kawacode-pipe-inc-var! num-writ 1)
			    ;; unreading does not alter write-pos
			    (kawacode-pipe-dec-var-mod-n! read-pos 1 buf-size))))
	   ((= (kawacode-pipe-var-ref num-read) buf-size)
	    (kawacode-pipe-debug "handling undeflow")
	    (kawacode-pipe-debug "got input %s" (funcall underflow-handler))
	    (kawacode-pipe-read! pipe))
	   (t (let ((res (prog1 (aref buf (kawacode-pipe-var-ref read-pos))
			   (kawacode-pipe-inc-var! num-read  1)
			   (kawacode-pipe-inc-var! num-writ -1)
			   ;; reading does not alter write-pos
			   (kawacode-pipe-inc-var-mod-n! read-pos 1 buf-size))))
		(kawacode-pipe-debug "read %c" res)
		res))))))

(defun kawacode-pipe-read-ln! (pipe)
  "Read a line from PIPE."
  (let ((chars '()))
    (while (not (funcall (lambda (chars)
			   (string-suffix-p kawacode-pipe-default-newline-delim
					    (concat chars)))
			 (setq chars (append chars (list (kawacode-pipe-read! pipe)))))))
    (concat chars)))

(defun kawacode-pipe-read-sexp! (pipe)
  "Read an sexp from PIPE."
  (read (lambda (&optional unread)
	  (kawacode-pipe-read! pipe unread))))

(defun kawacode-pipe-read-all! (pipe)
  "Read all currently available characters from PIPE into a string."
  (kawacode-pipe-with-pipe
   pipe
   (ignore write-pos underflow-handler)
   (let* ((buf-size (length buf))
	  (after-last-pos (mod (+ (kawacode-pipe-var-ref read-pos)
				  (kawacode-pipe-var-ref num-writ))
			       buf-size)))
     ;; after-last-pos -- the position just after the last
     ;; character to be read
     (prog1
	 (if (< after-last-pos (kawacode-pipe-var-ref read-pos))
	     (concat (substring-no-properties buf (kawacode-pipe-var-ref read-pos))
		     (substring-no-properties buf 0 after-last-pos))
	   (substring-no-properties buf (kawacode-pipe-var-ref read-pos) after-last-pos))
       (kawacode-pipe-inc-var-mod-n! read-pos (kawacode-pipe-var-ref num-writ) buf-size)
       (kawacode-pipe-set-var! num-read buf-size)
       ;; reading does not alter the write position
       (kawacode-pipe-set-var! num-writ 0)))))
;;}}}

;;{{{
;;; Writing Functions

(defun kawacode-pipe-write! (pipe char-or-str)
  "Write CHAR-OR-STR to PIPE."
  (let ((str (if (characterp char-or-str)
		 (char-to-string char-or-str)
	       char-or-str)))
   (kawacode-pipe-with-pipe
    pipe
    (ignore read-pos underflow-handler)
    (let ((buf-size (length buf)))
      (cond ((> (+ (kawacode-pipe-var-ref num-writ) (length str)) buf-size)
	     (error "Buffer overflow"))
	    (t
	     (kawacode-pipe-debug "wrote '%s'" str)
	     (prog1 (kawacode-pipe-memcpy! str buf (kawacode-pipe-var-ref write-pos))
	       (kawacode-pipe-inc-var! num-writ (length str))
	       (kawacode-pipe-dec-var! num-read (length str))
	       ;; writing does not alter the read position
	       (kawacode-pipe-inc-var-mod-n! write-pos (length str) buf-size))))))))

(defun kawacode-pipe-write-ln! (pipe &optional string)
  "Write STRING followed by a new line delimiter to PIPE."
  (kawacode-pipe-write! pipe (concat (or string "") kawacode-pipe-default-newline-delim)))

(defun kawacode-pipe-write-sexp! (pipe sexp)
  "Write SEXP to PIPE."
  (prin1 sexp (lambda (c) (kawacode-pipe-write! pipe c)))
  (kawacode-pipe-write! pipe " "))

;;}}}

;;}}}

(provide 'kawacode-pipe)
;;; kawacode-pipe.el ends here
