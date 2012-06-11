;;; string-fns.el --- an assortment of string-manipulation functions

;; Copyright (C) 1991-1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: string-fns.el,v 1.5 2004/05/11 08:57:40 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

;; Originally from Emacs 20.1, modified for compatibility with older emacsen.
(defmacro with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  (` (let ((standard-output
            (get-buffer-create (generate-new-buffer-name " *string-output*"))))
       (let ((standard-output standard-output))
         (,@ body))
       (let ((orig-buf (current-buffer)))
         (set-buffer standard-output)
         (prog1
             (buffer-string)
           (kill-buffer nil)
           (set-buffer orig-buf))))))

;; indent like save-excursion
(put 'with-output-to-string 'lisp-indent-function 0)

;;;###autoload
(defun base16-decode-string (string)
  "Convert a hexadecimal-encoded string to its ascii equivalent.
Each character in the resulting string corresponds to a 2-digit hexadecimal
character in STRING.  E.g. the hex encoding of \"666E6F7264\" is \"fnord\"."
  (let* ((l (/ (length string) 2))
         (i 0)
         (s (make-string l 0))
         (tem (copy-sequence "?\\x00")))
    (while (< i l)
      (aset tem 3 (aref string (* 2 i)))
      (aset tem 4 (aref string (1+ (* 2 i))))
      ;; This is mule-safe since read will return a char type
      (aset s i (read tem))
      (setq i (1+ i)))
    s))

;;;###autoload
(defun base16-encode-string (string)
  "Return a string representing the hexadecimal encoding of STRING.
Each character in STRING can be represented by a 2-digit hexadecimal
character.  E.g. the base 16 encoding of \"fnord\" is \"666E6F7264\"."
  (let* ((l (length string))
         (i 0)
         (n (make-string (* 2 l) 0))
         tem)
    (while (< i l)
      (setq tem (format "%.2x" (aref string i)))
      (aset n (* 2 i) (aref tem 0))
      (aset n (+ 1 (* 2 i)) (aref tem 1))
      (setq i (1+ i)))
    (upcase n)))

;; Roland McGrath wrote this.
;;;###autoload
(defun glob->regexp (pattern)
  (let ((len (length pattern))
	(i 0)
	(slash-p t)
	c)
    ;; We may mutate the string below.
    (setq pattern (copy-sequence pattern))
    (while (< i len)
      (setq c (aref pattern i))
      (cond ((= c ?\\)
	     (if (memq (aref pattern (1+ i)) '(?\\ ?^ ?$ ?+ ?. ?*))
		 (setq i (1+ i))
	       (setq pattern (concat (substring pattern 0 i)
				     (substring pattern (1+ i)))
		     len (1- len))))
	    ((= c ??)
	     (let ((re (if slash-p "[^./]" "[^/]")))
	       (setq pattern (concat (substring pattern 0 i)
				     re
				     (substring pattern (1+ i)))
		     i (+ i (length re) -1)
		     len (+ len (length re) -1))))
	    ((= c ?*)
	     (let ((re (if slash-p "[^./]*" "[^/]*")))
	       (setq pattern (concat (substring pattern 0 i)
				     re
				     (substring pattern (1+ i)))
		     i (+ i (length re) -1)
		     len (+ len (length re) -1))))
	    ((= c ?\[)
	     (if (= (aref pattern (1+ i)) ?!)
		 (progn
		   (aset pattern (1+ i) ?^)
		   (setq i (1+ i)))))
	    ((memq c '(?^ ?$ ?+ ?.))
	     (setq pattern (concat (substring pattern 0 i)
				   "\\" (substring pattern i))
		   i (1+ i)
		   len (1+ len))))
      (setq slash-p (= c ?/)
	    i (1+ i)))
    pattern))

;;;###autoload
(defun integer-to-string (n &optional base)
  "Return the representation of N as a string in base BASE (default 10).
Uses a minus sign if negative.
N must be an integer.
BASE must be between 2 and 35, inclusive."
  (let ((digits nil)
        (negativep (< n 0))
        r m)
    (or base (setq base 10))
    (if (or (< base 2) (> base 35))
        (signal 'domain-error (list 'base-between-2-and-35-inclusive-p base)))
    (setq n (abs n))
    (while (not (zerop n))
      (setq m (/ n base)
            r (- n (* m base))
            n m
            digits (cons (+ r (if (> r 9) ?W ?0)) digits)))
    (if negativep (setq digits (cons ?- digits)))
    (mapconcat 'char-to-string (or digits '(?0)) "")))

;;;###autoload
(defun matching-substring (n &optional string)
  "Return substring matched by last search.
N specifies which match data pair to use
Value is nil if there is no Nth match.
If STRING is not specified, the current buffer is used."
  (if (match-beginning n)
      (if string
	  (substring string (match-beginning n) (match-end n))
	(buffer-substring (match-beginning n) (match-end n)))))

;;;###autoload
(defun non-whitespace-regexp (&optional table)
  "Return a regular expression matching a non-whitespace character.
This function is sensitive to the current buffer's syntax table unless
TABLE, a syntax table, is specified."
  (or table
      (setq table (syntax-table)))
  (let ((s "")
        (ws-syntax '(?- ?\ )))
    (cond ((fboundp 'map-syntax-table)
           ;; XEmacs 20 way.
           (map-syntax-table
            (function (lambda (key val)
                        (and (memq (char-syntax key table) ws-syntax)
                             (setq s (concat s (char-to-string key))))
                        nil))
            table))
          (t
           (let ((orig-table (syntax-table))
                 (i 0)
                 (l (min 256 (length table))))
             (set-syntax-table table)
             (unwind-protect
                 (while (< i l)
                   (and (memq (char-syntax i) ws-syntax)
                        (setq s (concat s (char-to-string i))))
                   (setq i (1+ i)))
               (set-syntax-table orig-table)))))
    (concat "[^" s "]")))

;;;###autoload
(defun path-string->list (path)
  "Convert a colon-separated path string into a list.
Any null paths are converted to \".\" in the returned list so that
elements of the path may be treated consistently."
  (let* ((list (string-split path ":"))
         (l list))
    (while l
      (and (string= "" (car l))
           (setcar l "."))
      (setq l (cdr l)))
    list))

;;;###autoload
(defun pretty-directory-file-name (directory)
  "Like `directory-file-name', but truncate home directory prefix to `~/'."
  (let ((dir (directory-file-name directory))
        (home (expand-file-name "~")))
    (save-match-data
      (if (string-match (concat "^" (regexp-quote home)) dir)
          (concat "~" (substring dir (match-end 0)))
        dir))))

;;;###autoload
(defun re-substring (re string n &optional startpos)
  "Return the Nth substring matched by RE in STRING.
The arguments to this function are, in order:

    RE        -- regular expression
    STRING    -- string to search
    N         -- number of matched substring
    STARTPOS  -- optional argument; starting position in STRING to search."
  (save-match-data
    (and (string-match re string startpos)
         (substring string (match-beginning n) (match-end n)))))

;;;###autoload
(defun replace-string-regexp (string regexp replacement &optional count)
  "In STRING, replace occurences matching REGEXP with REPLACEMENT.
Optional argument COUNT means replace first COUNT occurences found,
otherwise replace all of them.
The original string is not modified."
  (save-match-data
    (let ((pos 0)
          (newstr ""))
      (while (and (or (null count)
                      (prog1
                          (> count 0)
                        (setq count (1- count))))
                  (string-match regexp string pos))
        (setq newstr
              (concat newstr
                      (substring string pos (match-beginning 0))
                      replacement))
        (setq pos (match-end 0)))
      (concat newstr (substring string pos)))))

;;;###autoload
(defun strchr (s c)
  "Return position of first occurence of char C in string S.
If there are no occurences, return -1."
  (let ((l (length s))
        (i 0)
        (p -1))
    (while (< i l)
      (if (eq (aref s i) c)
          (setq p i
                i l)
        (setq i (1+ i))))
    p))

;;;###autoload
(defun string-split (string &optional separator limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
Optional argument SEPARATOR can be any regexp, but anything matching the
 separator will never appear in any of the returned substrings.
 If not specified, SEPARATOR defaults to \"[ \\f\\t\\n\\r\\v]+\".
If optional arg LIMIT is specified, split into no more than that many
 fields \(though it may split into fewer\)."
  (or separator (setq separator "[ \f\t\n\r\v]+"))
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))

;;;###autoload
(defun string<->vector (obj)
  "Convert a string to a vector of characters or vice-versa."
  (let* ((l (length obj))
         (nobj (funcall (cond ((vectorp obj) 'make-string)
                              ((stringp obj) 'make-vector))
                        l 0))
         (i 0))
    (while (< i l)
      (aset nobj i (aref obj i))
      (setq i (1+ i)))
    nobj))

;; string-to-vector is in Emacs 20 and later.
(or (fboundp 'string-to-vector)
    (defalias 'string-to-vector 'string<->vector))

;; The string-to-number builtin in versions of emacs later than 19 support
;; a base argument already; this function is for use with v19 and earlier.
;;;###autoload
(defun string-to-number-base (string &optional base)
  "Convert STRING to a number by parsing it as a decimal number.
This parses both integers and floating point numbers.

If BASE, interpret STRING as a number in that base.  If BASE isn't
present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
If the base used is not 10, floating point is not recognized.
If a digit is encountered which is not within BASE, all further digits are
ignored."
  ;; This does not include a special case for octal base as the built-in
  ;; octal reader cannot handle more than 3 digits at a time; might as well
  ;; use the last case loop.
  (cond ((or (null base) (= base 10))
         (string-to-number string))
        ((= base 16)
         (car (read-from-string (format "?\\x%s" string))))
        ((or (< base 2) (> base 16))
         nil)
        (t
         (let* ((cvt (if (fboundp 'char-to-int) 'char-to-int 'identity))
                (p 0)
                (l (length string))
                (n 0)
                (i 0)
                (dec-off (funcall cvt ?0))
                (hex-off (funcall cvt ?W)))
           (while (< p l)
             (setq i (funcall cvt (aref string p)))
             (setq i (if (> i hex-off)
                         (- i hex-off)
                       (- i dec-off)))
             (if (and (>= i 0)
                      (< i base))
                 (setq n (+ (* n base) i)
                       p (1+ p))
               (setq p l)))
           n))))

;;;###autoload
(defun with-command-output-to-string (&rest args)
  "Execute inferior COMMAND, returning the text it outputs as a string.
Remaining arguments are optional arguments to the external command."
  (let ((buf (generate-new-buffer " *command output*"))
        (orig-buf (current-buffer)))
    (apply 'call-process (car args) nil buf nil (cdr args))
    (set-buffer buf)
    (prog1
        (buffer-string)
      (kill-buffer buf)
      (set-buffer orig-buf))))

;;;###autoload
(defun current-time-vector ()
  "Return a vector containing current date and time information.
Components are day, month, date, hour, minutes, seconds, and year.

   example: (current-time-vector)
            => [\"Mon\" \"Sep\" \"9\" \"22\" \"46\" \"10\" \"1991\"]"
  (apply 'vector (string-split (current-time-string) "[ \t:]+")))

(provide 'string-fns)

;;; string-fns.el ends here.
