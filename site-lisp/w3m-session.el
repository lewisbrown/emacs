;;; w3m-session.el --- Persistent emacs-w3m sessions

;; Copyright (C) 2003, 2004  Jose A Ortega Ruiz

;; Author: Jose A Ortega Ruiz <jao@member.fsf.org>
;; Version: 0.3.3
;; Keywords: hypermedia, w3m, WWW

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; INTRODUCTION:
;;
;; w3m-session provides persistent emacs-w3m browsing sessions. When
;; quitting w3m (or, if you request it, at any other time while using
;; it) you can save the current w3m session (that is, the set of open
;; tabs and the URLs they're visiting). Upon restarting emacs-w3m
;; (possibly after restarting Emacs itself) you'll have the possibity
;; of recovering the saved session (that is, of re-opening the saved
;; tabs and URLs). You also have at your disposal a command to recover
;; the saved session at any other time.
;;
;; INSTALLATION:
;;
;; Just put this file somewhere on your Emacs load path and add the
;; following line to your .emacs file:
;;
;;   (require 'w3m-session)
;;
;; After restarting Emacs (or evaluating the form above), each time
;; you start emacs-w3m with 'w3m' you'll get a prompt asking whether
;; your last browsing session should be loaded. Likewise, when
;; quitting the browser, you'll have the possibility of saving your
;; current session (overwriting the previous one).
;;
;; In addition, two new interactive functions are defined:
;;
;;   w3m-session-load  --  load the last stored session
;;   w3m-session-save  --  save the current session
;;
;; These functions can be invoked at any time while running emacs-w3m.
;; Optionally, you can bind them to key shortcuts with the proper
;; variations of the following elisp magic in your .emacs:
;;  (defun w3m-add-keys ()
;;    (define-key w3m-mode-map "S" 'w3m-session-save)
;;    (define-key w3m-mode-map "L" 'w3m-session-load))
;;  (add-hook 'w3m-mode-hook 'w3m-add-keys)
;;
;; CUSTOMIZATION:
;;
;; A new customization group, w3m-session, is available. There you can
;; customize the following variables:
;;
;;   w3m-session-load-always     -- if t, `w3m-session-load' will *not* ask
;;                                for confirmation (default nil)
;;   w3m-session-save-always     -- if t, `w3m-session-load' will *not* ask
;;                                for confirmation (default nil)
;;   w3m-session-show-titles     -- if t, the load prompt will list the
;;                                session URL titles (default t)
;;   w3m-session-duplicate-tabs  -- what to do when loading a session that
;;                                contains a URL already open
;;   w3m-session-file            -- the file where w3m session info
;;                                is stored (default "~/.w3m-session")
;;
;; You can also customize them in your .emacs file, to wit:
;;
;;   (setq w3m-session-file "~/.emacs.d/w3m-session")
;;   (setq w3m-session-save-always nil)
;;   (setq w3m-session-load-always nil)
;;   (setq w3m-session-show-titles t)
;;   (setq w3m-session-duplicate-tabs 'ask) ;  'never, 'always, 'ask
;;
;; HISTORY:
;;
;;   Version 0.3.3 ():
;;
;;     - save session file with pretty print.
;;     - handle correctly multiple emacs-w3m (re)starts during a
;;       single emacs session.
;;     - code cleanup.
;;
;;   Version 0.3.2 (Mon Sep 29, 2003):
;;
;;     - bug fix: when searching or going to home/bookmarks/etc,
;;       keep the current tab's focus.
;;
;;   Version 0.3.1 (Tue Aug 26, 2003):
;;
;;     - type of `w3m-session-file' set to 'file' in customisation
;;       buffer.
;;     - bug fix: syntax error due to a typo in `w3m-session-file'
;;
;;   Version 0.3 (Mon Aug 25, 2003):
;;
;;     - the load session tab lists the titles of the session's pages
;;       (customizable via 'w3m-session-show-titles').
;;     - the duplicated tab prompt displays also the URL's title.
;;     - bug fix: active tab in session now is correctly saved.
;;
;;   Version 0.2 (Fri Aug 22, 2003):
;;
;;     - the session info now includes the active tab, which gets
;;       displayed when the session is reloaded.
;;     - when reloading a session in a running emacs-w3m, if the
;;       session contains a URL that is already being displayed by the
;;       browser, the tab can be reused or duplicated (customizable
;;       via `w3m-session-duplicate-tabs').
;;
;;   Version 0.1 (Wed Aug 20, 2003) -- Initial release.
;;


;;; Code:

;;; Dependencies:

(require 'cl)
(require 'w3m)
(require 'advice)

;;; Custom variables:

(defgroup w3m-session nil
  "w3m - session saving in w3m."
  :group 'w3m
  :prefix "w3m-session-")

(defcustom w3m-session-save-always nil
  "If on, always save w3m session without asking."
  :group 'w3m-session
  :type 'boolean)

(defcustom w3m-session-load-always nil
  "If on, always load w3m session without asking."
  :group 'w3m-session
  :type 'boolean)

(defcustom w3m-session-show-titles t
  "If on, show URL titles in the load prompt."
  :group 'w3m-session
  :type 'boolean)

(defcustom w3m-session-duplicate-tabs 'never
  "How to treat session URL already being visited.

When loading a session with `w3m-session-load', if one of the URLs in
the session is already displayed in a w3m tab, w3m-session can:
- `never' create a new tab (just reload it), or
- `always' duplicate the URL in a new tab, or
- `ask' the user what to do."
  :group 'w3m-session
  :type  '(choice (const :value never)
                  (const :value always)
                  (const :value ask)))

(defcustom w3m-session-file "~/.w3m-session"
  "File to save the w3m session data."
  :group 'w3m-session
  :type 'file)

;;; Interactive functions:

(defun w3m-session-save ()
  "Save the current w3m session."
  (interactive)
  (when (and (w3m-alive-p)
             (or w3m-session-save-always
                 (y-or-n-p "Save current w3m session? ")))
    (w3m-session-current-to-file)))

(defun w3m-session-load ()
  "Load last stored session into w3m."
  (interactive)
  (let ((s (w3m-session-load-aux)))
    (when s
      (let* ((urls (w3m-session-url s))
             (offset (w3m-session-offset s))
             (buffers (unless (equal w3m-session-duplicate-tabs 'always)
                        (w3m-session-find-duplicated urls))))
        (w3m-goto-url-new-session urls t)
        (when buffers (w3m-session-close-buffers buffers))
        (unless (zerop offset) (w3m-next-buffer offset))))))

;;; Internals:

;;;; advice w3m to use session management

(defadvice w3m (before jao-load-session activate)
  "Optionally load last w3m session on startup."
  (interactive
   (let ((s (w3m-session-current (w3m-session-load-aux))))
     (list (or (w3m-session-url s) w3m-home-page) t t))))

(defadvice w3m (after jao-select-tab activate)
  "Goto the saved focused tab"
  (interactive)
  (let ((offset (w3m-session-offset)))
    (unless (zerop offset)
      (w3m-next-buffer offset))
    (ad-deactivate 'w3m)))

(defadvice w3m-quit (before jao-save-session activate)
  "Save session before quitting."
  (interactive)
  (w3m-session-save)
  ;; this is a little hack: when quitting a w3m session with a tab
  ;; selected other than the first, the frame is not automatically
  ;; closed as should be when w3m-pop-up-frames is t:
  (switch-to-buffer (car (w3m-list-buffers)))
  (ad-activate 'w3m))

;;;; save session on exit using the desktop package

(eval-after-load "desktop"
  '(add-hook 'desktop-save-hook 'w3m-session-save))

;;;; auxiliar functions

(defvar jao-w3m-current-session '(w3m-session 0 nil))

(defun w3m-session-url (&optional s)
  (let ((s (or s jao-w3m-current-session)))
    (concat "group:"
            (mapconcat 'car (nth 2 s) "&"))))

(defun w3m-session-offset (&optional s)
  (let ((s (or s jao-w3m-current-session)))
    (nth 1 s)))

(defun w3m-session-titles (&optional s)
  (let ((s (or s jao-w3m-current-session)))
    (mapcar 'cdr (nth 2 s))))

(defun w3m-session-current (&optional s)
  (save-current-buffer
    (setq jao-w3m-current-session
          (or s
              `(w3m-session
                ,(w3m-session-active-tab)
                ,(mapcar
                  (lambda (b) (set-buffer b)
                    (cons w3m-current-url
                          (w3m-buffer-title b))) (w3m-list-buffers)))))))

(defun w3m-session-current-url ()
  (when (w3m-alive-p)
    (save-current-buffer
      (concat "group:"
              (mapconcat (lambda  (b) (set-buffer b) w3m-current-url)
                         (w3m-list-buffers) "&")))))

(defun w3m-session-active-tab (&optional n bs alive)
  (let ((n (or n 0))
        (bs (or bs (w3m-list-buffers)))
        (alive (or alive (w3m-alive-p))))
    (if (equal alive (car bs)) n
      (w3m-session-active-tab (incf n) (cdr bs) alive))))

(defun w3m-session-find-duplicated (urls)
  (when (w3m-alive-p)
    (save-current-buffer
      (let* ((duplicate-p
              (lambda (b)
                (y-or-n-p
                 (format "'%s' (%s) is already open. Duplicate tab? "
                         (w3m-buffer-title b) w3m-current-url))))
             (test-b
              (lambda (b)
                (set-buffer b)
                (if (and
                     (string-match (regexp-quote w3m-current-url) urls)
                     (or (equal w3m-session-duplicate-tabs 'never)
                         (not (funcall duplicate-p b))))
                    b 'not)))
             (buffers (mapcar test-b (w3m-list-buffers))))
        (delete 'not buffers)))))

(defun w3m-session-close-buffers (buffers)
  (save-current-buffer
    (mapc 'kill-buffer buffers)))


(defun w3m-session-load-aux ()
  (let ((new-session (w3m-session-from-file
                      (expand-file-name w3m-session-file))))
    (if (and new-session
             (or w3m-session-load-always
                 (y-or-n-p
                  (if w3m-session-show-titles
                      (format "Load last w3m session %S? "
                              (w3m-session-titles new-session))
                    (format "Load last w3m session? ")))))
        (w3m-session-current new-session))))

(defun w3m-session-from-file (fname)
  (when (file-readable-p fname)
    (with-temp-buffer
      (insert-file-contents fname)
      (goto-char (point-min))
      (let ((sexp (read (current-buffer))))
        (and (equal 'w3m-session (car sexp)) sexp)))))

(defun w3m-session-current-to-file ()
  (require 'pp)
  (with-temp-buffer
    (insert ";;;; File generated by w3m-session. DO NOT EDIT!\n")
    (pp (w3m-session-current) (current-buffer))
    (insert "\n" ";;;; End of " (file-name-nondirectory w3m-session-file) "\n")
    (write-region (point-min) (point-max) (expand-file-name w3m-session-file))))

(provide 'w3m-session)
;;; w3m-session.el ends here

