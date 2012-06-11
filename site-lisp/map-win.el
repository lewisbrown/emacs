;;; map-win.el --- run expressions for various windows

;; Copyright (C) 1994, 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, macros
;; Status: Works in Emacs 18, 19, and XEmacs/Lucid Emacs.
;; Created: 1994-06-23

;; LCD Archive Entry:
;; map-win-popup|Noah Friedman|friedman@prep.ai.mit.edu|
;; ensure display of a process buffer when new output arrives|
;; $Date: 1995/03/25 08:47:43 $|$Revision: 1.1 $|~/functions/map-win.el.gz|

;; $Id: map-win.el,v 1.1 1995/03/25 08:47:43 friedman Exp $

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
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; This is pretty much equivalent to the `walk-windows' function in Emacs
;; 19, but the macro is "cleaner" since it avoids creating local variables
;; whose names could interfere with the dynamic scope of the caller.  It
;; also allows the frames argument to specify a particular frame object to
;; traverse.

;;; Code:

(require 'backquote)

;;; Ensure smooth operation in both Emacs and XEmacs/Lucid.
;;; These are defined here before any macros are used so that macro
;;; expansion done at compile time will be correct.

(or (fboundp 'defalias)
    (fset 'defalias 'fset))

(cond ((fboundp 'make-frame)
       ;; Emacs 19
       (defalias 'map-win-frame-first-window 'frame-first-window)
       (defalias 'map-win-framep             'framep)
       (defalias 'map-win-make-frame         'make-frame)
       (defalias 'map-win-select-frame       'select-frame)
       (defalias 'map-win-selected-frame     'selected-frame))

      ((fboundp 'make-screen)
       ;; XEmacs
       (defalias 'map-win-frame-first-window 'screen-selected-window)
       (defalias 'map-win-framep             'screenp)
       (defalias 'map-win-make-frame         'make-screen)
       (defalias 'map-win-select-frame       'select-screen)
       (defalias 'map-win-selected-frame     'selected-screen)))

(defmacro map-windows (body &optional minibuffers frames save-resultp)
  "For each window in turn, run body with that window and buffer made current.

Optional second arg `minibuffers' t means map minibuffer windows even if
they are not active.  `minibuffers' nil or omitted means map minibuffer
windows iff they are active.  `minibuffers' neither t nor nil means not to
map over minibuffers even if they are active; but this function must not be
called while currently in a minibuffer or this flag is unilaterally
overridden to be `t', to avoid an endless loop.

Optional third arg `frames' = `t' means map over windows on all frames.
`frames' = `visible' means include windows on all visible frames.
`frames' = 0 means include windows on all visible and iconified frames.
If `frames' is a frame object, consider only windows on that frame.
Anything else means restrict to the currently-selected frame.

Optional fourth arg `save-resultp' non-`nil' means save the return value of
each evaluation of `body' in a list, in order from first to last, and
return it at the end.  `save-resultp' = `nil' means do not save these
values; just return `nil'."
  ;; Use new, uninterned symbols to avoid name collisions with body.
  (let ((orig-window (make-symbol "$orig-window$"))
        (this-window (make-symbol "$this-window$"))
        (orig-buffer (make-symbol "$orig-buffer$"))
        (orig-frame (make-symbol "$orig-frame$"))
        (done (make-symbol "$done$"))
        (result (make-symbol "$result$"))
        (result-list (make-symbol "$result-list$")))
    (` (let* (((, orig-window) (selected-window))
              ((, this-window) (, orig-window))
              ((, orig-buffer) (current-buffer))
              ((, orig-frame) (and (fboundp 'map-win-selected-frame)
                                   (map-win-selected-frame)))
              (, done)
              (, result)
              (, result-list))

         ;; next-window can't be told to search a specific frame; it either
         ;; searches some subset of all visible or iconified frames, or the
         ;; selected frame.  So if the frames argument is a frame object,
         ;; just cycle through the windows on that frame.
         (,@ (and (fboundp 'map-win-framep)
                  (` ((cond
                       ((and (map-win-framep (, orig-frame))
                             (map-win-framep (, frames)))
                        (map-win-select-frame (, frames))
                        (setq (, orig-window)
                              (select-window (map-win-frame-first-window)))
                        (setq (, this-window) (, orig-window))))))))

         ;; If starting from a minibuffer window, make sure to get back.
         (and (eq (, this-window) (minibuffer-window))
              (setq minibuffers t))

         (while (not (, done))
           (select-window (, this-window))
           (set-buffer (window-buffer (, this-window)))
           (setq (, result)
                 (, (cond
                     ((and (listp body)
                           (memq (car body) '(function lambda)))
                      (` (funcall (, body))))
                     (t body))))
           (,@ (and save-resultp
                    (` ((setq (, result-list)
                              (cons (, result) (, result-list)))))))
           (setq (, this-window)
                 (next-window (, this-window)
                              (, minibuffers)
                              (,@ (and (fboundp 'map-win-make-frame)
                                       frames
                                       (list frames)))))
           (and (eq (, this-window) (, orig-window))
                (setq (, done) t)))

         ;; Restore original frame if changed.
         (,@ (and (fboundp 'map-win-framep)
                  (` ((cond
                       ((and (map-win-framep (, orig-frame))
                             (map-win-framep (, frames)))
                        (map-win-select-frame (, orig-frame))))))))

         (set-buffer (, orig-buffer))

         (, (and save-resultp
                 (` (nreverse (, result-list)))))))))

;; This gives map-windows the same indentation style as save-excursion.
(put 'map-windows 'lisp-indent-function 0)


(provide 'map-win)

;; map-win.el ends here
