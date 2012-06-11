;;; save-undo.el --- preserve undo boundary across multiple-buffer edits

;; Copyright (C) 1993, 1998, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions

;; $Id: save-undo.el,v 1.2 1999/10/08 11:19:29 friedman Exp $

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

;; The default behavior of undo is that when editing in one buffer, an
;; undo boundary is created in another buffer the first time it is edited.
;; This makes sense for interactive behavior, but it's not desirable when a
;; single interactive command creates multiple undo boundaries via
;; non-interactive buffer switches.  For example, calling rfc822-addresses
;; within an interactive command can cause this to happen.  Wrapping the call
;; to rfc822-address with this macro can prevent that.

;;; Code:

;;;###autoload
(defmacro save-undo-boundary (&rest body)
  "Evaluate BODY, avoiding creating new undo boundaries from buffer excursions.

When a form temporarily switches buffers and performs insertions or
deletions in that other buffer, the next insertion or deletion in the
current buffer will create a new undo boundary in the current buffer.

That behavior can be undesirable when the user issues a single command but
has to perform multiple undo commands to unwind all its effects.
Use this macro to prevent that from happening."
  (` (prog1
         ;; This body may do some insertion and/or deletion in another
         ;; buffer.  Doing so will cause last_undo_buffer to be that buffer
         ;; instead of our current buffer.
         (progn (,@ body))
       ;; This dynamic let preserves our buffer's real undo list.
       (let ((buffer-undo-list nil)
             (buffer-read-only nil))
         ;; Do an insertion to cause the unwanted undo boundary,
         ;; and to set last_undo_buffer to the current buffer.
         (insert "\146\156\157\162\144")
         ;; Undo the insertion, leaving the buffer as it was.  Now that
         ;; last_undo_buffer is the current buffer, later insertions will not
         ;; cause an undo boundary in the original undo list.
         (primitive-undo 1 buffer-undo-list)))))

;; indent like save-excursion
(put 'save-undo-boundary 'lisp-indent-function 0)

(provide 'save-undo)

;;; save-undo.el ends here.
