(setq custom-file "~/elisp/custom-config.el")
(load custom-file)

;;(setq stack-trace-on-error t)

;;(require follow-mouse)
(setq inhibit-startup-message t)
(setq require-final-newline t)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;(setq enable-recursive-minibuffers t)

(setq load-path (append (list (expand-file-name "~/elisp"))
                        load-path))


;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")

(if (string-match "windows" (symbol-name system-type))
    (let ((dir default-directory)
          (site-lisp (convert-standard-filename "/usr/share/emacs/site-lisp")))
      ;;      (setq w32-num-mouse-buttons 2)

      ;;      (setq load-path (append (list site-lisp) load-path))
      (add-to-list load-path site-lisp)
      (cd site-lisp)
      (normal-top-level-add-subdirs-to-load-path)
      (cd dir)))

;; TODO: make php auto load and set this on a load hook
(setq php-manual-path "/usr/share/doc/php/php-chunked-xhtml")

;; TODO: load here or require?
;;(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")

;; TODO: use autoload for some of these
(mapcar 'require '(
                   ;;cedet
                   ;;ecb
		   ;;paren
		   ;;follow-mouse
		   generic-x
		   ;;cygwin-mount
                   ;;buff-menu
		   ;;remember-config
		   ;;planner-config
		   key-maps
                   recentf
                   tramp
                   php-mode
                   ))

;;(cygwin-mount-activate)

;; Enable wheelmouse support by default
;;(cond (window-system
;;       (load-library "mwheel")))
;;(if (not (string-match "windows" (symbol-name system-type)))
;;    (turn-on-follow-mouse))


;;; files
(recentf-mode 1)
(filesets-init)

;;; buffers
;;(eval-after-load "buff-menu" '(require 'buff-menu+)) ; bound to C-x C-b


;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;;C C++
;; (defun lb-compile-adjust-variable ()
;;   (unless (file-exists-p "Makefile")
;;     (set (make-local-variable 'compile-command)
;;          (let ((file (file-name-nondirectory buffer-file-name)))
;;            (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)
;;                    " " file)))))

;; (eval-after-load 'compile (lambda ()
;;                             (add-hook 'c-mode-hook
;;                                       'lb-compile-adjust-variable)))

;;;Scheme
;; Quack
;;(autoload 'run-scheme "quack" nil t)
;;
;;(add-hook 'scheme-mode-hook
;;	  (lambda ()
;;	    (define-key scheme-mode-map [f5]
;;	      '(lambda ()
;;		 (interactive)
;;		 (ignore-errors
;;		   (let ((symbol (thing-at-point 'symbol)))
;;		     (info "(r5rs)")
;;		     (Info-index symbol)))))))

;;; elib
;;(setq load-path (append (list "/usr/share/emacs/site-lisp/elib")
;;                       load-path))


;;; ILISP
;;(defun ilisp ()
;;  (interactive)
;;  (load ".ilisp")
;;  (run-ilisp))

;;; This makes reading a Lisp or Scheme file load in ILISP.
;; (set-default 'auto-mode-alist
;;  	     (append '(("\\.lisp$" . lisp-mode)
;; 		       ("\\.lsp$" . lisp-mode)
;; 		       ("\\.cl$" . lisp-mode)
;; 		       ("\\.ste" . lisp-mode)) ; stella
;; 		     auto-mode-alist))

;; (setq lisp-mode-hook '(lambda () (load ".ilisp") (require 'ilisp)))

;; (set-default 'auto-mode-alist
;; 	     (append '(("\\.scm$" . scheme-mode)
;; 		       ("\\.ss$" . scheme-mode)
;; 		       ("\\.stk$" . scheme-mode)
;; 		       ("\\.stklos$" . scheme-mode))
;; 		     auto-mode-alist))

;;(setq scheme-mode-hook '(lambda () (load ".ilisp") (require 'ilisp)))

;;; ACL
;; (defun acl ()
;;   (interactive)
;;   (load "/local/acl5/eli/fi-site-init")
;;   (setq fi:common-lisp-image-name "/local/acl5/lisp")
;;   (fi:common-lisp))

;;; STELLA
;; (defun stella (&optional arg)
;;   (interactive "P")
;;   (load "/local/acl5/eli/fi-site-init")
;;   (fi:common-lisp "STELLA"		     ;buffer
;; 		  "/home/lbrown/lang/stella" ;directory
;; 		  "/local/acl5/lisp"     ;executable 
;; 		  nil			;args
;; 		  nil			     ;host
;; 		  "/home/lbrown/lang/stella/stella.dxl" ;image
;; 		  ))

					;;;W3M
;;(setq browse-url-browser-function 'w3m-browse-url)


;;; Info
;;; info
(setq configure-info-directory "/usr/share/info")
(setq Info-default-directory-list
      (list 
       "/usr/share/info"
       ;;"/usr/local/info/" 
       ;;"/usr/local/share/info/"
       ;;"/usr/local/gnu/info/"
       ;;"/usr/local/gnu/lib/info/"
       ;;"/usr/local/gnu/lib/emacs/info/"
       ;;"/usr/local/emacs/info/"
       ;;"/usr/local/lib/info/"
       ;;"/usr/local/lib/emacs/info/"
       ;;"/usr/info/"
       ))

(autoload 'Info-find-node "info")
(autoload 'Info-find-node "myinfo")
(defun myinfo (&optional file node)
  "Enter Info,  the documentation browser.
 Optional argument FILE specifies the file to examine;
 the default is the top-level directory of Info.

 In interactive use a file name and node are read from the minibuffer.

 The search path for Info files is in the variable `Info-directory-list'.
 The top-level Info directory is made by combining all the files named `dir' 
 in all the directories in that path."
  (interactive 
   (list (read-string "File(dir): " nil nil "dir")
         (read-string "Node(top): " nil nil "top")))
  (if file 
      (Info-find-node file node) ) )

;; TODO write function copy-line
(defun copy-line (arg)
  "Copy line from point, including new line, to kill buffer.
With arg N, copy that many lines."
  (interactive "P")
  (copy-region-as-kill (point)
		       (progn
			 (if arg
			     (forward-visible-line (prefix-numeric-value arg))
			   (if (eobp)
			       (signal 'end-of-buffer nil))
			   (let ((end
				  (save-excursion
				    (end-of-visible-line) (point))))
			     (if (or (save-excursion
				       ;; If trailing whitespace is visible,
				       ;; don't treat it as nothing.
				       (unless show-trailing-whitespace
					 (skip-chars-forward " \t" end))
				       (= (point) end))
				     (and kill-whole-line (bolp)))
				 (forward-visible-line 1)
			       (goto-char end))))
			 (point))))








(defun open-line-and-go (arg)
  "Open a newline, leaving present line intact, 
and go to the beginning of that line.
With arg N, insert N newlines and go to last."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point))
         (newloc)
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (end-of-line)
    (newline arg)
    (setq newloc (point))
    (goto-char loc)
    (while (> arg 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq arg (1- arg)))
    (goto-char newloc)
    (end-of-line)))

(defun open-line-and-go-neg (arg)
  "Open a newline, leaving present line intact, 
and go to the beginning of that line.
With arg N, insert N newlines and go to last."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
					;(loc (point))
         (newloc)
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (beginning-of-line)
    (setq newloc (point))
    (newline arg)
    (goto-char newloc)
    (while (> arg 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq arg (1- arg)))
    (goto-char newloc)
    (end-of-line)))

(defun quote-region (char)
  "quote region with given char."
  (insert-char char 1 t)
  (exchange-point-and-mark)
  (insert-char char 1 t) 
  (exchange-point-and-mark) )

(defun unquote-region ()
  "unquote region."
  (delete-char 1 nil)
  (exchange-point-and-mark)
  (delete-char 1 nil)
  (exchange-point-and-mark) )

(defun yank-secondary ()
  "Insert the secondary selection at point.
  Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))

(defun iter (first last)
  "Insert numbers from FIRST to LAST, one per line."
  (interactive "nFirst: \nnLast: ")
  (let ((i first))
    (if (< first last)
        (while (<= i last)
          (insert (format "%d\n" i))
          (setq i (+ 1 i)))
      (while (>= i last)
	(insert (format "%d\n" i))
	(setq i (- i 1))))))

(defun start-bash-here()
  "Open a bash window in the current directory."
  (interactive)
  ;; I'd like to use mswindows-shell-execute but it doesn't seem to start
  ;; in the current directory, or provide a way to specify start directory.
  (start-process
   "*bash*"
   nil
   "/bin/mintty" "-"))


(defun underline-previous-line ()
  "Underline the previous line with dashes."
  (interactive)
  (let ((start-pos (point))
        (start-col nil)
        (end-col nil))
    (beginning-of-line 0)
    (if (re-search-forward "[^ ]" (save-excursion (end-of-line) (point)) t)
        (progn
          (setq start-col (- (current-column) 1))

          (end-of-line)
          (re-search-backward "[^ ]" nil t)
          (setq end-col (current-column))

          ;; go to next line and insert dashes
          (beginning-of-line 2)
          (insert
           (make-string start-col ?\ )
           (make-string (+ 1 (- end-col start-col)) ?-)
           "\n")
          )
      (goto-char start-pos)
      (error "No text on previous line"))
    ))

; dired
(add-hook 'dired-load-hook
          (function (lambda ()
;                      (load "dired-x")
                      (load "dired-details+")
                      (setq dired-omit-files-p t)
                      ;; global var here
                      )))

(add-hook 'dired-mode-hook
          (function (lambda ()
                      ;; buffer local vars
                      )))

(defun dired-mouse-find-alternate-file (event)
  "In dired, visit the file or directory you click on instead of the dired buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename nil t))))
    (select-window (posn-window (event-end event)))
    (find-alternate-file (file-name-sans-versions file t))))


;; ;; browse-url function
;; (defun lb/browse-url-win-or-w3m (url browse)
;;   (interactive (list ("URL(google): " nil nil "www.google.com")
;;                      ("browser(FireFox): " nil nil "firefox")))
;; ;(url (read-string "URL(google): " nil nil "http://www.google.com"))
;; ;         (browser (read-string "browser(w3m): " nil nil "w3m")))
;;   (if (equal browser "w3m")
;;       (w3m-browse-url url)
;;     (browse-url-default-windows-browser url)))

;; (defadvice browse-url (around browse-url-win-or-w3m first compile preactivate)
;;   "Ask user what browser he wants to use before opening a url."
;;   (if (command-p lb/browse-url-win-or-w3m)
;;       (message-box "lb/browse-url-win-or-w3m is a command."))
;;   (call-interactively lb/browse-url-win-or-w3m))

;; ;(ad-activate 'browse-url)
;; (ad-enable-advice 'browse-url 'around 'browse-url-win-or-w3m)



;;;_+ Backup configuration

(setq backup-by-copying t)
(setq backup-directory-alist  '(("." . "~/.saves")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)
(setq bookmark-save-flag 1)

;;;_+ planner
;; (when (fboundp 'plan)
;;   (planner-update-wiki-project)
;;   (setq planner-carry-tasks-forward nil)
;;   (plan))

;;;_+ org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(org-agenda-list)

;; evil
;(require 'evil)
;(evil-mode 1)

(cd "~")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
