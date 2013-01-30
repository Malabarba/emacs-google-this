;;; google-this.el --- A set of functions and bindings to google under point.

;; Copyright (C) 2012 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/emacs-google-this
;; Version: 1.0
;; Keywords: convenience hypermedia

;;; Commentary:

;; google-this is a package that provides a set of functions and
;; keybindings for launching google searches from within emacs.

;; The main function is `google-this' (bound to C-x g t). It does a
;; google search using the currently selected region, or the
;; expression under point. All functions are bound under "C-x g", to
;; see all keybindings type "C-x g C-h".

;; To start a blank search, do `google-search' (C-x g RET). If you
;; want more control of what "under point" means, there are the
;; `google-word', `google-symbol', `google-line' and
;; `google-region'functions, bound as w, s, l and r, respectively.

;; If the `google-wrap-in-quotes' variable is t, than searches are
;; enclosed by double quotes (default is NOT). If a prefix argument is
;; given to any of the functions, invert the effect of
;; `google-wrap-in-quotes'.

;; There is also a `google-error' (C-x g e) function. It checks the
;; current error in the compilation buffer, tries to do some parsing
;; (to remove file name, line number, etc), and googles it. It's still
;; experimental, and has only really been tested with gcc error
;; reports.

;;; Instructions:

;; INSTALLATION

;;  Make sure "google-this.el" is in your load path, then place
;; 	this code in your .emacs file:
;;		(require 'google-this)
;; 		(google-this-mode 1)

;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:

;;; Code:


(defgroup google-this '()
  "Customization group for `google-this-mode'.")

(defcustom google-wrap-in-quotes nil
  "If not nil, searches are wrapped in double quotes.

If a prefix argument is given to any of the functions, the
opposite happens."
  :type 'boolean
  :group 'google-this)

(define-prefix-command 'google-this-mode-submap)
(define-key google-this-mode-submap [return] 'google-search)
(define-key google-this-mode-submap "t" 'google-this) 
(define-key google-this-mode-submap "w" 'google-word)
(define-key google-this-mode-submap "s" 'google-symbol)
(define-key google-this-mode-submap "l" 'google-line)
(define-key google-this-mode-submap "e" 'google-error) 
(define-key google-this-mode-submap "r" 'google-cpp-reference) 

(defvar google-url "https://www.google.com/search?q=%s"
  "URL to google searches.")

(defvar google-quoted-url "https://www.google.com/search?q=%22%s%22"
  "URL to quoted google searches.")


(defcustom url-parser-regexps '(
                                ("%" "%25")
                                ("\\+" "%2B")
                                ("&" "%26")
                                ("\"" "%22")
                                ("/" "%2F")
                                ("[[:blank:]]+" "+")
                                )
  "List of (REGEXP REPLACEMENT) used by `parse-and-google-string'.
You shouldn't have to edit this. If you are forced to edit this
for some reason, contact me and let me know."
  :type '(repeat (list regexp string))
  :group 'google-this )

(defun google-decide-url (prefix)
  "Decide whether to quote or not."
  (if (if prefix (not google-wrap-in-quotes) google-wrap-in-quotes)
      google-quoted-url
    google-url))

(defun google-search (prefix)
  "Write and do a google search."
  (interactive "P")
  (let ((TEXT (replace-regexp-in-string
               "^\\s-+" ""
               (or (thing-at-point 'symbol)
                   (thing-at-point 'word)
                   (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))))
    (setq TEXT (read-string (concat "Googling [" TEXT "]: ") nil nil TEXT))
    (if (stringp TEXT)
        (parse-and-google-string TEXT prefix)
      (message "[google-string] Empty query."))))

(defun parse-and-google-string (text prefix &optional url-decider)
  "Convert illegal characters in TEXT to their %XX versions,
and then google."
  (unless url-decider (setq url-decider 'google-decide-url))
  (browse-url (replace-regexp-in-string
               "%s" 
               (dolist (rp url-parser-regexps text)
                 (setq text (replace-regexp-in-string
                             (car rp) (car (cdr rp)) text)))
               (funcall url-decider prefix))))

(defun google-string (prefix &optional TEXT NOCONFIRM)
  "Google given TEXT, but ask the user first if NOCONFIRM is nil."
  (interactive)
  (unless NOCONFIRM
    (setq TEXT (read-string "Googling: " 
                            (if (stringp TEXT) (replace-regexp-in-string "^[[:blank:]]*" "" TEXT)))))
  (if (stringp TEXT)
      (parse-and-google-string TEXT prefix)
    (message "[google-string] Empty query.")))

(defun google-line (prefix)
  "Google the current line."
  (interactive "P")
  (let ((Line (buffer-substring (line-beginning-position) (line-end-position))))
    (google-string prefix Line)))

(defun google-word (prefix)
  "Google the current word."
  (interactive "P")
  (google-string prefix (thing-at-point 'word) t))

(defun google-symbol (prefix)
  "Google the current symbol."
  (interactive "P")
  (google-string prefix (thing-at-point 'symbol) t))


(defun google-region (prefix)
  "Google the current region."
  (interactive "P")
  (google-string
   (buffer-substring-no-properties (region-beginning) (region-end)) prefix))

(defun google-this (prefix)
  "Description"
  (interactive "P")
  (cond
   ((use-region-p) (google-region prefix))
   ((thing-at-point 'symbol) (google-string prefix (thing-at-point 'symbol)))
   ((thing-at-point 'word) (google-string prefix (thing-at-point 'word)))
   (t (google-line prefix))))

(defun google-error (prefix)
  "Google the current error in the compilation buffer."
  (interactive "P")
  (unless (boundp 'compilation-mode-map)
    (error "No compilation active."))
  (save-excursion
    (let ((pt (point)) 
          (buffer-name (next-error-find-buffer)))
      (unless (compilation-buffer-internal-p)
        (set-buffer buffer-name))
      (google-string prefix
                     (replace-regexp-in-string "^[^:]*:[0-9 ]*:\\([0-9 ]*:\\)? *" ""
                      (buffer-substring (line-beginning-position) (line-end-position)))))))

(defun google-cpp-reference ()
  "Visit the most probable cppreference.com page for this word."
  (interactive)
  (parse-and-google-string (concat "site:cppreference.com " (thing-at-point 'symbol)) nil 'google-feeling-lucky-decider))

(defun google-feeling-lucky-decider (prefix)
  "Just returns the feeling lucky url."
  "http://www.google.com/search?btnI=I'm Feeling Lucky&q=%s")

;;;###autoload
(define-minor-mode google-this-mode nil nil " Google"
  `(("g" . ,google-this-mode-submap))
  :global t
  :group 'google-this)


(provide 'google-this)

;;; google-this.el ends here
