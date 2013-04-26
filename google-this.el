;;; google-this.el --- A set of functions and bindings to google under point.

;; Copyright (C) 2012 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/emacs-google-this
;; Version: 1.2.1
;; Keywords: convenience hypermedia

;;; Commentary:

;; google-this is a package that provides a set of functions and
;; keybindings for launching google searches from within emacs.

;; The main function is `google-this' (bound to C-c / g). It does a
;; google search using the currently selected region, or the
;; expression under point. All functions are bound under "C-c /"
;; prefix, in order to comply with emacs' standards. To see all
;; keybindings type "C-c / C-h".
;;
;; If you don't like this keybind, just reassign the
;; `google-this-mode-submap' variable.
;; My personal preference is "C-x g":
;; 
;;        (global-set-key (kbd "C-x g") 'google-this-mode-submap)

;; To start a blank search, do `google-search' (C-c / RET). If you
;; want more control of what "under point" means for the `google-this'
;; command, there are the `google-word', `google-symbol',
;; `google-line' and `google-region' functions, bound as w, s, l and r,
;; respectively. They all do a search for what's under point.

;; If the `google-wrap-in-quotes' variable is t, than searches are
;; enclosed by double quotes (default is NOT). If a prefix argument is
;; given to any of the functions, invert the effect of
;; `google-wrap-in-quotes'.

;; There is also a `google-error' (C-c / e) function. It checks the
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
;; 1.2.1 - 20130426 - Created an error parser for the google-error function. It works with c-like errors and is extendable to other types of errors using the varible `google-error-regexp'.
;; 1.2.1 - 20130426 - autoloaded any functions that the user might want to call directly.
;; 1.2 - 20130421 - Fixed docs.
;; 2013-05-04 -- Changed the keybinding to be standards compliant.
;; 2013-03-03 -- Fixed problem with backslash.
;; 2013-02-27 -- Added support for google-translate and google-maps packages. And added `google-forecast' function. And added `google-location-suffix' so we're not constrained to google.com anymore.
;;; Code:


(defgroup google-this '()
  "Customization group for `google-this-mode'.")
(defconst google-this-version "1.2.1"
  "Version string of the `google-this' package.")
(defconst google-this-version-int 2
  "Integer version number of the `google-this' package (for comparing versions).")
(defcustom google-wrap-in-quotes nil
  "If not nil, searches are wrapped in double quotes.

If a prefix argument is given to any of the functions, the
opposite happens."
  :type 'boolean
  :group 'google-this)

(defcustom google-this-suspend-after-search nil
  "Whether emacs should be minimized after a search is launched (calls `suspend-frame')."
  :type 'boolean
  :group 'google-this)

(define-prefix-command 'google-this-mode-submap)
(define-key google-this-mode-submap [return] 'google-search)
(define-key google-this-mode-submap "t" 'google-this) 
(define-key google-this-mode-submap "w" 'google-word)
(define-key google-this-mode-submap "s" 'google-symbol)
(define-key google-this-mode-submap "l" 'google-line)
(define-key google-this-mode-submap "e" 'google-error) 
(define-key google-this-mode-submap "f" 'google-forecast)
(define-key google-this-mode-submap "r" 'google-cpp-reference) 
(define-key google-this-mode-submap "m" 'google-maps)
;; "c" is for "convert language" :-P
(define-key google-this-mode-submap "c" 'google-translate-query-or-region)

(defun google-translate-query-or-region ()
  "If region is active `google-translate-at-point', otherwise `google-translate-query-translate'."
  (interactive)
  (unless (functionp 'google-translate-at-point)
    (error "[google-this]: This command requires the 'google-translate' package."))
  (if (region-active-p)
      (call-interactively 'google-translate-at-point)
    (call-interactively 'google-translate-query-translate)))

(defcustom google-location-suffix "com"
  "The url suffix associated with your location (com, co.uk, fr, etc)."
  :type 'string
  :group 'google-this)

(defun google-url () "URL to google searches."
  (concat "https://www.google." google-location-suffix "/search?q=%s"))

(defun google-quoted-url () "URL to quoted google searches."
  (concat "https://www.google." google-location-suffix "/search?q=%22%s%22"))


(defcustom url-parser-regexps '(
                                ("%" "%25")
                                ("\\+" "%2B")
                                ("&" "%26")
                                ("\"" "%22")
                                ("/" "%2F")
                                ("\\\\" "\\\\\\\\")
                                ("[[:blank:]]+" "+")
                                )
  "List of (REGEXP REPLACEMENT) used by `parse-and-google-string'.

You shouldn't have to edit this. If you are forced to edit this
for some reason, contact me and let me know."
  :type '(repeat (list regexp string))
  :group 'google-this )

(defcustom google-error-regexp '(("^[^:]*:[0-9 ]*:\\([0-9 ]*:\\)? *" ""))
  "List of (REGEXP REPLACEMENT) pairs to parse error strings."
  :type '(repeat (list regexp string))
  :group 'google-this)


(defun google-decide-url (prefix)
  "Decide whether to quote or not."
  (if (if prefix (not google-wrap-in-quotes) google-wrap-in-quotes)
      (google-quoted-url)
    (google-url)))

;;;###autoload
(defun google-search (prefix)
  "Write and do a google search."
  (interactive "P")
  (let ((TEXT (replace-regexp-in-string
               "^\\s-+" ""
               (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (or (thing-at-point 'symbol)
                     (thing-at-point 'word)
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))) )))
    (setq TEXT (read-string (concat "Googling [" TEXT "]: ") nil nil TEXT))
    (if (stringp TEXT)
        (parse-and-google-string TEXT prefix)
      (message "[google-string] Empty query."))))

(defun parse-and-google-string (text prefix &optional url-decider)
  "Convert illegal characters in TEXT to their %XX versions, and then google."
  (unless url-decider (setq url-decider 'google-decide-url))
  (browse-url (replace-regexp-in-string
               "%s" 
               (dolist (rp url-parser-regexps text)
                 (setq text (replace-regexp-in-string
                             (car rp) (car (cdr rp)) text)))
               (funcall url-decider prefix)))
  (when google-this-suspend-after-search
    (suspend-frame)))

;;;###autoload
(defun google-string (prefix &optional TEXT NOCONFIRM)
  "Google given TEXT, but ask the user first if NOCONFIRM is nil."
  (interactive)
  (unless NOCONFIRM
    (setq TEXT (read-string "Googling: " 
                            (if (stringp TEXT) (replace-regexp-in-string "^[[:blank:]]*" "" TEXT)))))
  (if (stringp TEXT)
      (parse-and-google-string TEXT prefix)
    (message "[google-string] Empty query.")))

;;;###autoload
(defun google-line (prefix)
  "Google the current line."
  (interactive "P")
  (let ((Line (buffer-substring (line-beginning-position) (line-end-position))))
    (google-string prefix Line)))

;;;###autoload
(defun google-word (prefix)
  "Google the current word."
  (interactive "P")
  (google-string prefix (thing-at-point 'word) t))

;;;###autoload
(defun google-symbol (prefix)
  "Google the current symbol."
  (interactive "P")
  (google-string prefix (thing-at-point 'symbol) t))


;;;###autoload
(defun google-region (prefix)
  "Google the current region."
  (interactive "P")
  (google-string
   prefix (buffer-substring-no-properties (region-beginning) (region-end))))

;;;###autoload
(defun google-this (prefix)
  "Automatically decide what the user wants to google (always something under point).

Unlike `google-search' (which presents an empty prompt with
\"this\" as the default value), this function inserts the query
in the minibuffer to be edited."
  (interactive "P")
  (cond
   ((region-active-p) (google-region prefix))
   ((thing-at-point 'symbol) (google-string prefix (thing-at-point 'symbol)))
   ((thing-at-point 'word) (google-string prefix (thing-at-point 'word)))
   (t (google-line prefix))))

;;;###autoload
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
                     (google-this-clean-error-string 
                      (buffer-substring (line-beginning-position) (line-end-position)))))))


;;;###autoload
(defun google-this-clean-error-string (s)
  "Parse error strings and turn them into googleable strings.

Removes unhelpful details like file names and line numbers from
simple error strings (such as c-like erros).

Uses replacements in `google-error-regexp' and stops at the first match."
  (interactive)
  (dolist (cur google-error-regexp out)
    (when (string-match (car cur) s)
        (setq out 
              (replace-regexp-in-string  (car cur)
                                         (car (cdr cur))
                                         s))
        (return out))))

;;;###autoload
(defun google-cpp-reference ()
  "Visit the most probable cppreference.com page for this word."
  (interactive)
  (parse-and-google-string (concat "site:cppreference.com " (thing-at-point 'symbol)) nil 'google-feeling-lucky-decider))

(defun google-feeling-lucky-decider (prefix)
  "Just returns the feeling lucky url."
  (concat "https://www.google." google-location-suffix "/search?btnI=I'm Feeling Lucky&q=%s"))

;;;###autoload
(defun google-forecast (prefix)
  "Just searches google for \"weather\"."
  (interactive "P")
  (if (not prefix) (parse-and-google-string "weather" nil)
    (parse-and-google-string (concat "weather" (read-string "Location: " nil nil ""))) nil))

;;;###autoload
(define-minor-mode google-this-mode nil nil " Google"
  `((,(kbd "C-c /") . ,google-this-mode-submap))
  :global t
  :group 'google-this)
;; (global-set-key (kbd "C-x g") 'google-this-mode-submap)

(provide 'google-this)

;;; google-this.el ends here
