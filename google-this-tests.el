(require 'ert)
(add-to-list 'load-path (expand-file-name "./"))
(require 'noflet)
(require 'google-this)

(ert-deftest google-this-insert-lucky-string-should-replace-region ()
  (with-temp-buffer
    (insert "prefix ")
    (save-excursion (insert " suffix"))
    (let ((left (point))
          (old-last-url google-this--last-url)
          right)      
      (insert "imdb raiders")
      (setq right (point))
      (noflet ((region-active-p () t)
               (region-beginning () left)
               (region-end () right)
               (read-string (_ s) s)
               (kill-region (l r)
                 (delete-region l r))
               ;; Make this run synchronously, no need for manual search
               (google--do-lucky-search (_ func)
                 (funcall func "http://www.imdb.com/title/tt0082971/")))
        (call-interactively 'google-lucky-and-insert-url)
        (should (equal "prefix http://www.imdb.com/title/tt0082971/ suffix" (buffer-substring-no-properties (point-min) (point-max))))
        (setq google-this--last-url old-last-url)))))

;;; Local Variables:
;;; lisp-indent-function:common-lisp-indent-function
;;; End:
