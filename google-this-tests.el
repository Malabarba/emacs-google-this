(require 'ert)
(require 'noflet)

(require 'google-this)

(ert-deftest google-this-insert-lucky-string-should-replace-region ()
  (with-temp-buffer
    (insert "imdb raiders")
    (let ((term-end (point))
          (old-last-url google-this--last-url))
      (noflet ((region-active-p () t)
               (region-beginning () (point-min))
               (region-end () term-end)
               (kill-region (l r)
                 (delete-region l r))
               (google--do-lucky-search (_ _)
                 (setq google-this--last-url "http://www.imdb.com/title/tt0082971/")))
        (insert " suffix")
        (google-lucky-and-insert-url "imdb raiders" t)
        (should (equal "http://www.imdb.com/title/tt0082971/ suffix" (buffer-substring-no-properties (point-min) (point-max))))
        (setq google-this--last-url old-last-url)))))

;;; Local Variables:
;;; lisp-indent-function:common-lisp-indent-function
;;; End:
