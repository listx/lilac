(require 'ert)
(require 'lilac)

(defun nref (s) (concat "__NREF__" s))
(ert-deftest t-lilac-is-parent-block ()
  (with-temp-buffer
    (insert "#+name: parent\n")
    (insert "#+caption: parent\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child") "\n"))
    (insert "#+end_src\n")
    (goto-char (point-min))
    (let ((src-block (org-element-at-point)))
      (should-not (equal nil (lilac-is-parent-block src-block))))))
(ert-deftest t-lilac-get-noweb-children ()
  (let ((body
         (concat
          "#+name: parent\n"
          "#+caption: parent\n"
          "#+begin_src emacs-lisp\n"
          "; foo\n"
          "#+end_src\n")))
    (should (equal (lilac-get-noweb-children body)
                   ())))
  (let ((body
         (concat
          "#+name: parent\n"
          "#+caption: parent\n"
          "#+begin_src emacs-lisp\n"
          "; foo\n"
          (nref "one") "\n"
          "; bar\n"
          (nref "two") "\n"
          "#+end_src\n")))
    (should (equal (lilac-get-noweb-children body)
                   `(,(nref "one") ,(nref "two"))))))

(provide 'lilac-tests)
