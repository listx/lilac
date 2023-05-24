(require 'ert)
(require 'lilac)

(org-mode)

(defun nref (s) (concat "__NREF__" s))
(ert-deftest t-lilac-get-noweb-children ()
  (let ((body
         (concat
          "#+name: foo\n"
          "#+caption: foo\n"
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
(ert-deftest t-lilac-insert-noweb-source-code-block-captions ()
  (with-temp-buffer
    (insert "#+name: parent\n")
    (insert "#+caption: parent\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert (concat (nref "child2") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child1") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child2") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; baz\n")
    (insert "#+end_src\n")
    (lilac-insert-noweb-source-code-block-captions nil)
    (goto-char (point-min))
    (should (search-forward
             (concat "#+caption: =" (nref "child1") "=  [[parent][1]]")
             nil t))
    (should (search-forward
             (concat "#+caption: =" (nref "child2") "=  [[parent][1]]")
             nil t)))
  (with-temp-buffer
    (insert "#+name: parent1\n")
    (insert "#+caption: parent1\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert (concat (nref "child2") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child1") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child2") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; baz\n")
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: parent2\n"))
    (insert "#+caption: parent2\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (lilac-insert-noweb-source-code-block-captions nil)
    (goto-char (point-min))
    (should (search-forward
             (concat "#+caption: =" (nref "child1") "=  [[parent1][1]]  [[parent2][2]]")
             nil t))
    (should (search-forward
             (concat "#+caption: =" (nref "child2") "=  [[parent1][1]]")
             nil t))))

(provide 'lilac-tests)
