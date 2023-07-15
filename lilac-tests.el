(require 'ert)
(require 'lilac)

(org-mode)

(defun nref (s) (concat "__NREF__" s))
(defun lilac-temp-publish (fname-prefix content)
  (let* ((fname-org (make-temp-file
                    "fname-prefix"
                    nil
                    ".org"))
         (fname-html (concat (string-remove-suffix "org" fname-org) "html")))
    (with-temp-file fname-org
      (org-mode)
      (insert content))
    (find-file fname-org)
    (lilac-publish)
    fname-html))
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
             (concat "#+caption: ="
                     (nref "child1")
                     "=  [[parent1][1]]  [[parent2][2]]")
             nil t))
    (should (search-forward
             (concat "#+caption: =" (nref "child2") "=  [[parent1][1]]")
             nil t))))
(ert-deftest t-lilac-get-parent-blocks ()
  (with-temp-buffer
    (insert "#+name: foo\n")
    (insert "#+caption: foo\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+name: bar\n")
    (insert "#+caption: bar\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert "#+end_src\n")
    (should-not (lilac-get-parent-blocks)))
  (with-temp-buffer
    (insert "#+name: parent1\n")
    (insert "#+caption: parent1\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+name: parent2\n")
    (insert "#+caption: parent2\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert (concat (nref "child2") "\n"))
    (insert "#+end_src\n")
    (should (lilac-get-parent-blocks))))
(ert-deftest t-lilac-mk-child-parents-hash-table ()
  (with-temp-buffer
    (insert "#+name: foo\n")
    (insert "#+caption: foo\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+name: bar\n")
    (insert "#+caption: bar\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert "#+end_src\n")
    (let* ((parent-blocks (lilac-get-parent-blocks))
           (child-parents-hash-table
             (lilac-mk-child-parents-hash-table parent-blocks)))
      (should (equal (hash-table-count child-parents-hash-table) 0))))
  (with-temp-buffer
    (insert "#+name: parent1\n")
    (insert "#+caption: parent1\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+name: parent2\n")
    (insert "#+caption: parent2\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert (concat (nref "child2") "\n"))
    (insert "#+end_src\n")
    (let* ((parent-blocks (lilac-get-parent-blocks))
           (child-parents-hash-table
             (lilac-mk-child-parents-hash-table parent-blocks)))
      (should (equal (hash-table-count child-parents-hash-table) 2)))))
(ert-deftest t-lilac-mk-smart-captions ()
  (with-temp-buffer
    (insert "#+name: parent1\n")
    (insert "#+caption: parent1\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+name: parent2\n")
    (insert "#+caption: parent2\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert (concat (nref "child2") "\n"))
    (insert "#+end_src\n")
    (let* ((parent-blocks (lilac-get-parent-blocks))
           (child-parents-hash-table
             (lilac-mk-child-parents-hash-table parent-blocks))
           (smart-captions (lilac-mk-smart-captions
                            child-parents-hash-table)))
      (should (equal smart-captions nil))))
  (with-temp-buffer
    (insert "#+name: parent1\n")
    (insert "#+caption: parent1\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+name: parent2\n")
    (insert "#+caption: parent2\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child1") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; child1\n")
    (insert "#+end_src\n")
    (let* ((parent-blocks (lilac-get-parent-blocks))
           (child-parents-hash-table
             (lilac-mk-child-parents-hash-table parent-blocks))
           (smart-captions (lilac-mk-smart-captions
                            child-parents-hash-table)))
      (should (equal smart-captions
       `((181 . ,(concat "#+caption: ="
                         (nref "child1")
                         "=  [[parent1][1]]  [[parent2][2]]\n")))))))
  (with-temp-buffer
    (insert "#+name: parent1\n")
    (insert "#+caption: parent1\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert "#+name: parent2\n")
    (insert "#+caption: parent2\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; bar\n")
    (insert (concat (nref "child2") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child1") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; child1\n")
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child2") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; child2\n")
    (insert "#+end_src\n")
    (let* ((parent-blocks (lilac-get-parent-blocks))
           (child-parents-hash-table
             (lilac-mk-child-parents-hash-table parent-blocks))
           (smart-captions (lilac-mk-smart-captions
                            child-parents-hash-table)))
      (should (equal smart-captions
       `((181 . ,(concat "#+caption: ="
                         (nref "child1")
                         "=  [[parent1][1]]\n"))
         (247 . ,(concat "#+caption: ="
                         (nref "child2")
                         "=  [[parent2][1]]\n")))))))
  (with-temp-buffer
    (insert "#+name: parent1\n")
    (insert "#+caption: parent1\n")
    (insert "#+begin_src emacs-lisp\n")
    (insert "; foo\n")
    (insert (concat (nref "child1") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child1") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; child1\n")
    (insert (concat (nref "child2") "\n"))
    (insert "#+end_src\n")
    (insert "\n")
    (insert (concat "#+name: " (nref "child2") "\n"))
    (insert "#+begin_src emacs-lisp\n")
    (insert "; child2\n")
    (insert "#+end_src\n")
    (let* ((parent-blocks (lilac-get-parent-blocks))
           (child-parents-hash-table
             (lilac-mk-child-parents-hash-table parent-blocks))
           (smart-captions (lilac-mk-smart-captions
                            child-parents-hash-table)))
      (should (equal smart-captions
       `((91 . ,(concat "#+caption: ="
                         (nref "child1")
                         "=  [[parent1][1]]\n"))
         (172 . ,(concat "#+caption: ="
                         (nref "child2")
                         "=  [["
                         (nref "child1")
                         "][1]]\n"))))))))
(ert-deftest t-lilac-children-are-linked-from-parent ()
    (let* ((fname-html
            (lilac-temp-publish
             "t-lilac-children-are-linked-from-parent-"
             (concat
              "#+name: parent1\n"
              "#+caption: parent1\n"
              "#+begin_src emacs-lisp\n"
              "; foo\n"
              (concat (nref "child1") "\n")
              "#+end_src\n"
              "\n"
              "#+name: parent2\n"
              "#+caption: parent2\n"
              "#+begin_src emacs-lisp\n"
              "; bar\n"
              (concat (nref "child1") "\n")
              "#+end_src\n"
              "\n"
              (concat "#+name: " (nref "child1") "\n")
              "#+begin_src emacs-lisp\n"
              "; child1\n"
              "#+end_src\n")))
           (html (elquery-read-file fname-html))
           (got-child-link-text-parent1
            (elquery-text
             (car (elquery-$
                   "#parent1 .lilac-child-link-from-parent a"
                   html))))
           (got-child-link-text-parent2
            (elquery-text
             (car (elquery-$
                   "#parent2 .lilac-child-link-from-parent a"
                   html)))))
      (should (equal got-child-link-text-parent1 "child1"))
      (should (equal got-child-link-text-parent2 "child1"))))
(ert-deftest t-lilac-children-are-linked-from-parent ()
  (let* ((fname-html
          (lilac-temp-publish
           "t-lilac-children-are-linked-from-parent-nested-"
           (concat
            "#+name: parent1\n"
            "#+caption: parent1\n"
            "#+begin_src emacs-lisp\n"
            "; foo\n"
            (concat (nref "child1") "\n")
            "#+end_src\n"
            "\n"
            (concat "#+name: " (nref "child1") "\n")
            "#+begin_src emacs-lisp\n"
            "; child1\n"
            (concat (nref "nested-child") "\n")
            "#+end_src\n"
            "\n"
            (concat "#+name: " (nref "nested-child") "\n")
            "#+caption: nested-child\n"
            "#+begin_src emacs-lisp\n"
            "; nested-child\n"
            "#+end_src\n")))
         (html (elquery-read-file fname-html))
         (got-child-link-text-parent1
          (elquery-text
           (car (elquery-$
                 "#parent1 .lilac-child-link-from-parent a"
                 html))))
         (got-child-link-text-child1
          (elquery-text
           (car (elquery-$
                 (concat "#" (nref "child1")
                         " .lilac-child-link-from-parent a")
                 html)))))
      (should (equal got-child-link-text-parent1 "child1"))
      (should (equal got-child-link-text-child1 "nested-child"))))

(provide 'lilac-tests)
