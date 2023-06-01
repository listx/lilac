; Set garbage-collection threshold to 16 GiB.
(setq gc-cons-threshold #x400000000)
;; Built-in packages (distributed with Emacs).
(require 'elisp-mode)

;; Third-party packages (checked in as Git submodules)
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/s.el"))
(require 's)
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/compat.el"))
(require 'compat)
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/dash.el"))
(require 'dash)
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/dr-qubit.org"))
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/f.el"))
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/parsebib"))
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/citeproc-el"))
(require 'citeproc)
(require 'oc-csl)
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/emacs-htmlize"))
(require 'htmlize)
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/magit/lisp"))
(require 'magit-section)
(add-to-list 'load-path (concat (getenv "PWD") "/deps/elisp/nix-mode"))
(require 'nix-mode)
(setq org-export-time-stamp-file nil)
(setq org-html-postamble nil)
(defun org-export-deterministic-reference (references)
  (let ((new (length references)))
     (while (rassq new references) (setq new (1+ new)))
     new))
(advice-add #'org-export-new-reference
            :override #'org-export-deterministic-reference)
(setq org-babel-noweb-wrap-start "__NREF__")
(setq org-babel-noweb-wrap-end "")

(defun lilac-nref-rx (match-optional-params)
  (rx-to-string
   (lilac-nref-rx-primitive match-optional-params)))

(defun lilac-nref-rx-primitive (match-optional-params)
  (if match-optional-params
   `(group
           "__NREF__"
          ;; Noweb reference must start with a letter...
          (any alpha)
          ;; ...and must be followed by
          ;; letters,numbers,dashes,underscores,periods...
          (* (or (any alnum) "-" "_" "."))
          ;; ...and may terminate with a "(...)" where the "..." may be an empty
          ;; string, or some other argument.
          (* (or "()"
                 (and "("
                      (* (not ")"))
                      ")"))))
   `(group
          "__NREF__"
          (any alpha)
          (* (or (any alnum) "-" "_" ".")))))

;; Customize noweb delimiters. Unlike traditional << and >> delimiters, we just
;; use the "__NREF__" prefix as our only delimiter. This has the advantage of
;; being encoded the same way into HTML, which makes our HTML modifications
;; easier and more consistent across different source code languages.
;; See https://emacs.stackexchange.com/a/73720/13006.
(defun org-babel-noweb-wrap (&optional regexp)
  "Return regexp matching a Noweb reference.

Match any reference, or only those matching REGEXP, if non-nil.
When matching, reference is stored in match group 1."
  (lilac-nref-rx t))
(defun lilac-publish ()
  (interactive)
  (setq org-html-htmlize-output-type 'css)
  (setq org-export-before-parsing-hook
   '(lilac-UID-for-all-src-blocks
     lilac-insert-noweb-source-code-block-captions
     lilac-UID-for-all-headlines))

  (setq org-export-filter-src-block-functions
   '(lilac-populate-child-HTML_ID-hash-table
     lilac-populate-org_id-human_id-hash-table))
  (org-html-export-to-html)

  (clrhash lilac-polyblock-names-totals)

  (setq org-export-filter-src-block-functions
   '(lilac-link-to-children-from-parent-body
     lilac-prettify-source-code-captions))
  (setq org-export-filter-final-output-functions
   '(lilac-replace-org_ids-with-human_ids))
  (org-html-export-to-html))

;; Modify Org buffer
(defun lilac-UID-for-all-src-blocks (_backend)
  (let* ((all-src-blocks
           (org-element-map (org-element-parse-buffer) 'src-block 'identity))
         (counter 0)
         (auto-names
           (-remove 'null
             (cl-loop for src-block in all-src-blocks collect
               (let* ((pos (org-element-property :begin src-block))
                      (parent-name-struct (lilac-get-src-block-name src-block))
                      (direct-name (org-element-property :name src-block))
                      (no-direct-name (s-blank? direct-name))
                      (prefix
                        (cond ((s-blank? (car parent-name-struct))
                               "___anonymous-src-block")
                              (t
                               (car parent-name-struct))))
                      (name-final
                       (format "#+name: %s-%x\n" prefix counter)))
                 (setq counter (1+ counter))
                 (when no-direct-name
                   (cons pos name-final)))))))
    (lilac-insert-strings-into-buffer auto-names)))
(defun lilac-insert-noweb-source-code-block-captions (_backend)
  (let* ((parent-blocks
           (lilac-get-parent-blocks))
         (child-parents-hash-table
           (lilac-mk-child-parents-hash-table parent-blocks))
         (all-src-blocks
           (org-element-map (org-element-parse-buffer) 'src-block 'identity))
         (smart-captions
           (lilac-mk-smart-captions child-parents-hash-table)))
    (lilac-insert-strings-into-buffer smart-captions)))

(defun lilac-is-parent-block (src-block)
  (let ((body (org-element-property :value src-block)))
    (lilac-get-noweb-children body)))
(defun lilac-get-parent-blocks ()
  (org-element-map (org-element-parse-buffer) 'src-block
    (lambda (src-block)
       (if (lilac-is-parent-block src-block) src-block))))
(defun lilac-mk-child-parents-hash-table (parent-blocks)
  (let ((hash-table (make-hash-table :test 'equal)))
    (mapc
     (lambda (parent-block)
      (let* ((parent-name (org-element-property :name parent-block))
             (parent-body (org-element-property :value parent-block))
             (child-names (lilac-get-noweb-children parent-body)))
        (mapc (lambda (child-name)
                (let* ((parents (gethash child-name hash-table)))
                  (if parents
                    (puthash child-name
                             (cl-pushnew parent-name parents)
                             hash-table)
                    (puthash child-name (list parent-name) hash-table))))
              child-names)))
     (reverse parent-blocks))
    hash-table))
(defun lilac-mk-smart-captions (child-parents-hash-table)
  (-remove 'null
    (cl-loop for src-block in (org-element-map (org-element-parse-buffer) 'src-block 'identity) collect
      (let* ((child (lilac-get-src-block-name src-block))
             (child-name (car child))
             (NSCB_NAME (format "=%s= " child-name))                  ;ref:NSCB_NAME
             (NSCB_POLYBLOCK_INDICATOR                                ;ref:NSCB_POLYBLOCK_INDICATOR
               (if (lilac-get-noweb-ref-polyblock-name src-block)
                   "(polyblock)"
                 ""))
             (polyblock-counter
              (gethash child-name lilac-polyblock-names-totals 0))
             (polyblock-counter-incremented
              (puthash child-name (1+ polyblock-counter)
                       lilac-polyblock-names-totals))
             (parents (gethash child-name child-parents-hash-table))
             (parents-zipped (lilac-enumerate parents))
             (pos (org-element-property :begin src-block))
             (NSCB_LINKS_TO_PARENTS                                   ;ref:NSCB_LINKS_TO_PARENTS
              (mapconcat (lambda (parent-with-idx)
                           (format " [[%s][%d]]"
                                   (nth 1 parent-with-idx)
                                   (1+ (nth 0 parent-with-idx))))
                         parents-zipped " "))
             (smart-caption
              (concat
                "#+caption: "
                NSCB_NAME
                NSCB_POLYBLOCK_INDICATOR
                NSCB_LINKS_TO_PARENTS
                "\n")))
        (when parents (cons pos smart-caption))))))
(defun lilac-insert-strings-into-buffer (pos-strings)
  (cl-loop for pos-string in (reverse pos-strings) do
        (let ((pos (car pos-string))
              (str (cdr pos-string)))
          (goto-char pos)
          (insert str))))
(defun lilac-get-noweb-children (s)
  (let* ((lines (split-string s "\n"))
         (refs (-remove 'null
                 (mapcar
                  (lambda (line)
                   (if (string-match (lilac-nref-rx nil) line)
                       (match-string-no-properties 1 line)))
                  lines))))
    refs))
(defun lilac-get-noweb-ref-polyblock-name (source-code-block)
  (let* ((headers (org-element-property :header source-code-block))
         (noweb-ref-name
          (nth 0
           (-remove 'null
            (mapcar
             (lambda (header)
               (if (string-match ":noweb-ref \\(.+\\)" header)
                   (match-string-no-properties 1 header)))
             headers)))))
    noweb-ref-name))
(defun lilac-get-src-block-name (src-block)
  (let* ((name-direct (org-element-property :name src-block))
         (name-indirect (lilac-get-noweb-ref-polyblock-name src-block)))
    (if name-indirect
        `(,name-indirect "(polyblock)")
        `(,name-direct ""))))
(defun lilac-enumerate (lst &optional start)
  (let ((ret ()))
    (cl-loop for index from (if start start 0)
           for item in lst
           do (push (list index item) ret))
    (reverse ret)))

; See https://emacs.stackexchange.com/a/7150.
(defun lilac-matches (regexp s &optional group)
  "Get a list of all regexp matches in a string"
  (if (= (length s) 0)
      ()
      (save-match-data
        (let ((pos 0)
              (matches ()))
          (while (string-match regexp s pos)
            (push (match-string (if group group 0) s) matches)
            (setq pos (match-end 0)))
          (reverse matches)))))
(defun lilac-UID-for-all-headlines (_backend)
  (let* ((all-headlines
           (org-element-map (org-element-parse-buffer) 'headline 'identity))

         (headline-uid-hash-table (make-hash-table :test 'equal))
         (headline-UIDs
           (-remove 'null
             (cl-loop for headline in all-headlines collect
               (let* ((headline-UID
                       (lilac-get-unique-id headline headline-uid-hash-table))
                      ;; Get the position just after the headline (just
                      ;; underneath it).
                      (pos (progn
                             (goto-char (org-element-property :begin headline))
                             (re-search-forward "\n"))))
                 (cons pos (concat
                            ":PROPERTIES:\n"
                            ":CUSTOM_ID: " headline-UID "\n"
                            ":END:\n")))))))
    (lilac-insert-strings-into-buffer headline-UIDs)))

(defun lilac-get-unique-id (headline hash-table)
  (let* ((name (org-element-property :raw-value headline))
         (disambiguation-number 0)
         (key (concat "h-" (lilac-normalize-string name)))
         (val (gethash key hash-table)))
    ;; Discard the key if a value already exists. This drives up the
    ;; disambiguation number.
    (while val
      (setq disambiguation-number (1+ disambiguation-number))
      (setq key (concat "h-"
                        (lilac-normalize-string
                         (format "%s-%s" name disambiguation-number))))
      (setq val (gethash key hash-table)))
    (puthash key t hash-table)
    key))

(defun lilac-normalize-string (s)
  (string-trim
    (replace-regexp-in-string "[^A-Za-z0-9]" "-" s)
    "-"
    "-"))

;; Modify HTML
; Define a global hash table for mapping child source block names to their HTML
; IDs.
(setq lilac-child-HTML_ID-hash-table (make-hash-table :test 'equal))

(defun lilac-populate-child-HTML_ID-hash-table (src-block-html backend info)
  (when (org-export-derived-backend-p backend 'html)
    (let* ((child-name (lilac-get-src-block-name-from-html src-block-html))
           (child-HTML_ID (lilac-get-src-block-HTML_ID src-block-html))
           (child-HTML_ID-exists-already
            (gethash child-name lilac-child-HTML_ID-hash-table nil)))
      ; Skip blocks that lack an HTML ID.
      (if (and child-HTML_ID (not child-HTML_ID-exists-already))
        (puthash child-name child-HTML_ID lilac-child-HTML_ID-hash-table))
      ; Return src-block-html as-is (no modifications).
      src-block-html)))

(defun lilac-get-src-block-name-from-html (src-block-html)
  (let* ((match-nref (string-match
                      (concat
                       "<label.+?<code>"
                       (lilac-nref-rx nil)
                       "</code>")
                      src-block-html))
         (match-raw (if (not match-nref)
                        (string-match
                         (rx-to-string
                          '(and
                            "<label"
                            (+ (not ">"))
                            ">"
                            (group (*? anychar))
                            "</label>"))
                         src-block-html)))
         (matched-contents (match-string-no-properties 1 src-block-html)))
    (if match-nref
        matched-contents
        (if match-raw
            (lilac-clean-up-match-raw matched-contents)))))

(defun lilac-clean-up-match-raw (s)
  (let* ((normalized (lilac-normalize-string s))
         (rx (rx-to-string
                '(and
                  "Listing-"
                  (+ (any digit))
                  (+ "-")
                  "span"
                  (* "-")
                  (group (+ anychar)))))
         (match (string-match rx normalized)))
    (if match
        (match-string-no-properties 1 normalized)
        normalized)))

;ref:lilac-get-src-block-HTML_ID
(defun lilac-get-src-block-HTML_ID (src-block-html)
  (let ((match (string-match "<pre [^>]+?id=\"\\([^\"]+\\)\">" src-block-html)))
    (if match (match-string-no-properties 1 src-block-html))))

(defun lilac-link-to-children-from-parent-body (src-block-html backend info)
  (when (org-export-derived-backend-p backend 'html)
    ;; Break up source block into 3 subparts --- the leading <div ...>, the
    ;; <label ...></label> (if any) and <pre ...></pre>.
    ;; Then run the linkifying logic against only the body, and then return the
    ;; original label and new body.
    (let* ((div-caption-body (lilac-get-source-block-html-parts-without-newlines
                              src-block-html))
           (leading-div (nth 0 div-caption-body))
           (caption (nth 1 div-caption-body))
           (body (nth 2 div-caption-body))
           (body-linkified-without-newlines
            (replace-regexp-in-string
             (lilac-nref-rx nil)
             (lambda (child-name-text)
                 (let* ((HTML_ID (gethash child-name-text
                                          lilac-child-HTML_ID-hash-table)))
                  (if HTML_ID
                      (concat "<span class=\"lilac-child-link-from-parent\">"
                              "<a href=\"#" HTML_ID "\">"
                              (string-remove-prefix "__NREF__" child-name-text)
                              "</a></span>")
                      child-name-text)))
             body))
           (body-linkified-with-newlines
            (lilac-to-multi-line body-linkified-without-newlines)))
      (concat leading-div caption body-linkified-with-newlines "</div>"))))

(defun lilac-to-single-line (s)
  (replace-regexp-in-string "\n" "<<<LILAC_NEWLINE>>>" s))

(defun lilac-to-multi-line (s)
  (replace-regexp-in-string "<<<LILAC_NEWLINE>>>" "\n" s))
(setq lilac-polyblock-names (make-hash-table :test 'equal))
(setq lilac-polyblock-names-totals (make-hash-table :test 'equal))

(defun lilac-prettify-source-code-captions (src-block-html backend info)
  (when (org-export-derived-backend-p backend 'html)
    ;; Break up source block into 3 subparts --- the leading <div ...>, the
    ;; <label ...></label> (if any) and <pre ...></pre>.
    ;; Then run the linkifying logic against only the body, and then return the
    ;; original label and new body.
    (let* ((div-caption-body (lilac-get-source-block-html-parts-without-newlines
                              src-block-html))
           (leading-div (nth 0 div-caption-body))
           (body (nth 2 div-caption-body))
           (pre-id-match
             (string-match
               (rx-to-string
                 '(and
                       "<pre "
                       (* (not ">"))
                       "id=\""
                       (group (+ (not "\"")))))
               body))
           (pre-id
             (if pre-id-match
                 (match-string-no-properties 1 body)
                 "#deadlink"))
           (body-with-newlines
            (lilac-to-multi-line body))
           (caption (nth 1 div-caption-body))
           ; caption-parts just captures whatever substring is inside the
           ; <label> tags.
           (caption-parts
             (let* ((caption-match
                      (string-match "<label [^>]+>\\(.*?\\)</label>" caption)))
               (if caption-match
                   (match-string-no-properties 1 caption)
                   "")))
           (source-block-name-match
             (string-match
               (rx-to-string
                 '(and
                       "<code>"
                       (group (+ (not "<")))
                       "</code>"))
               caption-parts))
           ;; A source code block is anonymous if: (1) it does not have a
           ;; "#+name: ..." line, or (2) it does not have a "#+header:
           ;; :noweb-ref ..." line.
           (source-block-name
             (if source-block-name-match
                 (match-string-no-properties 1 caption-parts)
                 "anonymous"))
           ;; This is just used for the side effect of recording the
           ;; source-block-name, to be used for the fallback-id.
           (source-block-counter
            (gethash source-block-name lilac-polyblock-names 0))
           (source-block-counter-incremented
            (puthash source-block-name (1+ source-block-counter)
                     lilac-polyblock-names))
           (source-block-name-styled
             (cond ((string-prefix-p "__NREF__" source-block-name)
                    (concat
                      "<span class=\"lilac-caption-source-code-block-name\">"
                      (string-remove-prefix "__NREF__" source-block-name)
                      "</span>"))
                   (t
                    (concat
                      "<span class=\"lilac-caption-source-code-block-name\">"
                      "&#x1f4c4; "
                      source-block-name
                      "</span>"))))
           (polyblock-chain-total
            (gethash source-block-name lilac-polyblock-names-totals 0))
           (polyblock-chain-location
            (if (= polyblock-chain-total 0)
                ""
              (format "(%s/%s) "
                      source-block-counter-incremented polyblock-chain-total)))
           (polyblock-indicator
             (if (string-match "\(polyblock\)" caption-parts)
                 polyblock-chain-location ""))
           (parent-id-regexp
               (rx-to-string
                 '(and
                       " <a href=\""
                       (group (+ (not "\""))))))
           (parent-ids-with-idx
            (lilac-enumerate
             (lilac-matches parent-id-regexp caption-parts 1) 1))
           (parent-links
             (mapconcat (lambda (parent-id-with-idx)
                          (let ((parent-id (car (cdr parent-id-with-idx)))
                                (idx (car parent-id-with-idx)))
                             (format (concat
                                      "<span class="
                                      "\"lilac-caption-parent-link\">"
                                      "<a href=\"%s\">%s</a></span>")
                               parent-id
                               (if (= idx 1)
                                   (string-remove-prefix
                                    "__NREF__" source-block-name)
                                 idx))))
                        parent-ids-with-idx ""))
           ;; For polyblocks, only the first (head) block gets an id field for a
           ;; <pre> tag. The rest (tail) don't have this field so they would
           ;; normally get assigned a deadlink. To avoid this, use a counter for
           ;; the parent-id, because this parent-id is shared across all
           ;; polyblocks. Then use this with the parent-id to generate an
           ;; alternate, fallback-id. This way the tail polyblocks get assigned
           ;; a unique (meaningful) ID and not just "##deadlink".
           (fallback-id
             (if (string= pre-id "#deadlink")
                 (format "%s-%s"
                         source-block-name
                         source-block-counter-incremented)
                 pre-id))
           (pre-tag-match
             (string-match
               (rx-to-string
                 '(and
                       "<pre "
                       (group (* (not ">")))
                       ">"))
               body))
           (pre-tag-entire (match-string-no-properties 0 body))
           (pre-tag-contents (match-string-no-properties 1 body))
           (body-with-replaced-pre
             (if pre-id-match
                 body-with-newlines
                 (string-replace pre-tag-entire
                                 (concat "<pre " pre-tag-contents
                                         (format " id=\"%s\"" fallback-id) ">")
                                 body-with-newlines)))
           (link-symbol
             (format (concat "<span class=\"lilac-caption-link-symbol\">"
                             "<a href=\"#%s\">&#x1f517;</a></span>")
               fallback-id))
           (caption-without-listing-prefix
            (replace-regexp-in-string "<span.+?span>" "" caption))
           (caption-text
            (if (> (length parent-links) 0)
                (concat
                  "<div class=\"lilac-caption\">"
                    parent-links
                    polyblock-indicator
                    link-symbol
                  "</div>")
                (concat
                  "<div class=\"lilac-caption\">"
                    caption-without-listing-prefix
                    link-symbol
                  "</div>")))
           )
      (if (s-blank? caption)
       src-block-html
       (concat
        leading-div
          "<div class=\"lilac-pre-with-caption\">"
            caption-text
            body-with-replaced-pre
          "</div>"
        "</div>")))))

(defun lilac-get-source-block-html-parts-without-newlines (src-block-html)
    (let* ((one-line (lilac-to-single-line src-block-html))
           (leading-div
             (let ((div-match
                    (string-match "<div [^>]+>" one-line)))
               (match-string-no-properties 0 one-line)))
           (caption
             (let* ((caption-match
                      (string-match "<label [^>]+>.*?</label>" one-line)))
               (if caption-match
                   (match-string-no-properties 0 one-line)
                   "")))
           (body (progn (string-match "<pre [^>]+>.*?</pre>" one-line)
                        (match-string-no-properties 0 one-line))))
      `(,leading-div ,caption ,body)))
(setq lilac-org_id-human_id-hash-table (make-hash-table :test 'equal))
(setq lilac-human_id-count-hash-table (make-hash-table :test 'equal))
(setq lilac-human_id-org_id-hash-table (make-hash-table :test 'equal))

(defun lilac-populate-org_id-human_id-hash-table (src-block-html backend info)
  (when (org-export-derived-backend-p backend 'html)
    (let* ((block-name (lilac-get-src-block-name-from-html src-block-html))
           (block-name-count (gethash block-name
                                      lilac-human_id-count-hash-table
                                      0))
           (orgid (lilac-get-src-block-HTML_ID src-block-html)))
      (when orgid
        (puthash block-name
                 (1+ block-name-count)
                 lilac-human_id-count-hash-table)
        (cond ((= block-name-count 0)
                (progn
                  (puthash orgid
                           block-name
                           lilac-org_id-human_id-hash-table)
                  (puthash block-name
                           orgid
                           lilac-human_id-org_id-hash-table)))
              ((= block-name-count 1)
                (let* ((orgid-first-block
                        (gethash block-name lilac-human_id-org_id-hash-table)))
                  (puthash orgid-first-block
                           (format "%s-1" block-name)
                           lilac-org_id-human_id-hash-table)
                  (puthash orgid
                           (format "%s-%d" block-name (1+ block-name-count))
                           lilac-org_id-human_id-hash-table)))
              (t
                 (puthash orgid
                          (format "%s-%d" block-name (1+ block-name-count))
                          lilac-org_id-human_id-hash-table))))
      src-block-html)))

(defun lilac-replace-org_ids-with-human_ids (entire-html backend info)
  (when (org-export-derived-backend-p backend 'html)
    (let ((html-oneline (lilac-to-single-line entire-html)))
      (maphash
       (lambda (k v)
        (when (and k v)
         (setq html-oneline
               (replace-regexp-in-string
                (rx-to-string `(and " id=" (* (not "\"")) "\"" ,k "\""))
                (format " id=\"%s\"" v) html-oneline))
         (setq html-oneline
               (replace-regexp-in-string
                (rx-to-string `(and " href=" (* (not "\"")) "\"#" ,k "\""))
                (format " href=\"#%s\"" v) html-oneline))))
       lilac-org_id-human_id-hash-table)
      (lilac-to-multi-line html-oneline))))
(defun lilac-gen-css-and-exit ()
  (font-lock-flush)
  (font-lock-fontify-buffer)
  (org-html-htmlize-generate-css)
  (with-current-buffer "*html*"
    (write-file "syntax-highlighting.css"))
  (kill-emacs))

;; Without this, batch-org-gen-css-and-exit produces a near-empty CSS file.
(require 'font-lock)
(require 'subr-x) ;; for `when-let'

(unless (boundp 'maximal-integer)
  (defconst maximal-integer (lsh -1 -1)
    "Maximal integer value representable natively in emacs lisp."))

(defun face-spec-default (spec)
  "Get list containing at most the default entry of face SPEC.
Return nil if SPEC has no default entry."
  (let* ((first (car-safe spec))
     (display (car-safe first)))
    (when (eq display 'default)
      (list (car-safe spec)))))

(defun face-spec-min-color (display-atts)
  "Get min-color entry of DISPLAY-ATTS pair from face spec."
  (let* ((display (car-safe display-atts)))
    (or (car-safe (cdr (assoc 'min-colors display)))
    maximal-integer)))

(defun face-spec-highest-color (spec)
  "Search face SPEC for highest color.
That means the DISPLAY entry of SPEC
with class 'color and highest min-color value."
  (let ((color-list (cl-remove-if-not
             (lambda (display-atts)
               (when-let ((display (car-safe display-atts))
                  (class (and (listp display)
                          (assoc 'class display)))
                  (background (assoc 'background display)))
             (and (member 'light (cdr background))
                  (member 'color (cdr class)))))
             spec)))
    (cl-reduce (lambda (display-atts1 display-atts2)
         (if (> (face-spec-min-color display-atts1)
            (face-spec-min-color display-atts2))
             display-atts1
           display-atts2))
           (cdr color-list)
           :initial-value (car color-list))))

(defun face-spec-t (spec)
  "Search face SPEC for fall back."
  (cl-find-if (lambda (display-atts)
        (eq (car-safe display-atts) t))
          spec))

; This is slightly tweaked from the original, because the incoming "face" value
; can look like (fixed-pitch face-name) --- so we take the second element.
(defun my-face-attribute (face attribute &optional frame inherit)
  "Get FACE ATTRIBUTE from `face-user-default-spec' and not from
`face-attribute'."
  (let*
    ((face-spec (face-user-default-spec (if (listp face)
                                            (car (cdr face))
                                          face)))
     (display-attr (or (face-spec-highest-color face-spec)
               (face-spec-t face-spec)))
     (attr (cdr display-attr))
     (val (or (plist-get attr attribute)
              (car-safe (cdr (assoc attribute attr))))))
    (when (and (null (eq attribute :inherit))
           (null val))
      (let ((inherited-face (my-face-attribute face :inherit)))
    (when (and inherited-face
           (null (eq inherited-face 'unspecified)))
      (setq val (my-face-attribute inherited-face attribute)))))
    (or val 'unspecified)))

(advice-add 'face-attribute :override #'my-face-attribute)
(setq org-html-doctype "html5")
(setq org-html-head-include-scripts t)
(setq org-src-preserve-indentation t)
(setq-default tab-width 4)
(setq make-backup-files nil)
(defun lilac-publish-profile ()
  (interactive)
  (profiler-start 'cpu)
  (lilac-publish)
  (profiler-stop)
  (profiler-report)
  (profiler-report-write-profile "emacs-profile-weave.txt") t)

(defun lilac-tangle-profile ()
  (interactive)
  (profiler-start 'cpu)
  (org-babel-tangle)
  (profiler-stop)
  (profiler-report)
  (profiler-report-write-profile "emacs-profile-tangle.txt") t)
(provide 'lilac)
