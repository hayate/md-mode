;;; md-nav-tests.el --- Tests for navigation features  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'md-mode)
(require 'md-link)
(require 'md-outline)

(defconst md-nav-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst md-nav-test--project
  (expand-file-name ".." md-nav-test--directory)
  "The md-mode checkout, which the fixtures resolve references against.")

(defmacro md-nav-test--rendered (file &rest body)
  "Render FILE from the fixture directory and run BODY in its view."
  (declare (indent 1))
  `(let ((source (find-file-noselect
                  (expand-file-name ,file md-nav-test--directory))))
     (unwind-protect
         (progn
           (with-current-buffer source (md--show-render))
           (with-current-buffer (buffer-local-value 'md--view-buffer source)
             ,@body))
       (when (buffer-live-p source) (kill-buffer source)))))

;;; Recognising references

(ert-deftest md-link-parses-real-references ()
  (should (equal (md-link-parse "md-parse.el:1") '("md-parse.el" . 1)))
  (should (equal (md-link-parse "common/views/webhook.py:507")
                 '("common/views/webhook.py" . 507)))
  (should (equal (md-link-parse "md-mode.el") '("md-mode.el")))
  (should (equal (md-link-parse "  spaced.el:2  ") '("spaced.el" . 2))))

(ert-deftest md-link-rejects-things-that-only-look-like-references ()
  "Scanning technical prose finds plenty that is not a file."
  (dolist (text '("12:30"                 ; a time, not file 12 line 30
                  "http://localhost:3000" ; a URL with a port
                  "https://x/foo.py:507"  ; still a URL
                  "{\"key\": 42}"         ; object syntax
                  "foo: 507"              ; a mapping, note the space
                  "C:\\src\\foo.py:507"   ; a Windows path
                  ":513"                  ; a bare line number
                  ""
                  "just words here"))
    (should-not (md-link-parse text))))

;;; Resolving them

(ert-deftest md-link-resolves-inside-the-project ()
  (should (equal (md-link-resolve "md-parse.el" md-nav-test--directory)
                 (file-truename (expand-file-name "md-parse.el" md-nav-test--project))))
  (should (equal (md-link-resolve "test/corpus.md" md-nav-test--directory)
                 (file-truename (expand-file-name "test/corpus.md" md-nav-test--project)))))

(ert-deftest md-link-refuses-to-escape-its-roots ()
  "An untrusted document must not turn any local path into a link."
  (dolist (path '("../../../../etc/passwd"
                  "/etc/passwd"
                  "~/.authinfo"
                  "../../../../../../../../etc/hosts"))
    (should-not (md-link-resolve path md-nav-test--directory))))

(ert-deftest md-link-refuses-remote-names ()
  "Asking whether /ssh:host:/x is readable would open a connection."
  (should-not (md-link-resolve "/ssh:nonexistent.invalid:/x.el" md-nav-test--directory))
  (should-not (md-link-resolve "md-parse.el" "/ssh:nonexistent.invalid:/docs/")))

(ert-deftest md-link-resolution-is-cached ()
  (clrhash md-link--cache)
  (should (md-link-resolve "md-parse.el" md-nav-test--directory))
  (let ((count (hash-table-count md-link--cache)))
    (md-link-resolve "md-parse.el" md-nav-test--directory)
    (should (= count (hash-table-count md-link--cache)))))

;;; In a rendered document

(ert-deftest md-link-linkifies-only-what-resolves ()
  (md-nav-test--rendered "docs/design.md"
    (let ((linked '()))
      (let ((pos (point-min)))
        (while (setq pos (next-single-property-change pos 'md-link-file))
          (when (get-text-property pos 'md-link-file)
            (let ((end (next-single-property-change pos 'md-link-file)))
              (push (buffer-substring-no-properties pos end) linked)
              (setq pos end)))))
      (setq linked (nreverse linked))
      ;; These exist in the checkout.
      (should (member "md-parse.el:1" linked))
      (should (member "md-render.el:100" linked))
      ;; These live in another repository, so they stay plain text.
      (should-not (member "common/views/webhook.py:507" linked))
      (should-not (member "ops/libs/esim_email.py:147" linked))
      ;; And these are not references at all.
      (dolist (text '("12:30" "a.b.c" "https://x.example:8080/p"))
        (should-not (member text linked))))))

(ert-deftest md-link-links-carry-our-keymap ()
  "Binding `shr-map' during the render is what puts it there."
  (md-nav-test--rendered "docs/design.md"
    (goto-char (point-min))
    (should (search-forward "the QA notes" nil t))
    (let ((pos (match-beginning 0)))
      (should (eq (get-text-property pos 'keymap) md-link-map))
      ;; Inheriting shr-map keeps its own commands working.
      (should (eq (lookup-key md-link-map (kbd "RET")) #'md-link-follow))
      (should (eq (keymap-parent md-link-map) (default-value 'shr-map))))))

(ert-deftest md-link-identifies-local-targets ()
  (md-nav-test--rendered "docs/design.md"
    (should (md-link--local-target "./notes.md"))
    (should-not (md-link--local-target "./nope.md"))
    (should-not (md-link--local-target "https://example.com/x"))
    (should (md-link--has-scheme-p "https://example.com/x"))
    (should-not (md-link--has-scheme-p "./notes.md"))))

;;; Outline

(ert-deftest md-outline-finds-headings-with-levels ()
  (md-nav-test--rendered "docs/design.md"
    (let ((headings (md-outline-headings)))
      (should (equal (mapcar (lambda (h) (nth 0 h)) headings) '(1 2 2 2)))
      (should (equal (nth 1 (car headings)) "Design: the navigation features")))))

(ert-deftest md-outline-builds-a-nested-imenu-index ()
  (md-nav-test--rendered "docs/design.md"
    (let ((index (md-outline-imenu-index)))
      (should (= (length index) 1))
      ;; The level-1 heading owns the level-2 ones.
      (should (consp (cdr (car index))))
      (should (assoc "Where the code lives" (cdr (car index)))))))

(ert-deftest md-outline-folds-survive-a-re-render ()
  "A resize re-renders, and a fold is an overlay on erased text."
  (let ((source (find-file-noselect
                 (expand-file-name "docs/design.md" md-nav-test--directory))))
    (unwind-protect
        (progn
          (with-current-buffer source (md--show-render))
          (let ((view (buffer-local-value 'md--view-buffer source)))
            (with-current-buffer view
              (goto-char (point-min))
              (should (outline-on-heading-p))
              (outline-hide-subtree)
              (let ((folded (md-outline-folded-headings)))
                (should folded)
                (md--render-into view source 70)
                (should (equal (md-outline-folded-headings) folded))))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest md-outline-imenu-cache-is-dropped-on-re-render ()
  "Stale markers all collapse to point-min, so the cache must go."
  (let ((source (find-file-noselect
                 (expand-file-name "docs/design.md" md-nav-test--directory))))
    (unwind-protect
        (progn
          (with-current-buffer source (md--show-render))
          (let ((view (buffer-local-value 'md--view-buffer source)))
            (with-current-buffer view
              (setq imenu--index-alist '(("stale" . 1)))
              (md--render-into view source 70)
              (should (null imenu--index-alist)))))
      (when (buffer-live-p source) (kill-buffer source)))))

;;; Side by side

(ert-deftest md-sync-keeps-both-sides-on-one-anchor ()
  (let ((source (find-file-noselect
                 (expand-file-name "corpus.md" md-nav-test--directory))))
    (unwind-protect
        (progn
          (with-current-buffer source (md--show-render))
          (let ((view (buffer-local-value 'md--view-buffer source)))
            (md--sync-enable source view)
            (md--sync-enable view source)
            ;; Move in the source; the view follows.
            (with-current-buffer source
              (goto-char (point-min))
              (forward-line 43)
              (md--sync-post-command))
            (with-current-buffer view
              (should (equal (md-render-source-line (point)) 44)))
            ;; Bouncing the sync must not make point crawl: the mapping is
            ;; lossy, so only a changed anchor may move anything.
            (dotimes (_ 20)
              (with-current-buffer view (md--sync-post-command))
              (with-current-buffer source (md--sync-post-command)))
            (with-current-buffer source
              (should (equal (line-number-at-pos) 44)))
            (with-current-buffer view
              (should (equal md--sync-anchor 44)))
            (md--sync-disable source)
            (md--sync-disable view)
            (should-not (buffer-local-value 'md--sync-peer source))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest md-sync-stops-when-the-view-dies ()
  (let ((source (find-file-noselect
                 (expand-file-name "corpus.md" md-nav-test--directory))))
    (unwind-protect
        (progn
          (with-current-buffer source (md--show-render))
          (let ((view (buffer-local-value 'md--view-buffer source)))
            (md--sync-enable source view)
            (md--sync-enable view source)
            (kill-buffer view)
            ;; The source may still have a hook, but it must not error.
            (with-current-buffer source (md--sync-post-command))))
      (when (buffer-live-p source) (kill-buffer source)))))

;;; History

(ert-deftest md-link-history-walks-between-documents ()
  (let ((design (find-file-noselect
                 (expand-file-name "docs/design.md" md-nav-test--directory))))
    (unwind-protect
        (progn
          (with-current-buffer design (md--show-render))
          ;; Be genuinely in the view: `with-current-buffer' restores the
          ;; previous buffer, so the render above left us elsewhere.
          (switch-to-buffer (buffer-local-value 'md--view-buffer design))
          (goto-char (point-min))
          (should (search-forward "the QA notes" nil t))
          (goto-char (match-beginning 0))
          (md-link-follow)
          (should (equal (buffer-name) "*md: notes.md*"))
          (should (= (length md--history) 1))
          (md-link-back)
          (should (equal (buffer-name) "*md: design.md*"))
          (should (= (length md--forward) 1))
          (md-link-forward)
          (should (equal (buffer-name) "*md: notes.md*")))
      (dolist (name '("design.md" "notes.md" "*md: design.md*" "*md: notes.md*"))
        (when-let ((buffer (get-buffer name)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(provide 'md-nav-tests)
;;; md-nav-tests.el ends here

(ert-deftest md-outline-search-fallback-matches-the-builtin ()
  "The fallback runs where `outline-search-level' is unavailable.
It has to find the same headings, at the same levels, as the built-in."
  (md-nav-test--rendered "docs/design.md"
    (let ((builtin '()) (fallback '()))
      (cl-flet ((walk (search)
                  (let ((found '()))
                    (save-excursion
                      (goto-char (point-min))
                      (let ((outline-search-function search))
                        (while (outline-next-heading)
                          (push (cons (funcall outline-level)
                                      (buffer-substring-no-properties
                                       (point) (line-end-position)))
                                found))))
                    (nreverse found))))
        (setq fallback (walk #'md-outline--search))
        (when (fboundp 'outline-search-level)
          (setq builtin (walk #'outline-search-level))
          (should (equal fallback builtin)))
        (should (= (length fallback) 3))))))
