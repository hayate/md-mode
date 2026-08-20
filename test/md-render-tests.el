;;; md-render-tests.el --- Tests for the Markdown renderer  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'md-render)
(require 'md-mode)

(defconst md-render-test--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defmacro md-render-test--with (markdown &rest body)
  "Render MARKDOWN into a temporary buffer and run BODY there."
  (declare (indent 1))
  `(with-temp-buffer
     (md-render-dom (md-parse-string ,markdown) md-render-test--directory 60)
     ,@body))

(defun md-render-test--faces-at (position)
  "All faces in effect at POSITION, as a list."
  (let ((face (get-text-property position 'face)))
    (cond ((null face) nil)
          ((listp face) face)
          (t (list face)))))

(defun md-render-test--search (string)
  "Move to STRING, failing the test if it is absent."
  (goto-char (point-min))
  (should (search-forward string nil t))
  (match-beginning 0))

;;; The whole corpus

(ert-deftest md-render-corpus-without-error ()
  "Every construct in the corpus renders, and the result is not empty."
  (let ((markdown (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "corpus.md" md-render-test--directory))
                    (buffer-string))))
    (md-render-test--with markdown
      (should (> (buffer-size) 500))
      ;; Front matter is hidden, its delimiters do not leak through.
      (should-not (save-excursion (search-forward "tags: [render, test]" nil t)))
      ;; Markup characters are gone: this is a document, not source.
      (goto-char (point-min))
      (should-not (search-forward "**bold**" nil t))
      (goto-char (point-min))
      (should (search-forward "Heading one" nil t)))))

;;; Headings and text

(ert-deftest md-render-headings-are-scaled ()
  (md-render-test--with "# Title\n\nBody\n"
    (should (memq 'shr-h1 (md-render-test--faces-at
                           (md-render-test--search "Title"))))))

(ert-deftest md-render-emphasis-becomes-face-not-markup ()
  (md-render-test--with "a **b** c\n"
    (should (memq 'bold (md-render-test--faces-at
                         (md-render-test--search "b"))))))

;;; Blockquotes

(ert-deftest md-render-blockquote-has-a-bar ()
  (md-render-test--with "> quoted\n"
    (let ((prefix (get-text-property (md-render-test--search "quoted") 'line-prefix)))
      (should prefix)
      (should (equal (substring-no-properties prefix) "│ ")))))

(ert-deftest md-render-nested-blockquotes-stack-bars ()
  (md-render-test--with "> outer\n>\n> > inner\n"
    (let ((prefix (get-text-property (md-render-test--search "inner") 'line-prefix)))
      (should (equal (substring-no-properties prefix) "│ │ ")))))

;;; Code

(ert-deftest md-render-code-block-is-fixed-pitch ()
  (md-render-test--with "```\nplain code\n```\n"
    (should (memq 'md-code-block (md-render-test--faces-at
                                  (md-render-test--search "plain code"))))))

(ert-deftest md-render-code-block-is-highlighted ()
  "A fence with a known language is fontified by that language's mode."
  (md-render-test--with "```elisp\n(defun f () 1)\n```\n"
    (let ((faces (md-render-test--faces-at (md-render-test--search "defun"))))
      (should (memq 'font-lock-keyword-face faces))
      ;; The block background composes with the syntax faces, it does not
      ;; replace them.
      (should (memq 'md-code-block faces)))))

(ert-deftest md-render-unknown-language-is-not-interned ()
  "A fence label never selects a mode outside the allowlist."
  (should-not (assoc "definitely-not-a-language" md-render-language-modes))
  (md-render-test--with "```definitely-not-a-language\nbody text\n```\n"
    (should (md-render-test--search "body text"))))

(ert-deftest md-render-code-whitespace-is-preserved ()
  (md-render-test--with "```\n  indented\n\ttabbed\n```\n"
    (goto-char (point-min))
    (should (search-forward "  indented" nil t))))

;;; Images

(ert-deftest md-render-remote-images-are-not-fetched ()
  (let ((md-render-remote-images nil))
    (md-render-test--with "![alt](https://example.com/x.png)\n"
      (should (md-render-test--search "alt"))
      ;; The alt text, dimmed -- not an announcement that something failed.
      (goto-char (point-min))
      (should-not (search-forward "[image:" nil t)))))

(ert-deftest md-render-local-image-is-displayed ()
  (skip-unless (image-type-available-p 'png))
  (md-render-test--with "![pic](img/diagram.png)\n"
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) (point-max)))
        (let ((display (get-text-property (point) 'display)))
          (when (and (consp display) (eq (car display) 'image))
            (setq found t)))
        (forward-char 1))
      (should found))))

(ert-deftest md-render-missing-image-is-reported ()
  (md-render-test--with "![gone](img/nope.png)\n"
    (should (md-render-test--search "[missing image:"))))

;;; Tables

(ert-deftest md-render-table-has-box-junctions ()
  (md-render-test--with "| a | b |\n|---|---|\n| 1 | 2 |\n"
    (goto-char (point-min))
    ;; Corners, not a mesh of plus signs.
    (should (search-forward "┌" nil t))
    (goto-char (point-min))
    (should (search-forward "┘" nil t))
    (goto-char (point-min))
    (should (search-forward "┼" nil t))))

;;; Source line mapping

(ert-deftest md-render-maps-positions-to-source-lines ()
  (md-render-test--with "# One\n\nSecond para.\n\n## Third heading\n"
    (should (equal (md-render-source-line (md-render-test--search "One")) 1))
    (should (equal (md-render-source-line (md-render-test--search "Second")) 3))
    (should (equal (md-render-source-line (md-render-test--search "Third")) 5))))

(ert-deftest md-render-maps-source-lines-to-positions ()
  (md-render-test--with "# One\n\nSecond para.\n\n## Third heading\n"
    (goto-char (md-render-position-for-line 5))
    (should (looking-at "Third"))
    (goto-char (md-render-position-for-line 3))
    (should (looking-at "Second"))))

(ert-deftest md-render-unstamped-positions-fall-back ()
  "Blank lines and rules carry no stamp, so mapping uses the nearest block."
  (md-render-test--with "# One\n\nBody\n"
    ;; Every position resolves to some line, never nil.
    (let ((position (point-min)))
      (while (< position (point-max))
        (should (integerp (md-render-source-line position)))
        (setq position (1+ position))))))

;;; The toggle

(ert-deftest md-mode-round-trips-a-buffer ()
  (let ((source (generate-new-buffer "*md-test-source*")))
    (unwind-protect
        (with-current-buffer source
          (insert "# Title\n\nSome body text.\n")
          (goto-char (point-min))
          (let ((view (progn (md--show-render) (current-buffer))))
            (should (derived-mode-p 'md-view-mode))
            (should buffer-read-only)
            (should (eq (buffer-local-value 'md--source-buffer view) source))
            (goto-char (point-min))
            (should (search-forward "Title" nil t))
            ;; Back to the source, and the view is forgotten cleanly when the
            ;; source dies.
            (md--show-source)
            (should (eq (current-buffer) source))
            (kill-buffer source)
            (should-not (buffer-live-p view))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest md-mode-reuses-the-parsed-dom ()
  "An unedited buffer is not parsed twice."
  (with-temp-buffer
    (insert "# Title\n")
    (let ((first (md--dom (current-buffer))))
      (should (eq first (md--dom (current-buffer))))
      (insert "\nmore\n")
      (should-not (eq first (md--dom (current-buffer)))))))

(provide 'md-render-tests)
;;; md-render-tests.el ends here

;;; Regressions

(ert-deftest md-render-code-with-box-characters-is-not-rewritten ()
  "Table beautification must only touch text a table produced.
A code block drawing its own box used to have its corners rewritten."
  (md-render-test--with "```\n┼──┼\n│x│\n┼──┼\n```\n"
    (goto-char (point-min))
    (should (search-forward "┼──┼" nil t))
    (goto-char (point-min))
    (should-not (search-forward "┌" nil t))))

(ert-deftest md-render-table-still-gets-junctions ()
  (md-render-test--with "| a |\n|---|\n| 1 |\n"
    (goto-char (point-min))
    (should (search-forward "┌" nil t))))

(ert-deftest md-render-remote-image-path-is-not-opened ()
  "A TRAMP source must not reach a file primitive."
  (md-render-test--with "![x](/ssh:nonexistent.invalid:/x.png)"
    ;; The parser drops the unsafe src, so nothing is left to open.
    (should (or (save-excursion (goto-char (point-min))
                                (search-forward "[remote image:" nil t))
                (save-excursion (goto-char (point-min))
                                (search-forward "[image" nil t))
                (= (buffer-size) 0)))))

(ert-deftest md-render-maps-end-of-buffer ()
  "Point at the end of the document maps to the last block, not the first."
  (md-render-test--with "# One\n\nBody text\n"
    (should (equal (md-render-source-line (point-max)) 3))))

(ert-deftest md-render-maps-code-lines-individually ()
  (md-render-test--with "# H\n\n```\nalpha\nbeta\ngamma\n```\n"
    (should (equal (md-render-source-line (md-render-test--search "alpha")) 4))
    (should (equal (md-render-source-line (md-render-test--search "beta")) 5))
    (should (equal (md-render-source-line (md-render-test--search "gamma")) 6))))

(ert-deftest md-render-maps-table-rows-individually ()
  (md-render-test--with "| head |\n|------|\n| aaa |\n| bbb |\n"
    (should (equal (md-render-source-line (md-render-test--search "aaa")) 3))
    (should (equal (md-render-source-line (md-render-test--search "bbb")) 4))))

(ert-deftest md-mode-view-survives-a-revert ()
  "Reverting the source must not orphan its view and build a second one."
  (let* ((file (make-temp-file "md-mode-test" nil ".md" "# One\n\nBody\n"))
         (source (find-file-noselect file)))
    (unwind-protect
        (progn
          (with-current-buffer source (md--show-render))
          ;; `md--show-render' leaves the view current, so read the pointer
          ;; from the source buffer rather than from wherever we ended up.
          (let ((view (buffer-local-value 'md--view-buffer source)))
            (should (buffer-live-p view))
            (with-current-buffer source
              (revert-buffer t t t)
              (should (eq md--view-buffer view))
              (should (buffer-live-p view))
              (should (memq #'md--refresh after-save-hook)))
            (with-current-buffer source (md--show-render))
            (should (eq (buffer-local-value 'md--view-buffer source) view))))
      (when (buffer-live-p source) (kill-buffer source))
      (delete-file file))))

(ert-deftest md-view-suppresses-line-numbers ()
  "A global `display-line-numbers-mode' must not leak into the view.
It turns itself on after the major mode body, so the view has to switch
it off later than that."
  (let ((was global-display-line-numbers-mode))
    (unwind-protect
        (let ((source (generate-new-buffer "*md-line-number-test*")))
          (unwind-protect
              (progn
                (global-display-line-numbers-mode 1)
                (with-current-buffer source (insert "# Title\n\nBody\n") (md--show-render))
                (let ((view (buffer-local-value 'md--view-buffer source)))
                  (should (null (buffer-local-value 'display-line-numbers view)))
                  ;; The source keeps whatever the user asked for.
                  (should (buffer-local-value 'display-line-numbers source))))
            (when (buffer-live-p source) (kill-buffer source))))
      (global-display-line-numbers-mode (if was 1 -1)))))

(ert-deftest md-view-has-a-left-margin ()
  "The rendered document is not flush against the frame edge."
  (let ((source (generate-new-buffer "*md-margin-test*")))
    (unwind-protect
        (progn
          (with-current-buffer source (insert "# Title\n\nBody\n") (md--show-render))
          (let ((view (buffer-local-value 'md--view-buffer source)))
            (should (equal (buffer-local-value 'left-margin-width view)
                           md-view-margin))
            ;; The source is left alone.
            (should (equal (buffer-local-value 'left-margin-width source) 0))))
      (when (buffer-live-p source) (kill-buffer source)))))

(ert-deftest md-render-line-index-matches-a-linear-scan ()
  "The binary search must agree with the scan it replaced.
`md-sync-mode' looks up a position on every command, so the lookup has
to be indexed rather than a walk over the whole buffer -- but it must
land in exactly the same place."
  (cl-flet ((linear (line)
              (let ((pos (point-min)) (best (point-min)) (best-line 0))
                (while pos
                  (let ((here (get-text-property pos 'md-source-line)))
                    (when (and here (<= here line) (> here best-line))
                      (setq best pos best-line here)))
                  (setq pos (next-single-property-change pos 'md-source-line)))
                best)))
    (let ((markdown (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "corpus.md" md-render-test--directory))
                      (buffer-string))))
      (md-render-test--with markdown
        (dotimes (line 100)
          (should (= (md-render-position-for-line line) (linear line))))))))

(ert-deftest md-render-stamps-heading-level-itself ()
  "Emacs 29's shr records no outline level, so md-render must.
Locally shr gets there first, so the stamping is exercised directly."
  (with-temp-buffer
    (insert "\n\nHeading text\nbody line\n")
    (md--stamp-heading-level (point-min) (point-max) 3)
    (goto-char (point-min))
    (should (search-forward "Heading text" nil t))
    (should (equal (get-text-property (match-beginning 0) 'outline-level) 3))
    ;; Only the heading's own line is claimed, not what follows it.
    (goto-char (point-min))
    (should (search-forward "body line" nil t))
    (should-not (get-text-property (match-beginning 0) 'outline-level))))

(ert-deftest md-render-does-not-fight-shr-over-the-level ()
  "Where shr already recorded a level, leave it alone."
  (with-temp-buffer
    (insert "Heading\n")
    (put-text-property (point-min) (1+ (point-min)) 'outline-level 1)
    (md--stamp-heading-level (point-min) (point-max) 5)
    (should (equal (get-text-property (point-min) 'outline-level) 1))))

(ert-deftest md-render-measured-face-matches-what-shr-measured ()
  "A table must be drawn in the font its geometry was computed in.

shr sizes columns with `string-pixel-width', which measures in a work
buffer with no face remapping -- the frame's default face.  Not
`fixed-pitch', which leaves its height unspecified and so inherits the
height of whatever the default has been remapped to."
  (if (display-graphic-p)
      (let ((face (md--measured-face)))
        (should face)
        (should (equal (plist-get face :family)
                       (face-attribute 'default :family nil t)))
        (should (equal (plist-get face :height)
                       (face-attribute 'default :height nil t))))
    ;; Without a display there are no font metrics to disagree about.
    (should-not (md--measured-face))))

(ert-deftest md-render-plain-table-style-draws-no-rules ()
  "The escape hatch: no run of characters, so nothing can be mis-sized."
  (let ((md-render-table-style 'plain))
    (md-render-test--with "| a | b |\n|---|---|\n| 1 | 2 |\n"
      (goto-char (point-min))
      (dolist (glyph '("┌" "┐" "└" "┘" "├" "┤" "┬" "┴" "┼" "─" "│"))
        (goto-char (point-min))
        (should-not (search-forward glyph nil t)))
      ;; The content is still all there, in aligned columns.
      (goto-char (point-min))
      (should (search-forward "a" nil t))
      (goto-char (point-min))
      (should (search-forward "2" nil t)))))

(ert-deftest md-render-table-rule-gaps-are-closed ()
  "shr ends each run of a rule with a blank stretch to the column edge.
Left alone it is a visible gap in the rule, so it is struck through."
  (md-render-test--with "| a | b |\n|---|---|\n| 1 | 2 |\n"
    (goto-char (point-min))
    (let ((stretches 0)
          (struck 0)
          (end (line-end-position)))
      (dotimes (offset (- end (point-min)))
        (let ((pos (+ (point-min) offset)))
          (when (get-text-property pos 'shr-table-indent)
            (setq stretches (1+ stretches))
            (when (memq 'md-table-rule (md-render-test--faces-at pos))
              (setq struck (1+ struck))))))
      (should (> stretches 0))
      (should (= stretches struck)))))
