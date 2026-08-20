;;; md-render.el --- Render a Markdown DOM with shr  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea@byteset.com>
;; URL: https://github.com/hayate/md-mode
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, docs, markdown, hypermedia

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Turns the DOM produced by `md-parse' into a rendered buffer using `shr',
;; Emacs' own HTML renderer, so headings, filling, tables, links and images all
;; come from code that is already maintained.
;;
;; Three tags are rendered by hand because shr's defaults are not what a
;; Markdown reader wants: `pre' gains syntax highlighting, `blockquote' gains a
;; left bar, and `img' resolves relative paths and refuses to touch the network.
;; Every other block tag is wrapped only to record its source line, so point can
;; be carried between the source buffer and the rendered view.

;;; Code:

(require 'shr)
(require 'dom)
(require 'md-parse)

(defcustom md-render-max-width 100
  "Maximum line width, in characters, of a rendered document.
Long lines are hard to read, so the text is capped even in a wide window.
`shr-max-width' cannot be used for this: shr applies it only when
`shr-width' is nil, and we always set a width."
  :type 'integer
  :group 'md)

(defcustom md-render-remote-images nil
  "Whether to download and display images with remote URLs.
Nil, the default, renders them as a label.  Opening a document should
not make network requests to whoever wrote it."
  :type 'boolean
  :group 'md)

(defcustom md-render-highlight-code t
  "Whether to syntax-highlight fenced code blocks."
  :type 'boolean
  :group 'md)

(defcustom md-render-language-modes
  '(("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode)
    ("lisp" . lisp-mode) ("scheme" . scheme-mode)
    ("c" . c-mode) ("h" . c-mode) ("c++" . c++-mode) ("cpp" . c++-mode)
    ("java" . java-mode) ("python" . python-mode) ("py" . python-mode)
    ("sh" . sh-mode) ("bash" . sh-mode) ("shell" . sh-mode) ("zsh" . sh-mode)
    ("js" . js-mode) ("javascript" . js-mode) ("json" . js-mode)
    ("css" . css-mode) ("html" . mhtml-mode) ("xml" . nxml-mode)
    ("sql" . sql-mode) ("ruby" . ruby-mode) ("perl" . perl-mode)
    ("diff" . diff-mode) ("patch" . diff-mode) ("makefile" . makefile-mode)
    ("conf" . conf-mode) ("ini" . conf-mode) ("toml" . conf-toml-mode)
    ("yaml" . conf-mode) ("yml" . conf-mode))
  "Map from fenced-code language name to the major mode that highlights it.
This is deliberately an allowlist.  Deriving a mode name from the fence
label would let a downloaded document activate any installed major mode,
running its body and hooks."
  :type '(alist :key-type string :value-type symbol)
  :group 'md)

(defcustom md-render-max-highlight-size 20000
  "Fenced code blocks larger than this many characters are not highlighted."
  :type 'integer
  :group 'md)

(defcustom md-render-table-style 'box
  "How the borders of a table are drawn.

`box\' draws rules with box-drawing characters.  `plain\' draws no rules
at all and lets the aligned columns speak for themselves.

A rule is a run of characters whose length shr computes from font
metrics.  `plain\' has no such run, so it cannot come out the wrong
length whatever the fonts in play; it is the setting to reach for if a
table ever looks ragged."
  :type '(choice (const :tag "Box-drawing characters" box)
                 (const :tag "No rules, aligned columns only" plain))
  :group 'md)

(defcustom md-render-max-image-size (* 8 1024 1024)
  "Image files larger than this many bytes are not displayed."
  :type 'integer
  :group 'md)

(defface md-code-block '((t :inherit fixed-pitch :extend t))
  "Face for the body of a fenced code block."
  :group 'md)

(defface md-blockquote '((t :inherit shr-text :slant italic))
  "Face for quoted text."
  :group 'md)

(defface md-blockquote-bar '((t :inherit shadow))
  "Face for the bar drawn down the left of a blockquote."
  :group 'md)

(defface md-blocked-image '((t :inherit shadow))
  "Face for an image that was not fetched.
See `md-render-remote-images'."
  :group 'md)

(defface md-table-rule '((t :strike-through t))
  "Face for the stretch that closes a gap in a table rule.
`shr' ends each run of a rule with a space stretched to the exact
column boundary, which absorbs the rounding between whole characters
and the true width.  A blank stretch leaves a visible gap in the rule,
so it is struck through instead, which draws a line across it.")

(defvar-local md--base-directory nil
  "Directory that relative image paths are resolved against.")

(defvar-local md--line-index nil
  "Vector of (SOURCE-LINE . POSITION), ordered by line.
Built once per render.  Walking the whole buffer per lookup is fine for
a toggle but far too slow for `md-sync-mode\', which looks up on every
command.")

;;; Syntax highlighting of code blocks

(defvar md--highlight-cache (make-hash-table :test #'equal)
  "Cache of highlighted code, keyed by (MODE . CODE).")

(defun md--highlight (code language)
  "Return CODE fontified as LANGUAGE, or unchanged if that is not possible."
  (let ((mode (cdr (assoc (downcase (or language "")) md-render-language-modes))))
    (if (or (not md-render-highlight-code)
            (null mode)
            (not (fboundp mode))
            (> (length code) md-render-max-highlight-size))
        code
      (let ((key (cons mode code)))
        (or (gethash key md--highlight-cache)
            (progn
              (when (> (hash-table-count md--highlight-cache) 256)
                (clrhash md--highlight-cache))
              (puthash key
                       (condition-case nil
                           (with-temp-buffer
                             (insert code)
                             ;; `delay-mode-hooks' keeps a document from
                             ;; triggering whatever the user has hung off a
                             ;; major mode.
                             (delay-mode-hooks (funcall mode))
                             (font-lock-ensure)
                             (buffer-string))
                         (error code))
                       md--highlight-cache)))))))

(defun md--code-of (dom)
  "Concatenate the text of DOM exactly, without separators."
  (mapconcat (lambda (node) (if (stringp node) node (md--code-of node)))
             (dom-children dom) ""))

;;; Custom renderers

(defun md--render-pre (dom)
  "Render a code block DOM."
  (let* ((class (or (dom-attr dom 'class) ""))
         (language (and (string-match "language-\\(.*\\)" class)
                        (match-string 1 class)))
         (code (md--code-of dom)))
    (shr-ensure-paragraph)
    (let ((start (point))
          ;; As in `shr-tag-pre': nothing in a code block may be folded.
          (shr-folding-mode 'none)
          (shr-current-font 'default))
      (insert (md--highlight code language))
      (unless (bolp) (insert "\n"))
      (shr-ensure-paragraph)
      ;; `add-face-text-property' composes, so syntax highlighting survives.
      (add-face-text-property start (point) 'md-code-block t)
      (md--stamp-code-lines start (point) (dom-attr dom 'data-code-line))
      (when (> shr-indentation 0)
        (let ((pad (propertize " " 'display `(space :width (,shr-indentation)))))
          (add-text-properties start (point)
                               (list 'line-prefix pad 'wrap-prefix pad)))))))

(defun md--prepend-prefix (start end prefix)
  "Prepend PREFIX to the line and wrap prefixes between START and END.
Existing prefixes are kept, so nested blockquotes stack their bars."
  (let ((pos start))
    (while (< pos end)
      (let* ((next (next-single-property-change pos 'line-prefix nil end))
             (existing (get-text-property pos 'line-prefix))
             (combined (if existing (concat prefix existing) prefix)))
        (add-text-properties pos next
                             (list 'line-prefix combined 'wrap-prefix combined))
        (setq pos next)))))

(defun md--render-blockquote (dom)
  "Render a blockquote DOM with a bar down its left edge."
  (shr-ensure-paragraph)
  (let* ((start (point))
         (prefix (propertize "\u2502 " 'face 'md-blockquote-bar))
         (width (shr-string-pixel-width "  ")))
    ;; The prefix is drawn outside the filled text, so take its width off the
    ;; fill width rather than letting the quote overhang the window.
    (let ((shr-internal-width (max (* 20 shr-table-separator-pixel-width)
                                   (- shr-internal-width width))))
      (shr-generic dom))
    (shr-ensure-paragraph)
    (md--prepend-prefix start (point) prefix)
    (add-face-text-property start (point) 'md-blockquote t)))

(defun md--remote-p (url)
  "Non-nil if URL points somewhere on the network."
  (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" url))

(defun md--render-img (dom &optional url)
  "Render an image.
DOM is the image node and may be nil; URL overrides its source.  shr
calls this from `<object>' and from media posters, hence both arguments."
  (let* ((src (or url (and dom (dom-attr dom 'src))))
         (alt (or (and dom (dom-attr dom 'alt)) "")))
    (cond
     ((or (null src) (string-empty-p src)) nil)
     ;; shr blanks images while it measures table cells and renders them on a
     ;; later pass; decoding them now would be wasted work and break widths.
     ((or shr-inhibit-images (> shr-table-depth 0))
      (shr-insert (format "[%s]" (if (string-empty-p alt) "image" alt))))
     ((md--remote-p src)
      (if md-render-remote-images
          (shr-tag-img dom url)
        ;; Not fetched, by design.  Show the alt text rather than announcing
        ;; a failure: a README's badges are images inside links, and
        ;; "[image: tests]" reads as something broken when nothing is.
        (let ((start (point)))
          (shr-insert (if (string-empty-p alt)
                          (format "\u2b1a %s" src)
                        (format "\u2b1a %s" alt)))
          (add-face-text-property start (point) 'md-blocked-image t))))
     (t (md--insert-local-image src alt)))))

(defun md--insert-local-image (src alt)
  "Insert the image file at SRC, described by ALT."
  (let* ((name (ignore-errors (url-unhex-string src)))
         ;; `file-readable-p\' on a name like /ssh:host:/x.png makes Emacs open
         ;; a network connection.  A document does not get to do that.
         (remote (or (null name) (file-remote-p name)
                     (file-remote-p (or md--base-directory default-directory))))
         (file (and (not remote)
                    (ignore-errors
                      (expand-file-name name (or md--base-directory
                                                 default-directory)))))
         (size (and file (file-readable-p file) (file-regular-p file)
                    (file-attribute-size (file-attributes file)))))
    (cond
     (remote (shr-insert (format "[remote image: %s]" src)))
     ((null size) (shr-insert (format "[missing image: %s]" src)))
     ((> size md-render-max-image-size)
      (shr-insert (format "[image too large: %s]" src)))
     (t
      (condition-case nil
          (let ((image (create-image
                        file nil nil
                        :max-width (truncate (* 0.95 (shr--window-width)))
                        :max-height (truncate (* 0.8 (frame-pixel-height)))
                        :ascent 90)))
            (shr-ensure-newline)
            (insert-image image (if (string-empty-p alt) "[image]" alt))
            (insert "\n"))
        (error (shr-insert (format "[unreadable image: %s]" src))))))))

;;; Source line stamping
;;
;; shr knows nothing about `data-line', so each block tag is wrapped in a
;; renderer that records where its output landed.  Children render before their
;; parent stamps, so the parent must only fill in the gaps its children left --
;; otherwise an outer div would claim every line of the document.

(defun md--stamp-code-lines (start end first-line)
  "Map each rendered code line between START and END to its source line.
FIRST-LINE is the source line of the first line of the block."
  (when first-line
    (save-excursion
      (goto-char start)
      (let ((line (string-to-number first-line)))
        (while (< (point) end)
          (put-text-property (point) (min end (line-end-position))
                             'md-source-line line)
          (setq line (1+ line))
          (forward-line 1))))))

(defvar md-render-before-hook nil
  "Functions run in the buffer at the start of each render.")

(defvar md-render-code-span-functions nil
  "Functions called for each rendered inline code span, with START and END.
This is how `md-link\' turns a span such as `md-parse.el:1\' into a
link without `md-render\' having to know anything about references.")

(defvar md-render-link-map nil
  "Keymap installed on links, or nil for shr's own.
Bound during rendering so that `shr-urlify\' installs it itself, which
leaves the keymaps shr puts on linked images alone.")

(defun md--render-code (dom)
  "Render an inline code span DOM, offering it to decorators."
  (let ((start (point)))
    (shr-tag-code dom)
    (run-hook-with-args 'md-render-code-span-functions start (point))))

(defconst md--stamped-tags
  '(p h1 h2 h3 h4 h5 h6 blockquote pre ul ol li table td th hr div)
  "Block tags whose rendered extent is mapped back to a source line.")

(defun md--stamp-region (start end line)
  "Record LINE as the source line for unclaimed text between START and END."
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'md-source-line nil end)))
        (unless (get-text-property pos 'md-source-line)
          (put-text-property pos next 'md-source-line line))
        (setq pos next)))))

(defvar md--list-depth 0
  "Nesting depth of the list being rendered.")

(defun md--render-list (dom ordered)
  "Render a list DOM, ORDERED for an ordered one.
Unlike `shr-tag-ul', a nested list does not get a blank line around
it: in Markdown a sublist belongs to its item, not to the page."
  (if (> md--list-depth 0) (shr-ensure-newline) (shr-ensure-paragraph))
  (let* ((start (and ordered (dom-attr dom 'start)))
         (md--list-depth (1+ md--list-depth))
         (shr-list-mode (if ordered
                            (or (and start (ignore-errors (string-to-number start)))
                                1)
                          'ul)))
    (shr-generic dom))
  (unless (bolp) (insert "\n"))
  (when (zerop md--list-depth) (shr-ensure-paragraph)))

(defun md--render-ul (dom) "Render an unordered list DOM." (md--render-list dom nil))
(defun md--render-ol (dom) "Render an ordered list DOM." (md--render-list dom t))

(defun md--render-li (dom)
  "Render a list item DOM.
This is `shr-tag-li\' with a period after the ordinal, which is what a
Markdown reader expects to see.

Derived from `shr-tag-li\' in shr.el, which is part of GNU Emacs and is
copyright (C) 2010-2025 Free Software Foundation, Inc., released under
the GNU General Public License version 3 or later.  That derivation is
why this package carries the same licence."
  (shr-ensure-newline)
  (let ((start (point)))
    (let* ((bullet (if (numberp shr-list-mode)
                       (prog1 (format "%d. " shr-list-mode)
                         (setq shr-list-mode (1+ shr-list-mode)))
                     (car shr-internal-bullet)))
           (width (if (numberp shr-list-mode)
                      (shr-string-pixel-width bullet)
                    (cdr shr-internal-bullet))))
      (insert bullet)
      (shr-mark-fill start)
      (let ((shr-indentation (+ shr-indentation width)))
        (put-text-property start (1+ start) 'shr-continuation-indentation shr-indentation)
        (put-text-property start (1+ start) 'shr-prefix-length (length bullet))
        (shr-generic dom))))
  (unless (bolp) (insert "\n")))

(defun md--base-renderer (tag)
  "The renderer TAG would have had without our wrapper."
  (or (cdr (assq tag '((pre . md--render-pre)
                       (blockquote . md--render-blockquote)
                       (ul . md--render-ul)
                       (ol . md--render-ol)
                       (li . md--render-li)
                       (table . md--render-table)
                       (hr . md--render-hr))))
      (let ((fn (intern-soft (format "shr-tag-%s" tag))))
        (and (fboundp fn) fn))
      #'shr-generic))

(defun md--stamp-heading-level (start end level)
  "Record LEVEL on the heading rendered between START and END.
Emacs 30's `shr\' does this itself, but earlier ones do not, and the
outline machinery has nothing else to find headings by.  Stamping it
here rather than relying on shr also keeps the package off an
implementation detail of a library it does not own."
  (unless (get-text-property start 'outline-level)
    (save-excursion
      (goto-char start)
      (skip-chars-forward " \t\n" end)
      (when (< (point) end)
        (put-text-property (point) (min end (line-end-position))
                           'outline-level level)))))

(defun md--stamping-renderer (tag)
  "Return a renderer for TAG that records its source line."
  (let ((base (md--base-renderer tag))
        (level (and (string-match "\\`h\\([1-6]\\)\\'" (symbol-name tag))
                    (string-to-number (match-string 1 (symbol-name tag))))))
    (lambda (dom &rest args)
      (let ((start (point))
            (line (and (consp dom) (dom-attr dom 'data-line))))
        (apply base dom args)
        (when (and level (> (point) start))
          (md--stamp-heading-level start (point) level))
        (when (and line (> (point) start))
          (md--stamp-region start (point) (string-to-number line)))))))

(defun md--rendering-functions ()
  "Build the value for `shr-external-rendering-functions'."
  (append (list (cons 'img #'md--render-img)
                (cons 'code #'md--render-code))
          (mapcar (lambda (tag) (cons tag (md--stamping-renderer tag)))
                  md--stamped-tags)))


;;; Table junctions
;;
;; shr draws every join in a table with the single character
;; `shr-table-corner'.  The grid is correct but reads as a mesh of plus
;; signs, so each ruler line is rewritten with the box-drawing junction that
;; belongs at that position.

(defconst md--table-junctions
  '((left  ?\u250c ?\u251c ?\u2514)
    (middle ?\u252c ?\u253c ?\u2534)
    (right ?\u2510 ?\u2524 ?\u2518))
  "Junction characters by horizontal position and top/middle/bottom row.")

(defun md--ruler-line-p ()
  "Non-nil if the current line is a table ruler."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*\u253c[\u253c\u2500 \t]*$")))

(defun md--table-row-p ()
  "Non-nil if the current line is a table content row."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*\u2502")))

(defun md--close-rule-gaps (start end)
  "Draw a line through the stretch spaces of the ruler between START and END."
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'shr-table-indent nil end)))
        (when (get-text-property pos 'shr-table-indent)
          (add-face-text-property pos next 'md-table-rule))
        (setq pos next)))))

(defun md--fix-ruler (position row)
  "Rewrite the junctions of the ruler line at POSITION.
ROW is 0 for the top ruler, 1 for a middle one and 2 for the bottom."
  (save-excursion
    (goto-char position)
    (md--close-rule-gaps position (line-end-position))
    (let ((end (line-end-position))
          (columns '()))
      (while (search-forward "\u253c" end t)
        (push (1- (point)) columns))
      (setq columns (nreverse columns))
      (let ((last (1- (length columns)))
            (index 0))
        (dolist (column columns)
          (let* ((where (cond ((= index 0) 'left)
                              ((= index last) 'right)
                              (t 'middle)))
                 (glyph (nth row (cdr (assq where md--table-junctions))))
                 (props (text-properties-at column)))
            (goto-char column)
            (delete-char 1)
            (insert (apply #'propertize (char-to-string glyph) props)))
          (setq index (1+ index)))))))

(defun md--measured-face ()
  "The face `string-pixel-width\' measures in.

shr sizes a table from `string-pixel-width\', which measures in a work
buffer carrying no face remapping -- that is, in the frame\'s own
default face.  Drawing the table in any other font makes its rules the
wrong length.

`fixed-pitch\' is not that font.  It names a family and leaves the
height unspecified, so the height comes from the default face, which
`buffer-face-set\' has remapped to `variable-pitch\' throughout the view.
The result is a monospaced family at the body text\'s size: still not
the size the geometry was computed at."
  (when (display-graphic-p)
    (let ((family (face-attribute 'default :family nil t))
          (height (face-attribute 'default :height nil t)))
      (append (and (stringp family) (list :family family))
              ;; A real height is in tenths of a point; batch reports 1.
              (and (integerp height) (> height 10) (list :height height))))))

(defun md--render-table (dom)
  "Render a table DOM and mark the text it produced.
The mark is what `md--beautify-tables\' works from.  Recognising tables
by their glyphs instead would rewrite any code block or paragraph that
happened to contain box-drawing characters."
  (let ((start (point)))
    (shr-tag-table dom)
    (put-text-property start (point) 'md-table t)
    ;; shr sizes the columns with `string-pixel-width', which measures in the
    ;; default face, while the view shows body text in variable-pitch.  The
    ;; grid is therefore laid out in one font and drawn in another, so the
    ;; rules come up short of the verticals they should meet.  Showing the
    ;; Only the borders.  shr sizes a column by measuring the cell's own
    ;; rendered text, faces and all, so cell content must stay in the face it
    ;; was measured in or it overflows the column sized for it.  The rules are
    ;; different: shr counts those in units of `-' in the default face, so
    ;; they have to be drawn in that face to come out the right length.
    (md--face-borders start (point))))

(defconst md--box-drawing-chars
  (append "\u2500\u2502\u250c\u2510\u2514\u2518\u251c\u2524\u252c\u2534\u253c" nil)
  "The characters a table's borders are drawn from.")

(defun md--face-borders (start end)
  "Draw the border characters between START and END in the measured face.

shr sizes a column by measuring the cell's own rendered text, faces and
all, so cell content has to stay in the face it was measured in or it
overflows the column sized for it.  A rule is different: shr counts it
in units of `-\' in the default face, so it has to be drawn in that face
to come out the right length.

Deliberately a plain walk over the region rather than `skip-chars-forward\':
this runs inside rendering, and a scan that can fail to advance hangs
Emacs with no way out."
  (when-let ((face (md--measured-face)))
    (let ((pos start))
      (while (< pos end)
        (when (memq (char-after pos) md--box-drawing-chars)
          (add-face-text-property pos (1+ pos) face))
        (setq pos (1+ pos))))))

(defun md--render-hr (dom)
  "Render a thematic break DOM.
Its width is a character count derived from the same measurement the
tables use, so it needs the same face to come out the right length."
  (let ((start (point)))
    (shr-tag-hr dom)
    (when-let ((face (md--measured-face)))
      (add-face-text-property start (point) face))))

(defun md--beautify-table-region (start end)
  "Give the table between START and END proper box-drawing junctions."
  (save-excursion
    (goto-char start)
    (let ((rulers '()))
      (while (< (point) end)
        (when (md--ruler-line-p)
          (push (line-beginning-position) rulers))
        (forward-line 1))
      (setq rulers (nreverse rulers))
      (let ((last (1- (length rulers)))
            (index 0))
        (dolist (ruler rulers)
          (md--fix-ruler ruler (cond ((= index 0) 0) ((= index last) 2) (t 1)))
          (setq index (1+ index)))))))

(defun md--beautify-tables ()
  "Give every rendered table proper box-drawing junctions."
  (let ((pos (point-min)))
    (while (setq pos (text-property-any pos (point-max) 'md-table t))
      (let ((end (or (next-single-property-change pos 'md-table) (point-max))))
        (md--beautify-table-region pos end)
        (setq pos end)))))

;;; Entry point

(defun md-render-width ()
  "Width, in characters, to render at in the selected window."
  (max 20 (min md-render-max-width (- (window-body-width) 2))))

(defun md-render-dom (dom &optional base-directory width)
  "Render DOM into the current buffer.
BASE-DIRECTORY resolves relative image paths.  WIDTH defaults to the
width of the selected window."
  (let ((inhibit-read-only t)
        (shr-width (or width (md-render-width)))
        ;; Proportional fonts need a real display; on a terminal shr must
        ;; fall back to character metrics or filling collapses.
        (shr-use-fonts (display-graphic-p))
        (shr-bullet "\u2022 ")
        (shr-hr-line ?\u2500)
        (shr-table-horizontal-line (and (eq md-render-table-style 'box) ?\u2500))
        (shr-table-vertical-line (if (eq md-render-table-style 'box) ?\u2502 ?\s))
        (shr-table-corner (if (eq md-render-table-style 'box) ?\u253c ?\s))
        ;; Never hand a document's media to an embedded browser, whatever the
        ;; user has configured globally.
        (shr-use-xwidgets-for-media nil)
        ;; shr-urlify installs `shr-map' itself, and deliberately leaves any
        ;; keymap already on the text alone -- an image's, say.  Binding the
        ;; variable therefore gets our bindings onto links without
        ;; overwriting anything shr was protecting.
        (shr-map (or md-render-link-map shr-map))
        (shr-external-rendering-functions (md--rendering-functions)))
    (setq md--base-directory (or base-directory default-directory))
    (run-hooks 'md-render-before-hook)
    (erase-buffer)
    (shr-insert-document dom)
    (when (eq md-render-table-style 'box) (md--beautify-tables))
    (setq md--line-index nil)
    (goto-char (point-min))))

;;; Mapping between source and rendered positions

(defun md-render-source-line (&optional position)
  "Source line corresponding to POSITION in a rendered buffer."
  (let ((pos (or position (point))))
    (or (get-text-property pos 'md-source-line)
        ;; Blank lines, table rules and image padding carry no stamp, so fall
        ;; back to the nearest block that starts at or before point.  At
        ;; `point-max\' there is no character to read, so start one back.
        (and (> pos (point-min)) (get-text-property (1- pos) 'md-source-line))
        (let ((previous (previous-single-property-change
                         (min pos (max (point-min) (1- (point-max))))
                         'md-source-line)))
          (and previous (get-text-property (1- previous) 'md-source-line)))
        (let ((next (next-single-property-change pos 'md-source-line)))
          (and next (get-text-property next 'md-source-line)))
        1)))

(defun md--build-line-index ()
  "Collect the source line stamps in this buffer into a sorted vector."
  (let ((entries '())
        (pos (point-min))
        (seen (make-hash-table :test #'eql)))
    (while pos
      (let ((line (get-text-property pos 'md-source-line)))
        (when (and line (not (gethash line seen)))
          (puthash line t seen)
          (push (cons line pos) entries)))
      (setq pos (next-single-property-change pos 'md-source-line)))
    (vconcat (sort (nreverse entries)
                   (lambda (a b) (< (car a) (car b)))))))

(defun md-render-position-for-line (line)
  "Position in the rendered buffer that best corresponds to source LINE.
The nearest block starting at or before LINE, found by binary search."
  (let* ((index (or md--line-index
                    (setq md--line-index (md--build-line-index))))
         (count (length index)))
    (if (zerop count)
        (point-min)
      (let ((low 0) (high (1- count)) (best (point-min)))
        (while (<= low high)
          (let* ((mid (/ (+ low high) 2))
                 (entry (aref index mid)))
            (if (<= (car entry) line)
                (progn (setq best (cdr entry)) (setq low (1+ mid)))
              (setq high (1- mid)))))
        best))))

(provide 'md-render)
;;; md-render.el ends here
