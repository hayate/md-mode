;;; md-parse.el --- Parse Markdown into an shr-renderable DOM  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea@byteset.com>
;; URL: https://github.com/hayate/md-mode
;; Version: 0.1.0
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

;; Converts Markdown text into the DOM representation used by `dom.el' and
;; consumed by `shr' -- (TAG ATTRIBUTE-ALIST CHILD...) with strings as text
;; nodes.  No HTML text is ever generated, so there is nothing to escape and
;; nothing to reparse.
;;
;; The parser runs in two passes.  `md--parse-blocks' walks a list of lines and
;; produces block-level nodes; the text of each block is then handed to
;; `md--inline' which scans for spans.  Blockquotes and list items recurse
;; through `md--parse-blocks' on their dedented contents.
;;
;; Every block node carries a `data-line' attribute holding its 1-based source
;; line, which `md-render' turns into a text property so point can be mapped
;; between the source buffer and the rendered view.
;;
;; The goal is the Markdown people actually write, not CommonMark conformance.
;; Footnotes, definition lists and math are out of scope.

;;; Code:

(require 'dom)
(require 'subr-x)
(require 'seq)
(require 'cl-lib)

(defgroup md nil
  "Render Markdown documents inside Emacs."
  :group 'text
  :prefix "md-")

(defcustom md-parse-front-matter nil
  "Whether to render YAML front matter instead of hiding it.
Non-nil renders the block as code.  Nil, the default, drops it, which
is what GitHub and most other Markdown renderers do."
  :type 'boolean
  :group 'md)

(defcustom md-parse-html-blocks t
  "Whether to parse raw HTML blocks and splice them into the document.
Nil renders them as literal text.  Images inside spliced HTML go
through the same renderer as Markdown images, so remote fetching stays
governed by `md-render-remote-images'."
  :type 'boolean
  :group 'md)

;; Link reference definitions, bound for the duration of one parse.
(defvar md--refs nil)

(defvar md--depth 0
  "Current container nesting depth, to bound recursion.")

(defconst md--max-depth 24
  "Deepest nesting of blockquotes and lists that is parsed as structure.
Emacs signals `excessive-lisp-nesting\' long before a document like
that means anything, so deeper containers are rendered as plain text.")

;;; Lines
;;
;; A "line" is (NUMBER . TEXT) so that block nodes can record where they came
;; from.  Blockquote and list recursion carries the numbers through unchanged.

(defsubst md--ln (line) "Source line number of LINE." (car line))
(defsubst md--txt (line) "Text of LINE." (cdr line))

(defun md--blank-p (text)
  "Non-nil if TEXT is empty or only whitespace."
  (string-match-p "\\`[ \t]*\\'" text))

(defun md--indent (text)
  "Number of leading spaces in TEXT."
  (if (string-match "\\`\\( *\\)" text) (length (match-string 1 text)) 0))

(defun md--expand-leading-tabs (text)
  "Replace leading tabs in TEXT with spaces to the next multiple of four."
  (if (not (string-match "\\`[ \t]*\t" text))
      text
    (let ((col 0) (i 0) (len (length text)))
      (while (and (< i len) (memq (aref text i) '(?\s ?\t)))
        (setq col (if (eq (aref text i) ?\t) (* 4 (1+ (/ col 4))) (1+ col)))
        (setq i (1+ i)))
      (concat (make-string col ?\s) (substring text i)))))

(defun md--number-lines (strings)
  "Pair each of STRINGS with its 1-based line number."
  (let ((n 0))
    (mapcar (lambda (s) (setq n (1+ n)) (cons n (md--expand-leading-tabs s)))
            strings)))

(defun md--dedent (lines n)
  "Remove up to N leading spaces from each of LINES."
  (mapcar (lambda (l)
            (let* ((s (md--txt l))
                   (strip (min n (md--indent s))))
              (cons (md--ln l) (substring s strip))))
          lines))

(defun md--drop-trailing-blanks (lines)
  "Return LINES without trailing blank lines."
  (let ((r (reverse lines)))
    (while (and r (md--blank-p (md--txt (car r))))
      (setq r (cdr r)))
    (nreverse r)))

;;; Entities and text nodes

(defconst md--entities
  '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"")
    ("&apos;" . "'") ("&nbsp;" . " ") ("&hellip;" . "...")
    ("&mdash;" . "-") ("&ndash;" . "-") ("&copy;" . "(C)") ("&reg;" . "(R)")))

(defun md--decode-entities (text)
  "Decode the HTML entities in TEXT that libxml would have decoded for us."
  (if (not (string-match-p "&" text))
      text
    (let ((s text))
      (pcase-dolist (`(,entity . ,replacement) md--entities)
        (setq s (string-replace entity replacement s)))
      (replace-regexp-in-string
       "&#\\([0-9]+\\);\\|&#[xX]\\([0-9a-fA-F]+\\);"
       (lambda (m)
         (let ((decimal (match-string 1 m)) (hex (match-string 2 m)))
           (ignore-errors
             (char-to-string (if decimal (string-to-number decimal)
                               (string-to-number hex 16))))))
       s t t))))

;; Marks a hard line break while the paragraph is a single string.
(defconst md--hard-break "\0")

;;; Inline parsing
;;
;; The scanner walks the string once, trying constructs at each position.
;; Two things keep it linear on hostile input.  Bracket and parenthesis
;; matches are precomputed in a single stack pass, so `[[[[[[...' cannot make
;; every opener rescan the tail.  And any search that runs off the end of the
;; string records that fact: a later search for the same thing starts further
;; right, so it cannot succeed either, and returns immediately.

(defconst md--punctuation "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")

(defconst md--max-inline-depth 32
  "Deepest nesting of inline constructs that is parsed as structure.
Beyond this the remaining text is emitted literally, so that a document
of nested links cannot exhaust the Lisp stack.")

(defvar md--inline-depth 0)
(defvar md--brackets nil "Map from the index of a `[' to its matching `]'.")
(defvar md--parens nil "Map from the index of a `(' to its matching `)'.")
(defvar md--exhausted nil "Searches already known to run off the end.")

(defconst md--allowed-schemes
  '("http" "https" "mailto" "ftp" "ftps" "news" "irc" "ircs" "gopher")
  "URL schemes a document may link to.
An allowlist: anything else, notably `javascript:', `data:' and
`file:', is dropped rather than handed to the renderer.")

(defun md--safe-url-p (url)
  "Non-nil if URL is safe to put in the document.
Control characters are stripped before the scheme is examined, so that
an entity-encoded newline cannot smuggle one past.  A path with no
scheme is fine unless it is a remote name, which would make Emacs open
a network connection just to display it."
  (and (stringp url)
       (let ((clean (string-trim (replace-regexp-in-string "[\0-\37\177]" "" url))))
         (if (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" clean)
             (and (member (downcase (match-string 1 clean)) md--allowed-schemes) t)
           (not (file-remote-p clean))))))

(defun md--clean-url (url)
  "Return URL without control characters, or nil if it is not safe."
  (and (md--safe-url-p url)
       (string-trim (replace-regexp-in-string "[\0-\37\177]" "" url))))

(defun md--delimiter-map (string open close)
  "Map each OPEN index in STRING to the index of its matching CLOSE."
  (let ((map (make-hash-table :test #'eql))
        (stack '())
        (index 0)
        (length (length string)))
    (while (< index length)
      (pcase (aref string index)
        (?\\ (setq index (1+ index)))
        ((pred (eq open)) (push index stack))
        ((pred (eq close)) (when stack (puthash (pop stack) index map))))
      (setq index (1+ index)))
    map))

(defun md--matched (position map)
  "Index matching the delimiter at POSITION according to MAP."
  (and map (gethash position map)))

(defun md--exhausted-p (key)
  "Non-nil if the search named KEY has already failed."
  (and md--exhausted (gethash key md--exhausted)))

(defun md--exhaust (key)
  "Record that the search named KEY ran off the end.  Always return nil."
  (when md--exhausted (puthash key t md--exhausted))
  nil)

(defun md--looking-at (string position prefix)
  "Non-nil if STRING has PREFIX at POSITION."
  (let ((end (+ position (length prefix))))
    (and (<= end (length string))
         (eq t (compare-strings prefix nil nil string position end)))))

(defun md--text (string)
  "Return STRING as a text node, with entities decoded."
  (md--decode-entities string))

(defun md--inline (string)
  "Parse STRING into a list of inline DOM nodes."
  (if (> md--inline-depth md--max-inline-depth)
      (list (md--text string))
    (let ((md--inline-depth (1+ md--inline-depth))
          (md--brackets (and (string-search "[" string)
                             (md--delimiter-map string ?\[ ?\])))
          (md--parens (and (string-search "(" string)
                           (md--delimiter-map string ?\( ?\))))
          (md--exhausted (make-hash-table :test #'equal))
          (nodes '())
          (pos 0)
          (plain 0)
          (len (length string)))
      (while (< pos len)
        (let ((hit (md--inline-at string pos)))
          (cond
           (hit
            (when (> pos plain)
              (push (md--text (substring string plain pos)) nodes))
            (when (car hit) (push (car hit) nodes))
            (setq pos (cdr hit) plain pos))
           (t (setq pos (1+ pos))))))
      (when (> len plain)
        (push (md--text (substring string plain)) nodes))
      (nreverse nodes))))

(defun md--inline-at (s pos)
  "Try to parse an inline construct in S at POS.
Return (NODE . END) with END greater than POS, or nil."
  (pcase (aref s pos)
    (?\0 (cons '(br nil) (1+ pos)))
    (?\\ (md--inline-escape s pos))
    (?`  (md--inline-code s pos))
    (?<  (md--inline-autolink s pos))
    (?!  (md--inline-image s pos))
    (?\[ (md--inline-link s pos))
    ((or ?* ?_) (md--inline-emphasis s pos))
    (?~  (md--inline-strike s pos))
    (?h  (md--inline-bare-url s pos))
    (_ nil)))

(defun md--inline-escape (s pos)
  "Parse a backslash escape in S at POS."
  (when (and (< (1+ pos) (length s))
             (string-search (char-to-string (aref s (1+ pos))) md--punctuation))
    (cons (char-to-string (aref s (1+ pos))) (+ pos 2))))

(defun md--inline-code (s pos)
  "Parse a code span in S at POS."
  (let* ((ticks (md--run-length s pos ?`))
         (key (cons 'code ticks)))
    (unless (md--exhausted-p key)
      (let* ((start (+ pos ticks))
             (close (string-search (make-string ticks ?`) s start)))
        (if (not close)
            (md--exhaust key)
          (let ((body (substring s start close)))
            ;; One leading and trailing space is dropped, so that `` ` ``
            ;; can hold a literal backtick.
            (when (and (> (length body) 1)
                       (eq (aref body 0) ?\s)
                       (eq (aref body (1- (length body))) ?\s))
              (setq body (substring body 1 -1)))
            (cons `(code nil ,(string-replace md--hard-break " " body))
                  (+ close ticks))))))))

(defun md--inline-autolink (s pos)
  "Parse an autolink, or a line break tag, in S at POS."
  (let ((close (and (not (md--exhausted-p 'angle)) (string-search ">" s pos))))
    (if (not close)
        (md--exhaust 'angle)
      (let ((candidate (substring s pos (1+ close))))
        (cond
         ((string-match-p "\\`<br ?/?>\\'" candidate)
          (cons '(br nil) (1+ close)))
         ((string-match
           (concat "\\`<\\(\\(?:https?\\|mailto\\|ftp\\)://[^>[:space:]]+"
                   "\\|[^>[:space:]@]+@[^>[:space:]]+\\)>\\'")
           candidate)
          (let* ((url (match-string 1 candidate))
                 (href (if (string-match-p "\\`[^:]+@" url)
                           (concat "mailto:" url)
                         url)))
            (cons `(a ,(and (md--clean-url href) `((href . ,(md--clean-url href))))
                      ,(md--text url))
                  (1+ close)))))))))

(defun md--inline-bare-url (s pos)
  "Parse a bare http or https URL in S at POS."
  (when (and (or (md--looking-at s pos "http://") (md--looking-at s pos "https://"))
             (or (= pos 0)
                 (not (memq (aref s (1- pos)) '(?\( ?< ?\" ?/ ?= ?:)))))
    ;; The prefix is already known to match here, so the search cannot run on.
    (when (eq pos (string-match "https?://[^[:space:]<>\"']+[^[:space:]<>\"'.,;:!?)]"
                                s pos))
      (let ((url (match-string 0 s)))
        (cons `(a ((href . ,url)) ,url) (+ pos (length url)))))))

(defun md--parse-destination (s start)
  "Parse a link destination in S beginning at the open paren at START.
Return (URL TITLE END) or nil."
  (let ((close (md--matched start md--parens)))
    (when close
      (let* ((body (string-trim (substring s (1+ start) close)))
             (url body)
             (title nil))
        (when (string-match
               "\\`\\(<[^>]*>\\|[^[:space:]]*\\)\\(?:[ \t]+[\"'(]\\(.*\\)[\"')]\\)?\\'"
               body)
          (setq url (match-string 1 body)
                title (match-string 2 body))
          (when (string-prefix-p "<" url)
            (setq url (string-trim url "<" ">"))))
        (list url title (1+ close))))))

(defun md--link-node (tag text url title)
  "Build a link or image node.
TAG is `a' or `img', TEXT the label, URL the destination and TITLE the
optional title.  An unsafe URL is dropped rather than passed on, which
leaves the label visible but inert."
  (let* ((safe (md--clean-url (md--decode-entities url)))
         (attrs (if (eq tag 'img)
                    (append (and safe `((src . ,safe))) `((alt . ,text)))
                  (and safe `((href . ,safe))))))
    (when (and title (not (string-empty-p title)))
      (setq attrs (append attrs `((title . ,title)))))
    (if (eq tag 'img)
        `(img ,attrs)
      `(a ,attrs ,@(md--inline text)))))

(defun md--inline-ref (tag text label end)
  "Resolve a reference link for TEXT under LABEL.
TAG and END are as in `md--inline-at'."
  (let ((target (gethash (downcase (string-trim (if (string-empty-p label) text label)))
                         md--refs)))
    (when target
      (cons (md--link-node tag text (car target) (cdr target)) end))))

(defun md--inline-link-1 (s pos tag offset)
  "Shared body for links and images in S at POS.
TAG is `a' or `img'.  OFFSET is 0 for a link and 1 for an image."
  (let* ((bracket (+ pos offset))
         (close (md--matched bracket md--brackets)))
    (when close
      (let ((text (substring s (1+ bracket) close))
            (next (1+ close)))
        (cond
         ;; Inline destination: [text](url "title")
         ((and (< next (length s)) (eq (aref s next) ?\())
          (pcase (md--parse-destination s next)
            (`(,url ,title ,end) (cons (md--link-node tag text url title) end))))
         ;; Full reference: [text][label]
         ((and (< next (length s)) (eq (aref s next) ?\[))
          (let ((rclose (md--matched next md--brackets)))
            (when rclose
              (md--inline-ref tag text (substring s (1+ next) rclose) (1+ rclose)))))
         ;; Shortcut reference: [label]
         (t (md--inline-ref tag text "" next)))))))

(defun md--inline-link (s pos)
  "Parse a link in S at POS."
  (md--inline-link-1 s pos 'a 0))

(defun md--inline-image (s pos)
  "Parse an image in S at POS."
  (when (and (< (1+ pos) (length s)) (eq (aref s (1+ pos)) ?\[))
    (md--inline-link-1 s pos 'img 1)))

(defun md--inline-strike (s pos)
  "Parse a strikethrough span in S at POS."
  (when (eq (md--run-length s pos ?~) 2)
    (let ((close (md--find-closer s (+ pos 2) ?~ 2)))
      (when close
        (cons `(del nil ,@(md--inline (substring s (+ pos 2) close)))
              (+ close 2))))))

(defun md--run-length (s pos char)
  "Length of the run of CHAR in S starting at POS."
  (let ((n 0) (len (length s)))
    (while (and (< (+ pos n) len) (eq (aref s (+ pos n)) char))
      (setq n (1+ n)))
    n))

(defun md--emphasis-ok-p (s pos char width close)
  "Non-nil if an emphasis run in S is a real delimiter.
POS is where the opening run starts, CHAR its character, WIDTH its
length and CLOSE the index of the closing run.  Underscores inside a
word -- snake_case -- are not emphasis; asterisks always are."
  (or (not (eq char ?_))
      (and (or (zerop pos)
               (not (string-match-p "[[:alnum:]]" (string (aref s (1- pos))))))
           (let ((after (+ close width)))
             (or (>= after (length s))
                 (not (string-match-p "[[:alnum:]]" (string (aref s after)))))))))

(defun md--find-closer (s start char width)
  "Index in S at or after START of a closing run of WIDTH or more CHARs."
  (let ((key (list 'closer char width)))
    (unless (md--exhausted-p key)
      (let ((len (length s)) (index start) (found nil))
        (while (and (< index len) (not found))
          (cond
           ((eq (aref s index) ?\\) (setq index (+ index 2)))
           ((eq (aref s index) char)
            (let ((run (md--run-length s index char)))
              (if (and (>= run width)
                       ;; A closing run is never preceded by whitespace.
                       (not (memq (aref s (1- index)) '(?\s ?\t ?\n))))
                  (setq found index)
                (setq index (+ index run)))))
           (t (setq index (1+ index)))))
        (or found (md--exhaust key))))))

(defun md--emphasis-node (width body)
  "Wrap BODY nodes according to a delimiter run of WIDTH."
  (pcase width
    (1 `(em nil ,@body))
    (2 `(strong nil ,@body))
    (_ `(strong nil (em nil ,@body)))))

(defun md--inline-emphasis (s pos)
  "Parse emphasised text in S at POS."
  (let* ((char (aref s pos))
         (width (min 3 (md--run-length s pos char)))
         (start (+ pos width)))
    ;; A run at end of string, or one followed by whitespace, opens nothing.
    (when (and (< start (length s))
               (not (memq (aref s start) '(?\s ?\t ?\n))))
      (let ((close (md--find-closer s start char width)))
        (when (and close (md--emphasis-ok-p s pos char width close))
          (cons (md--emphasis-node width (md--inline (substring s start close)))
                (+ close width)))))))

;;; Block parsing

(defconst md--atx-re
  "\\`[ \t]\\{0,3\\}\\(#\\{1,6\\}\\)\\(?:[ \t]+\\(.*?\\)\\)?[ \t]*\\'")
(defconst md--fence-re
  "\\`\\( \\{0,3\\}\\)\\(```+\\|~~~+\\)[ \t]*\\(.*?\\)[ \t]*\\'")
(defconst md--thematic-re
  "\\`[ \t]\\{0,3\\}\\(\\(?:\\*[ \t]*\\)\\{3,\\}\\|\\(?:-[ \t]*\\)\\{3,\\}\\|\\(?:_[ \t]*\\)\\{3,\\}\\)\\'")
(defconst md--quote-re "\\`[ \t]\\{0,3\\}>")
(defconst md--setext-re "\\`[ \t]\\{0,3\\}\\(=+\\|-+\\)[ \t]*\\'")

(defun md--attrs (line)
  "Attribute alist recording the source line number of LINE."
  `((data-line . ,(number-to-string (md--ln line)))))

(defun md--list-marker (text)
  "Return (TYPE NUMBER CONTENT-COLUMN DELIMITER) if TEXT starts a list item."
  (cond
   ((string-match "\\`\\( \\{0,3\\}\\)\\([-+*]\\)\\(?:\\([ \t]+\\)\\|\\'\\)" text)
    (list 'ul nil (match-end 0) (match-string 2 text)))
   ((string-match "\\`\\( \\{0,3\\}\\)\\([0-9]\\{1,9\\}\\)\\([.)]\\)\\(?:\\([ \t]+\\)\\|\\'\\)" text)
    (list 'ol (string-to-number (match-string 2 text))
          (match-end 0) (match-string 3 text)))))

(defun md--block-start-p (text)
  "Non-nil if TEXT begins a block and so interrupts a paragraph."
  (or (string-match-p md--atx-re text)
      (string-match-p md--fence-re text)
      (string-match-p md--thematic-re text)
      (string-match-p md--quote-re text)
      ;; Only a list numbered 1 may interrupt a paragraph, so that a line
      ;; like "2026. A good year" stays prose.
      (let ((marker (md--list-marker text)))
        (and marker
             (or (eq (nth 0 marker) 'ul) (eql (nth 1 marker) 1))
             (not (md--blank-p (substring text (min (nth 2 marker) (length text)))))))))

(defun md--code-text (lines strip)
  "Join LINES into code text, removing STRIP leading spaces from each."
  (concat (string-join (mapcar #'md--txt (md--dedent lines strip)) "\n") "\n"))

(defun md--try-fence (lines)
  "Parse a fenced code block at the head of LINES."
  (let ((text (md--txt (car lines))))
    (when (string-match md--fence-re text)
      (let ((indent (length (match-string 1 text)))
            (fence (match-string 2 text))
            (info (match-string 3 text)))
        ;; A backtick fence may not carry backticks in its info string.
        (unless (and (eq (aref fence 0) ?`) (string-match-p "`" info))
          (let ((close-re (format "\\`[ \t]\\{0,3\\}%c\\{%d,\\}[ \t]*\\'"
                                  (aref fence 0) (length fence)))
                (rest (cdr lines))
                (body '())
                (closed nil))
            (while (and rest (not closed))
              (if (string-match-p close-re (md--txt (car rest)))
                  (setq closed t)
                (push (car rest) body))
              (setq rest (cdr rest)))
            (cons `(pre ,(append (md--attrs (car lines))
                                 ;; The body starts on the line after the fence.
                                 `((data-code-line
                                    . ,(number-to-string (1+ (md--ln (car lines))))))
                                 (let ((lang (car (split-string info))))
                                   (and lang `((class . ,(concat "language-" lang))))))
                        (code nil ,(md--code-text (nreverse body) indent)))
                  rest)))))))

(defun md--try-heading (lines)
  "Parse an ATX heading at the head of LINES."
  (let ((text (md--txt (car lines))))
    (when (string-match md--atx-re text)
      (let ((level (length (match-string 1 text)))
            (body (or (match-string 2 text) "")))
        (setq body (replace-regexp-in-string "[ \t]+#+[ \t]*\\'" "" body))
        (cons `(,(intern (format "h%d" level)) ,(md--attrs (car lines))
                ,@(md--inline body))
              (cdr lines))))))

(defun md--try-thematic-break (lines)
  "Parse a thematic break at the head of LINES."
  (when (string-match-p md--thematic-re (md--txt (car lines)))
    (cons `(hr ,(md--attrs (car lines))) (cdr lines))))

(defun md--try-blockquote (lines)
  "Parse a blockquote at the head of LINES."
  (when (and (< md--depth md--max-depth)
             (string-match-p md--quote-re (md--txt (car lines))))
    (let ((body '()) (rest lines) (done nil))
      (while (and rest (not done))
        (let ((text (md--txt (car rest))))
          (cond
           ((string-match "\\`[ \t]\\{0,3\\}> ?" text)
            (push (cons (md--ln (car rest)) (substring text (match-end 0))) body)
            (setq rest (cdr rest)))
           ;; Lazy continuation: prose under a quote stays in the quote.
           ((and body (not (md--blank-p text)) (not (md--block-start-p text)))
            (push (car rest) body)
            (setq rest (cdr rest)))
           (t (setq done t)))))
      (cons `(blockquote ,(md--attrs (car lines))
                         ,@(let ((md--depth (1+ md--depth)))
                             (md--parse-blocks (nreverse body))))
            rest))))

(defun md--unwrap-paragraphs (blocks)
  "Splice the children of top-level paragraphs in BLOCKS.
Tight list items hold inline content directly rather than a paragraph."
  (mapcan (lambda (node)
            (if (and (consp node) (eq (dom-tag node) 'p))
                (copy-sequence (dom-children node))
              (list node)))
          blocks))

(defconst md--checkbox-re "\\`\\[\\([ xX]\\)\\][ \t]+")

(defun md--parse-item (item-lines content-col)
  "Parse one list item from ITEM-LINES, dedenting by CONTENT-COL."
  (let* ((first (car item-lines))
         (text (md--txt first))
         (checkbox nil))
    (when (string-match md--checkbox-re text)
      (setq checkbox (if (equal (downcase (match-string 1 text)) "x")
                         "\u2611 " "\u2610 "))
      (setq first (cons (md--ln first) (substring text (match-end 0)))))
    (let ((blocks (let ((md--depth (1+ md--depth)))
                    (md--parse-blocks
                     (cons first (md--dedent (cdr item-lines) content-col))))))
      (if (not checkbox)
          blocks
        ;; Push the checkbox glyph into the first block so it renders inline.
        (let ((head (car blocks)))
          (if (and (consp head) (memq (dom-tag head) '(p)))
              (cons (append (list (dom-tag head) (dom-attributes head) checkbox)
                            (dom-children head))
                    (cdr blocks))
            (cons checkbox blocks)))))))

(defun md--list-node (type number items loose)
  "Build a list node of TYPE from ITEMS.
ITEMS are (SOURCE-LINE . BLOCKS) pairs.  NUMBER is the first ordinal,
LOOSE whether items keep their paragraphs.

Each item records its own source line.  A tight item has its paragraph
unwrapped, taking the `data-line\' attribute with it, so without this
the whole list would map back to whatever block preceded it."
  `(,type ,(append (and (eq type 'ol) number (/= number 1)
                        `((start . ,(number-to-string number))))
                   (and items `((data-line . ,(number-to-string (caar items))))))
          ,@(mapcar (lambda (item)
                      `(li ((data-line . ,(number-to-string (car item))))
                           ,@(if loose (cdr item)
                               (md--unwrap-paragraphs (cdr item)))))
                    items)))

(defun md--try-list (lines)
  "Parse a list at the head of LINES."
  (let ((first (and (< md--depth md--max-depth)
                    (md--list-marker (md--txt (car lines))))))
    (when first
      (let ((type (nth 0 first))
            (number (nth 1 first))
            (delim (nth 3 first))
            (items '())
            (loose nil)
            (rest lines)
            (done nil))
        (while (not done)
          (let ((marker (and rest (md--list-marker (md--txt (car rest))))))
            (if (not (and marker (eq (nth 0 marker) type) (equal (nth 3 marker) delim)))
                (setq done t)
              (let* ((content-col (nth 2 marker))
                     (head (car rest))
                     (item-lines (list (cons (md--ln head)
                                             (substring (md--txt head)
                                                        (min content-col
                                                             (length (md--txt head)))))))
                     (r (cdr rest))
                     (item-done nil))
                (while (not item-done)
                  (cond
                   ((null r) (setq item-done t))
                   ((md--blank-p (md--txt (car r)))
                    (let ((look r) (blanks '()))
                      (while (and look (md--blank-p (md--txt (car look))))
                        (push (car look) blanks)
                        (setq look (cdr look)))
                      (cond
                       ;; Indented content after a blank continues this item,
                       ;; and makes the whole list loose.
                       ((and look (>= (md--indent (md--txt (car look))) content-col))
                        (setq loose t)
                        (setq item-lines (append item-lines (nreverse blanks)))
                        (setq r look))
                       (t
                        ;; A blank line before another item of *this* list
                        ;; makes the list loose.  A different list starting
                        ;; below is not this list's business.
                        (when (and look
                                   (let ((m (md--list-marker (md--txt (car look)))))
                                     (and m (eq (nth 0 m) type)
                                          (equal (nth 3 m) delim))))
                          (setq loose t))
                        (setq r look)
                        (setq item-done t)))))
                   ((>= (md--indent (md--txt (car r))) content-col)
                    (setq item-lines (append item-lines (list (car r))))
                    (setq r (cdr r)))
                   ((and (not (md--list-marker (md--txt (car r))))
                         (not (md--block-start-p (md--txt (car r)))))
                    (setq item-lines (append item-lines (list (car r))))
                    (setq r (cdr r)))
                   (t (setq item-done t))))
                (push (cons (md--ln head) (md--parse-item item-lines content-col))
                      items)
                (setq rest r)))))
        (when items
          (cons (md--list-node type number (nreverse items) loose) rest))))))

;;; Tables

(defun md--split-cells (text)
  "Split TEXT on unescaped pipes that are not inside a code span."
  (let ((cells '()) (start 0) (i 0) (len (length text)))
    (while (< i len)
      (cond
       ((eq (aref text i) ?\\) (setq i (+ i 2)))
       ;; A pipe inside `code | span` is content, not a cell boundary.
       ((eq (aref text i) ?`)
        (let* ((ticks (md--run-length text i ?`))
               (fence (make-string ticks ?`))
               (close (string-search fence text (+ i ticks))))
          (setq i (if close (+ close ticks) len))))
       ((eq (aref text i) ?|)
        (push (substring text start i) cells)
        (setq i (1+ i) start i))
       (t (setq i (1+ i)))))
    (push (substring text start) cells)
    (mapcar (lambda (c) (string-trim (string-replace "\\|" "|" c)))
            (nreverse cells))))

(defun md--row-cells (text)
  "Split a table row TEXT into trimmed cell strings."
  (let ((cells (md--split-cells (string-trim text))))
    ;; Outer pipes produce empty leading and trailing cells.
    (when (and cells (string-empty-p (car cells)))
      (setq cells (cdr cells)))
    (when (and cells (string-empty-p (car (last cells))))
      (setq cells (butlast cells)))
    cells))

(defun md--table-delimiter-p (text)
  "Non-nil if TEXT is a table delimiter row."
  (and (string-match-p "|" text)
       (string-match-p "\\`[ \t]*|?\\(?:[ \t]*:?-+:?[ \t]*|\\)*[ \t]*:?-+:?[ \t]*|?[ \t]*\\'"
                       text)))

(defun md--alignment (spec)
  "Return the alignment named by delimiter cell SPEC."
  (let ((left (string-prefix-p ":" spec))
        (right (string-suffix-p ":" spec)))
    (cond ((and left right) "center")
          (right "right")
          (left "left"))))

(defun md--cell (tag text align line)
  "Build a table cell of TAG holding TEXT, aligned per ALIGN.
LINE is the source line of the row, recorded on the cell rather than on
the row: `shr\' renders each cell in a temporary buffer and reinserts it
as a propertized string, so only the cell's own text survives."
  `(,tag ,(append (and align `((align . ,align)))
                  `((data-line . ,(number-to-string line))))
         ,@(md--inline text)))

(defun md--try-table (lines)
  "Parse a GFM pipe table at the head of LINES."
  (when (and (cdr lines)
             (string-match-p "|" (md--txt (car lines)))
             (md--table-delimiter-p (md--txt (cadr lines))))
    (let* ((headers (md--row-cells (md--txt (car lines))))
           (aligns (mapcar #'md--alignment (md--row-cells (md--txt (cadr lines)))))
           (width (length headers))
           (rest (cddr lines))
           (rows '()))
      (while (and rest
                  (string-match-p "|" (md--txt (car rest)))
                  (not (md--blank-p (md--txt (car rest)))))
        (let ((cells (md--row-cells (md--txt (car rest)))))
          ;; Ragged rows are padded or truncated to the header width.
          (setq cells (if (> (length cells) width)
                          (seq-take cells width)
                        (append cells (make-list (- width (length cells)) ""))))
          (push `(tr ,(md--attrs (car rest))
                     ,@(seq-map-indexed
                        (lambda (cell i) (md--cell 'td cell (nth i aligns) (md--ln (car rest))))
                        cells))
                rows))
        (setq rest (cdr rest)))
      ;; shr double-boxes a table whose rows sit under thead or tbody, so the
      ;; rows must be direct children of the table element.
      (cons `(table ,(md--attrs (car lines))
                    (tr ,(md--attrs (car lines))
                        ,@(seq-map-indexed
                           (lambda (cell i) (md--cell 'th cell (nth i aligns) (md--ln (car lines))))
                           headers))
                    ,@(nreverse rows))
            rest))))

;;; Raw HTML, indented code, paragraphs

(defconst md--html-allowed-tags
  '(p div span br hr em strong b i u s del ins mark small sub sup
    code pre kbd samp var abbr cite q time
    a img ul ol li dl dt dd blockquote
    h1 h2 h3 h4 h5 h6
    table tr td th caption figure figcaption)
  "HTML tags spliced into the document as-is.
Everything else is dropped.  Markdown files arrive from cloned repos
and are not trusted, so this is an allowlist: `script', `style',
`base', `object', `iframe', `svg' and the media tags all carry
behaviour or fetch resources that a document renderer should not.")

(defconst md--html-container-tags '(thead tbody tfoot)
  "Tags whose children are spliced into the parent.
`shr' draws a nested box around a table whose rows sit under `thead'
or `tbody', so those wrappers are removed.")

(defconst md--html-allowed-attrs '(href src alt title align colspan rowspan)
  "HTML attributes kept when splicing raw HTML.")

(defconst md--html-node-limit 2000
  "Maximum number of nodes accepted from one raw HTML block.")

(defconst md--html-depth-limit 32
  "Maximum nesting depth accepted from one raw HTML block.")

(defun md--sanitize-html (nodes)
  "Return NODES with unsafe tags, attributes and URLs removed."
  (let ((count 0))
    (cl-labels
        ((walk (node depth)
           (cond
            ((> depth md--html-depth-limit) nil)
            ((>= count md--html-node-limit) nil)
            ((stringp node) (setq count (1+ count)) (list node))
            ((not (consp node)) nil)
            (t
             (setq count (1+ count))
             (let* ((tag (dom-tag node))
                    (children (mapcan (lambda (c) (walk c (1+ depth)))
                                      (dom-children node))))
               (cond
                ((memq tag md--html-container-tags) children)
                ((not (memq tag md--html-allowed-tags)) nil)
                (t
                 (let ((attrs (seq-filter
                               (lambda (pair)
                                 (and (memq (car pair) md--html-allowed-attrs)
                                      (or (not (memq (car pair) '(href src)))
                                          (md--safe-url-p (cdr pair)))))
                               (dom-attributes node))))
                   (list (append (list tag attrs) children))))))))))
      (mapcan (lambda (n) (walk n 0)) nodes))))

(defconst md--html-open-re
  "\\`[ \t]\\{0,3\\}\\(?:</?[a-zA-Z][a-zA-Z0-9-]*[ \t>/]\\|<!\\)"
  "A line opening a raw HTML block.
The tag name must be followed by whitespace, `>\' or `/\', so that an
autolink such as <http://example.com> is not mistaken for HTML.")

(defun md--html-to-blank (lines)
  "Collect an HTML block from LINES up to the next blank line."
  (let ((body '()) (rest lines))
    (while (and rest (not (md--blank-p (md--txt (car rest)))))
      (push (car rest) body)
      (setq rest (cdr rest)))
    (cons (nreverse body) rest)))

(defun md--html-extent (lines)
  "Collect the HTML block at the head of LINES.
An element opened on the first line runs to its closing tag, so that
blank lines inside a `<div>` do not truncate it."
  (let* ((first (md--txt (car lines)))
         (tag (and (string-match "\\`[ 	]\\{0,3\\}<\\([a-zA-Z][a-zA-Z0-9-]*\\)" first)
                   (downcase (match-string 1 first))))
         (closer (and tag (format "</%s>" tag))))
    (if (or (null closer) (string-match-p (regexp-quote closer) first))
        (md--html-to-blank lines)
      (let ((body '()) (rest lines) (done nil))
        (while (and rest (not done))
          (push (car rest) body)
          (when (string-match-p (regexp-quote closer) (downcase (md--txt (car rest))))
            (setq done t))
          (setq rest (cdr rest)))
        (if done (cons (nreverse body) rest) (md--html-to-blank lines))))))

(defun md--try-html (lines)
  "Parse a raw HTML block at the head of LINES."
  (when (and md-parse-html-blocks
             (fboundp 'libxml-parse-html-region)
             (string-match-p md--html-open-re (md--txt (car lines))))
    (pcase-let* ((`(,body . ,rest) (md--html-extent lines))
                 (html (string-join (mapcar #'md--txt body) "\n"))
                 (dom (with-temp-buffer
                        (insert html)
                        (libxml-parse-html-region (point-min) (point-max))))
                 ;; libxml wraps a fragment in (html nil (body nil ...)); take
                 ;; the body node itself, not the list `dom-by-tag' returns.
                 (root (and dom (or (dom-child-by-tag dom 'body) dom)))
                 (nodes (and root (md--sanitize-html (dom-children root)))))
      (cons (if nodes
                `(div ,(md--attrs (car lines)) ,@nodes)
              `(p ,(md--attrs (car lines)) ,(md--text html)))
            rest))))

(defun md--try-indented-code (lines)
  "Parse an indented code block at the head of LINES."
  (when (>= (md--indent (md--txt (car lines))) 4)
    (let ((body '()) (rest lines))
      (while (and rest (or (md--blank-p (md--txt (car rest)))
                           (>= (md--indent (md--txt (car rest))) 4)))
        (push (car rest) body)
        (setq rest (cdr rest)))
      (cons `(pre ,(append (md--attrs (car lines))
                           `((data-code-line
                              . ,(number-to-string (md--ln (car lines))))))
                  (code nil ,(md--code-text
                              (md--drop-trailing-blanks (nreverse body)) 4)))
            rest))))

(defun md--paragraph-text (lines)
  "Join LINES into paragraph text, marking hard line breaks."
  (string-replace
   (concat md--hard-break "\n") md--hard-break
   (string-join
    (mapcar (lambda (line)
              (let ((text (string-trim-left (md--txt line))))
                (if (string-match "\\(  +\\|\\\\\\)\\'" text)
                    (concat (substring text 0 (match-beginning 0)) md--hard-break)
                  text)))
            lines)
    "\n")))

(defun md--try-paragraph (lines)
  "Parse a paragraph, or a setext heading, at the head of LINES."
  (let ((body '()) (rest lines) (done nil) (level nil))
    (while (and rest (not done))
      (let ((text (md--txt (car rest))))
        (cond
         ((md--blank-p text) (setq done t))
         ;; A setext underline only counts under existing paragraph text.
         ((and body (string-match md--setext-re text))
          (setq level (if (eq (aref (string-trim text) 0) ?=) 1 2))
          (setq rest (cdr rest))
          (setq done t))
         ((and body (md--block-start-p text)) (setq done t))
         (t (push (car rest) body) (setq rest (cdr rest))))))
    (setq body (nreverse body))
    (cons (if body
              `(,(if level (intern (format "h%d" level)) 'p)
                ,(md--attrs (car body))
                ,@(md--inline (md--paragraph-text body)))
            nil)
          (if body rest (cdr lines)))))

(defun md--parse-blocks (lines)
  "Parse LINES into a list of block-level DOM nodes."
  (let ((nodes '()))
    (while lines
      (if (md--blank-p (md--txt (car lines)))
          (setq lines (cdr lines))
        (let ((hit (or (md--try-fence lines)
                       (md--try-heading lines)
                       (md--try-thematic-break lines)
                       (md--try-blockquote lines)
                       (md--try-list lines)
                       (md--try-table lines)
                       (md--try-html lines)
                       (md--try-indented-code lines)
                       (md--try-paragraph lines))))
          (when (car hit) (push (car hit) nodes))
          (setq lines (cdr hit)))))
    (nreverse nodes)))

;;; Document

(defun md--front-matter (lines)
  "Split leading YAML front matter off LINES.
Return (NODE . REST), where NODE may be nil."
  (if (not (and lines (member (string-trim (md--txt (car lines))) '("---"))))
      (cons nil lines)
    (let ((rest (cdr lines)) (body '()) (closed nil))
      (while (and rest (not closed))
        (if (member (string-trim (md--txt (car rest))) '("---" "..."))
            (setq closed t)
          (push (car rest) body))
        (setq rest (cdr rest)))
      (cond
       ((not closed) (cons nil lines))
       (md-parse-front-matter
        (cons `(pre ,(md--attrs (car lines))
                    (code nil ,(md--code-text (nreverse body) 0)))
              rest))
       (t (cons nil rest))))))

(defun md--collect-refs (lines)
  "Record link reference definitions from LINES and return LINES without them.
Definitions inside fenced code are left alone."
  (let ((out '()) (fence nil))
    (dolist (line lines)
      (let ((text (md--txt line)))
        (cond
         (fence
          (when (string-match-p fence text) (setq fence nil))
          (push line out))
         ((string-match md--fence-re text)
          (setq fence (format "\\`[ \t]\\{0,3\\}%c\\{%d,\\}[ \t]*\\'"
                              (aref (match-string 2 text) 0)
                              (length (match-string 2 text))))
          (push line out))
         ((and (< (md--indent text) 4)
               (string-match
                (concat "\\`[ \t]*\\[\\([^]]+\\)\\][ \t]*:[ \t]*"
                        "\\(<[^>]*>\\|[^[:space:]]+\\)"
                        "\\(?:[ \t]+[\"'(]\\(.*\\)[\"')]\\)?[ \t]*\\'")
                text))
          (puthash (downcase (string-trim (match-string 1 text)))
                   (cons (string-trim (match-string 2 text) "<" ">")
                         (match-string 3 text))
                   md--refs))
         (t (push line out)))))
    (nreverse out)))

;;;###autoload
(defun md-parse-string (string)
  "Parse Markdown STRING into a DOM tree that `shr' can render."
  (let* ((md--refs (make-hash-table :test #'equal))
         (clean (replace-regexp-in-string
                 "\r\n?" "\n" (string-replace md--hard-break "" string)))
         (split (split-string clean "\n"))
         ;; A trailing newline leaves a phantom empty final line, which would
         ;; show up as a blank line inside an unclosed fence.
         (split (if (and (cdr split) (equal (car (last split)) ""))
                    (butlast split)
                  split))
         (lines (md--number-lines split)))
    (pcase-let ((`(,front . ,rest) (md--front-matter lines)))
      (setq rest (md--collect-refs rest))
      `(div nil ,@(and front (list front)) ,@(md--parse-blocks rest)))))

;;;###autoload
(defun md-parse-buffer (&optional buffer)
  "Parse BUFFER, or the current buffer, into a DOM tree."
  (with-current-buffer (or buffer (current-buffer))
    (md-parse-string (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'md-parse)
;;; md-parse.el ends here
