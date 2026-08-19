;;; md-parse-tests.el --- Tests for the Markdown parser  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'md-parse)
(require 'dom)

(defconst md-test--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the test files.
Captured at load time; `load-file-name\' is nil once a test body runs.")

(defun md-test--blocks (markdown)
  "Parse MARKDOWN and return its block nodes, stripped of source lines."
  (md-test--strip (cdr (cdr (md-parse-string markdown)))))

(defun md-test--strip (node)
  "Remove source-line bookkeeping from NODE so tests can compare shapes."
  (cond
   ((stringp node) node)
   ((not (consp node)) node)
   ((and (symbolp (car node)) (or (null (cadr node)) (consp (cadr node))))
    (append (list (dom-tag node)
                  (seq-remove (lambda (pair)
                                (memq (car pair) '(data-line data-code-line)))
                              (dom-attributes node)))
            (mapcar #'md-test--strip (dom-children node))))
   (t (mapcar #'md-test--strip node))))

;;; The DOM contract
;;
;; shr reads nodes with `dom.el' accessors, which assume every element is
;; (TAG ATTRIBUTE-ALIST CHILD...).  A node missing its attribute slot raises
;; wrong-type-argument deep inside the renderer, so the shape is checked
;; directly rather than waiting for a render to fail.

(defun md-test--valid-node-p (node)
  "Signal unless NODE and its descendants are well-formed DOM nodes."
  (cond
   ((stringp node) t)
   ((not (consp node)) (error "Not a node: %S" node))
   (t
    (unless (symbolp (dom-tag node))
      (error "Tag is not a symbol: %S" node))
    (let ((attrs (cadr node)))
      (unless (listp attrs)
        (error "Missing attribute slot: %S" node))
      (dolist (pair attrs)
        (unless (and (consp pair) (symbolp (car pair)) (stringp (cdr pair)))
          (error "Bad attribute %S in %S" pair node))))
    (mapc #'md-test--valid-node-p (dom-children node))
    t)))

(ert-deftest md-parse-produces-valid-dom ()
  (let ((corpus (expand-file-name "corpus.md" md-test--directory)))
    (should (md-test--valid-node-p (md-parse-string
                                    (with-temp-buffer
                                      (insert-file-contents corpus)
                                      (buffer-string)))))))

;;; Headings

(ert-deftest md-parse-atx-headings ()
  (should (equal (md-test--blocks "# One\n") '((h1 nil "One"))))
  (should (equal (md-test--blocks "###### Six\n") '((h6 nil "Six"))))
  (should (equal (md-test--blocks "####### Seven\n") '((p nil "####### Seven"))))
  (should (equal (md-test--blocks "## Closed ##\n") '((h2 nil "Closed")))))

(ert-deftest md-parse-setext-headings ()
  (should (equal (md-test--blocks "Title\n=====\n") '((h1 nil "Title"))))
  (should (equal (md-test--blocks "Title\n-----\n") '((h2 nil "Title"))))
  ;; With no paragraph above it, a run of dashes is a thematic break.
  (should (equal (md-test--blocks "-----\n") '((hr nil)))))

;;; Inline spans

(ert-deftest md-parse-emphasis ()
  (should (equal (md-test--blocks "*i*\n") '((p nil (em nil "i")))))
  (should (equal (md-test--blocks "**b**\n") '((p nil (strong nil "b")))))
  (should (equal (md-test--blocks "***both***\n")
                 '((p nil (strong nil (em nil "both"))))))
  (should (equal (md-test--blocks "**a *b* c**\n")
                 '((p nil (strong nil "a " (em nil "b") " c"))))))

(ert-deftest md-parse-underscores-inside-words ()
  "snake_case is an identifier, not emphasis."
  (should (equal (md-test--blocks "a snake_case_name here\n")
                 '((p nil "a snake_case_name here"))))
  (should (equal (md-test--blocks "_yes_\n") '((p nil (em nil "yes"))))))

(ert-deftest md-parse-code-spans ()
  (should (equal (md-test--blocks "a `b | c` d\n")
                 '((p nil "a " (code nil "b | c") " d"))))
  ;; Doubled backticks so the span can hold a backtick.
  (should (equal (md-test--blocks "`` ` ``\n") '((p nil (code nil "`"))))))

(ert-deftest md-parse-escapes ()
  (should (equal (md-test--blocks "\\*not emphasis\\*\n")
                 '((p nil "*" "not emphasis" "*")))))

(ert-deftest md-parse-links-and-images ()
  (should (equal (md-test--blocks "[t](http://x \"ti\")\n")
                 '((p nil (a ((href . "http://x") (title . "ti")) "t")))))
  (should (equal (md-test--blocks "![a](p.png)\n")
                 '((p nil (img ((src . "p.png") (alt . "a")))))))
  ;; Parentheses inside a destination stay inside it.
  (should (equal (md-test--blocks "[t](http://x/a(b)c)\n")
                 '((p nil (a ((href . "http://x/a(b)c")) "t"))))))

(ert-deftest md-parse-reference-links ()
  (should (equal (md-test--blocks "[t][r]\n\n[r]: http://x\n")
                 '((p nil (a ((href . "http://x")) "t")))))
  (should (equal (md-test--blocks "[r]\n\n[r]: http://x\n")
                 '((p nil (a ((href . "http://x")) "r")))))
  ;; A definition inside a fence is code, not a definition.
  (should (equal (md-test--blocks "```\n[r]: http://x\n```\n")
                 '((pre nil (code nil "[r]: http://x\n"))))))

(ert-deftest md-parse-autolinks ()
  (should (equal (md-test--blocks "<http://x.example>\n")
                 '((p nil (a ((href . "http://x.example")) "http://x.example")))))
  (should (equal (md-test--blocks "see http://x.example here\n")
                 '((p nil "see " (a ((href . "http://x.example")) "http://x.example")
                      " here")))))

(ert-deftest md-parse-hard-break ()
  (should (equal (md-test--blocks "one  \ntwo\n")
                 '((p nil "one" (br nil) "two")))))

;;; Lists

(ert-deftest md-parse-tight-list ()
  (should (equal (md-test--blocks "- a\n- b\n")
                 '((ul nil (li nil "a") (li nil "b"))))))

(ert-deftest md-parse-loose-list ()
  (should (equal (md-test--blocks "- a\n\n- b\n")
                 '((ul nil (li nil (p nil "a")) (li nil (p nil "b")))))))

(ert-deftest md-parse-list-followed-by-other-list-stays-tight ()
  "A blank line before a *different* list must not make this one loose."
  (should (equal (md-test--blocks "- a\n- b\n\n1. c\n")
                 '((ul nil (li nil "a") (li nil "b"))
                   (ol nil (li nil "c"))))))

(ert-deftest md-parse-nested-list ()
  (should (equal (md-test--blocks "- a\n  - b\n- c\n")
                 '((ul nil (li nil "a" (ul nil (li nil "b"))) (li nil "c"))))))

(ert-deftest md-parse-ordered-list-start ()
  (should (equal (md-test--blocks "3. c\n4. d\n")
                 '((ol ((start . "3")) (li nil "c") (li nil "d")))))
  ;; A paragraph may only be interrupted by a list starting at 1.
  (should (equal (md-test--blocks "text\n2026. a year\n")
                 '((p nil "text\n2026. a year")))))

(ert-deftest md-parse-task-list ()
  (should (equal (md-test--blocks "- [ ] no\n- [x] yes\n")
                 '((ul nil (li nil "☐ " "no") (li nil "☑ " "yes"))))))

(ert-deftest md-parse-fence-inside-list-item ()
  (should (equal (md-test--blocks "- item\n\n  ```\n  code\n  ```\n")
                 '((ul nil (li nil (p nil "item") (pre nil (code nil "code\n"))))))))

;;; Blockquotes

(ert-deftest md-parse-blockquote ()
  (should (equal (md-test--blocks "> a\n")
                 '((blockquote nil (p nil "a")))))
  (should (equal (md-test--blocks "> > deep\n")
                 '((blockquote nil (blockquote nil (p nil "deep")))))))

(ert-deftest md-parse-blockquote-lazy-continuation ()
  (should (equal (md-test--blocks "> one\ntwo\n")
                 '((blockquote nil (p nil "one\ntwo"))))))

(ert-deftest md-parse-table-inside-blockquote ()
  (should (equal (md-test--blocks "> | a | b |\n> |---|---|\n> | 1 | 2 |\n")
                 '((blockquote nil
                               (table nil
                                      (tr nil (th nil "a") (th nil "b"))
                                      (tr nil (td nil "1") (td nil "2"))))))))

;;; Code

(ert-deftest md-parse-fenced-code ()
  (should (equal (md-test--blocks "```elisp\n(+ 1 2)\n```\n")
                 '((pre ((class . "language-elisp")) (code nil "(+ 1 2)\n")))))
  (should (equal (md-test--blocks "~~~\nx\n~~~\n")
                 '((pre nil (code nil "x\n")))))
  ;; An unclosed fence runs to the end of the document.
  (should (equal (md-test--blocks "```\nx\n")
                 '((pre nil (code nil "x\n"))))))

(ert-deftest md-parse-indented-code ()
  (should (equal (md-test--blocks "    x = 1\n    y = 2\n")
                 '((pre nil (code nil "x = 1\ny = 2\n"))))))

;;; Tables

(ert-deftest md-parse-table-alignment ()
  (should (equal (md-test--blocks "| a | b | c |\n|:--|:-:|--:|\n| 1 | 2 | 3 |\n")
                 '((table nil
                          (tr nil (th ((align . "left")) "a")
                              (th ((align . "center")) "b")
                              (th ((align . "right")) "c"))
                          (tr nil (td ((align . "left")) "1")
                              (td ((align . "center")) "2")
                              (td ((align . "right")) "3")))))))

(ert-deftest md-parse-table-ragged-rows ()
  "Short rows are padded and long rows truncated to the header width."
  (should (equal (md-test--blocks "| a | b |\n|---|---|\n| 1 |\n| 1 | 2 | 3 |\n")
                 '((table nil
                          (tr nil (th nil "a") (th nil "b"))
                          (tr nil (td nil "1") (td nil))
                          (tr nil (td nil "1") (td nil "2")))))))

(ert-deftest md-parse-table-escaped-pipe ()
  (should (equal (md-test--blocks "| a | b |\n|---|---|\n| x \\| y | z |\n")
                 '((table nil
                          (tr nil (th nil "a") (th nil "b"))
                          (tr nil (td nil "x | y") (td nil "z")))))))

;;; Front matter and HTML

(ert-deftest md-parse-front-matter-hidden ()
  (should (equal (md-test--blocks "---\ntitle: x\n---\n\nBody\n")
                 '((p nil "Body")))))

(ert-deftest md-parse-front-matter-shown ()
  (let ((md-parse-front-matter t))
    (should (equal (md-test--blocks "---\ntitle: x\n---\n\nBody\n")
                   '((pre nil (code nil "title: x\n")) (p nil "Body"))))))

(ert-deftest md-parse-html-is-sanitized ()
  "Active and resource-fetching tags never reach the renderer."
  (dolist (tag '("script" "style" "iframe" "object" "svg" "base" "form"))
    (let ((dom (md-parse-string
                (format "<div>\n<p>ok</p>\n<%s>x</%s>\n</div>\n" tag tag))))
      (should (dom-by-tag dom 'p))
      (should-not (dom-by-tag dom (intern tag))))))

(ert-deftest md-parse-html-drops-unsafe-urls ()
  (dolist (url '("javascript:x" "data:text/html,x" "file:///etc/passwd"))
    (let* ((dom (md-parse-string (format "<p><a href=\"%s\">a</a></p>\n" url)))
           (link (car (dom-by-tag dom 'a))))
      (should link)
      (should-not (dom-attr link 'href))))
  (let ((link (car (dom-by-tag (md-parse-string "<p><a href=\"http://x\">a</a></p>\n") 'a))))
    (should (equal (dom-attr link 'href) "http://x"))))

(ert-deftest md-parse-html-spans-blank-lines ()
  "A block element runs to its closing tag, not to the first blank line."
  (let ((blocks (md-test--blocks "<div>\n\n<p>a</p>\n\n</div>\n\nAfter\n")))
    (should (equal (length blocks) 2))
    (should (dom-by-tag (car blocks) 'p))
    (should (equal (nth 1 blocks) '(p nil "After")))))

(provide 'md-parse-tests)
;;; md-parse-tests.el ends here

;;; Regressions
;;
;; Each of these reproduces a defect found in review.

(ert-deftest md-parse-unmatched-delimiters-are-linear ()
  "A wall of unmatched delimiters must not take quadratic time.
Before delimiter matches were precomputed, 8000 unmatched brackets took
about ten seconds; anything in this range now finishes in milliseconds."
  (dolist (char '(?\[ ?` ?* ?~ ?\())
    (let ((start (float-time)))
      (md-parse-string (make-string 20000 char))
      (should (< (- (float-time) start) 2.0)))))

(ert-deftest md-parse-deep-inline-nesting-does-not-overflow ()
  "Nested inline constructs are bounded rather than recursed to death."
  (let ((links "x") (emphasis "x"))
    (dotimes (_ 200)
      (setq links (concat "[" links "](u)"))
      (setq emphasis (concat "*" emphasis "*")))
    (should (md-parse-string links))
    (should (md-parse-string emphasis))))

(ert-deftest md-parse-deep-block-nesting-does-not-overflow ()
  (should (md-parse-string (make-string 400 ?>)))
  (should (md-parse-string (mapconcat #'identity
                                      (make-list 200 "- a") "\n"))))

(ert-deftest md-parse-link-urls-are-filtered ()
  "The URL policy covers Markdown links, not only raw HTML."
  (dolist (url '("javascript:alert(1)" "data:text/html,x" "file:///etc/passwd"))
    (let ((link (car (dom-by-tag (md-parse-string (format "[t](%s)" url)) 'a))))
      (should link)
      (should-not (dom-attr link 'href))))
  ;; Ordinary destinations, including relative paths, are untouched.
  (dolist (url '("http://x" "mailto:a@b" "../other.md" "img/x.png"))
    (let ((link (car (dom-by-tag (md-parse-string (format "[t](%s)" url)) 'a))))
      (should (equal (dom-attr link 'href) url)))))

(ert-deftest md-parse-control-characters-cannot-smuggle-a-scheme ()
  "An entity-encoded newline must not hide `javascript:' from the check."
  (let ((link (car (dom-by-tag
                    (md-parse-string "<p><a href=\"&#10;javascript:alert(1)\">x</a></p>")
                    'a))))
    (should link)
    (should-not (dom-attr link 'href))))

(ert-deftest md-parse-remote-image-paths-are-rejected ()
  "A TRAMP path would make Emacs open a network connection to display it."
  (let ((image (car (dom-by-tag (md-parse-string "![x](/ssh:host:/x.png)") 'img))))
    (should image)
    (should-not (dom-attr image 'src))))

(ert-deftest md-parse-html-node-limit-is-exact ()
  (let ((md--html-node-limit 3))
    (should (equal (md--sanitize-html '((div nil "a" "b" "c" "d")))
                   '((div nil "a" "b"))))))

(ert-deftest md-parse-table-rows-record-their-line ()
  (let* ((dom (md-parse-string "| a |\n|---|\n| 1 |\n| 2 |\n"))
         (rows (dom-by-tag dom 'tr)))
    (should (equal (mapcar (lambda (row) (dom-attr row 'data-line)) rows)
                   '("1" "3" "4")))))

(ert-deftest md-parse-code-blocks-record-their-body-line ()
  (should (equal (dom-attr (car (dom-by-tag (md-parse-string "\n```\nx\n```\n") 'pre))
                           'data-code-line)
                 "3"))
  (should (equal (dom-attr (car (dom-by-tag (md-parse-string "    x\n") 'pre))
                           'data-code-line)
                 "1")))
