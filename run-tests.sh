#!/bin/sh
# Byte-compile and run the test suites.
set -e
cd "$(dirname "$0")"
rm -f ./*.elc
echo "== byte-compiling =="
# A warning fails the build.  MELPA will not accept a package that compiles
# with warnings, so the rule is enforced here rather than hoped for.
emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
      -f batch-byte-compile md-parse.el md-render.el md-link.el md-outline.el md-mode.el
echo "== manual =="
emacs -Q --batch --eval '(let ((make-backup-files nil))
  (require (quote texinfmt))
  (find-file "doc/md-mode.texi")
  (texinfo-format-buffer)
  (save-buffer))' >/dev/null
emacs -Q --batch --eval '(let ((file (expand-file-name "doc/md-mode.info")))
  (require (quote info))
  (dolist (node (list "Top" "Installation" "Reading" "Navigating" "File references"
                      "Other documents" "Outline" "Side by side" "Options" "Safety"
                      "Design" "Index"))
    (Info-find-node file node))
  (princ "all manual nodes resolve\n"))'

echo "== tests =="
emacs -Q --batch -L . -L test \
      -l test/md-parse-tests.el -l test/md-render-tests.el \
      -l test/md-nav-tests.el \
      -f ert-run-tests-batch-and-exit
