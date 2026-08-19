#!/bin/sh
# Byte-compile and run the test suites.
set -e
cd "$(dirname "$0")"
rm -f ./*.elc
echo "== byte-compiling =="
emacs -Q --batch -L . --eval '(setq byte-compile-error-on-warn nil)' \
      -f batch-byte-compile md-parse.el md-render.el md-mode.el
echo "== tests =="
emacs -Q --batch -L . -L test \
      -l test/md-parse-tests.el -l test/md-render-tests.el \
      -f ert-run-tests-batch-and-exit
