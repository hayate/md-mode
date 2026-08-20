#!/bin/sh
# Check the package the way MELPA does: package-lint, then checkdoc.
#
# Kept apart from run-tests.sh because this one needs the network: it
# installs package-lint from MELPA into a throwaway directory, so your own
# package directory is left alone.
set -e
cd "$(dirname "$0")"

files="md-mode.el md-parse.el md-render.el md-link.el md-outline.el"
elpa="${TMPDIR:-/tmp}/md-mode-lint-elpa"

echo "== fetching package-lint =="
if ! fetched=$(emacs -Q --batch --eval "(progn
  (setq package-user-dir \"$elpa\")
  (require 'package)
  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)
  (package-initialize)
  (unless (package-installed-p 'package-lint)
    (package-refresh-contents)
    (package-install 'package-lint)))" 2>&1); then
    printf '%s\n' "$fetched"
    exit 1
fi

echo "== package-lint =="
emacs -Q --batch --eval "(progn
  (setq package-user-dir \"$elpa\")
  (require 'package)
  (package-initialize)
  (require 'package-lint)
  ;; Without this, every file but md-mode.el is read as a package of its own
  ;; and asked for headers only the main file should carry.
  (setq package-lint-main-file \"md-mode.el\"))" \
      -f package-lint-batch-and-exit $files
echo "no packaging problems"

echo "== checkdoc =="
# checkdoc reports by warning rather than by exit status, so any output at
# all is a failure.
problems=$(emacs -Q --batch --eval "(progn
  (require 'checkdoc)
  (dolist (file '($files))
    (checkdoc-file (symbol-name file))))" 2>&1)
if [ -n "$problems" ]; then
    printf '%s\n' "$problems"
    exit 1
fi
echo "no documentation problems"
