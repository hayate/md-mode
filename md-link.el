;;; md-link.el --- Follow references out of a rendered document  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea@byteset.com>
;; Assisted-by: Claude:claude-opus-5
;; URL: https://github.com/hayate/md-mode
;; Version: 0.2.0
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

;; A design document is half prose and half an index into a codebase.  This
;; file makes that index navigable: an inline code span holding something like
;; `lib/thing.py:147' becomes a link that opens the file at that line, and a
;; link to another Markdown file opens it rendered rather than in a browser.
;;
;; A reference becomes a link ONLY if it resolves to a readable file.  An
;; unreachable one renders as ordinary inline code, so linkiness is a live
;; signal that the document and the code still agree: move the document, or
;; the file, and the link quietly stops being one.
;;
;; Markdown arrives in cloned repositories, so references are untrusted.  A
;; candidate must canonicalise to a path inside one of the allowed roots -- so
;; `../../../../etc/passwd' resolves nowhere -- and remote names are refused
;; before any filesystem call, since merely asking whether /ssh:host:/x is
;; readable opens a connection.

;;; Code:

(require 'shr)
(require 'subr-x)
(require 'md-render)

(declare-function md--show-render "md-mode")
(declare-function md-link-visit-document "md-mode")
(declare-function project-root "project")
(declare-function project-current "project")

(defcustom md-link-roots nil
  "Extra directories that file references may resolve inside.
The document's own directory and its project root are always tried
first.  A folder of specs that points into several checkouts can set
this per-directory from `.dir-locals.el'.

A reference resolves only if its canonical path lies inside one of
these roots, so a document cannot turn `../../etc/passwd' into a link."
  :type '(repeat directory)
  :group 'md)

(defcustom md-link-max-references 500
  "Most file references resolved in one document.
Each resolution touches the filesystem, so a pathological document is
capped rather than allowed to stall redisplay."
  :type 'integer
  :group 'md)

(defcustom md-link-follow-markdown t
  "Whether a link to a local Markdown file opens rendered.
Nil opens the source instead."
  :type 'boolean
  :group 'md)

(defconst md-link-markdown-extensions '("md" "markdown" "mdown" "mkd")
  "File extensions treated as Markdown when following a link.")

;;; Recognising a reference
;;
;; The grammar is anchored to a whole code span rather than searched for
;; inside one.  Scanning substrings turns `{"total": 42}' and `12:30' into
;; references to files named `total' and `12'.

(defun md-link--split (text)
  "Split TEXT into (PATH . LINE); LINE is nil when absent."
  (if (string-match "\\`\\(.+\\):\\([0-9]+\\)\\'" text)
      (cons (match-string 1 text) (string-to-number (match-string 2 text)))
    (cons text nil)))

(defun md-link--path-like-p (path)
  "Non-nil if PATH could plausibly name a file.
Requires a directory separator or a file extension, so that the `12' of
a timestamp like 12:30 is not offered as a filename.  URLs and
drive-lettered paths are rejected outright."
  (and (not (string-empty-p path))
       (not (string-match-p "[[:space:]]" path))
       (not (string-search "://" path))
       ;; A leading scheme, or a Windows drive letter.
       (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" path))
       (or (string-search "/" path)
           (string-match-p "\\.[A-Za-z][A-Za-z0-9]\\{0,7\\}\\'" path))))

(defun md-link-parse (text)
  "Parse TEXT as a file reference, returning (PATH . LINE) or nil."
  (let* ((text (string-trim text))
         (split (md-link--split text)))
    (and (md-link--path-like-p (car split)) split)))

;;; Resolving it

(defvar md-link--cache (make-hash-table :test #'equal)
  "Resolutions already computed, so a resize does not re-stat the disk.")

(defun md-link--project-root (directory)
  "Root of the project containing DIRECTORY, or nil."
  (or (when (require 'project nil t)
        (when-let ((project (ignore-errors (project-current nil directory))))
          (expand-file-name (project-root project))))
      (when-let ((git (locate-dominating-file directory ".git")))
        (expand-file-name git))))

(defun md-link--roots (directory)
  "Directories a reference from DIRECTORY may resolve inside.
DIRECTORY is checked for remoteness first: project discovery walks the
filesystem looking for a marker, and doing that on a remote name opens
a connection before anything downstream gets a chance to refuse it."
  (let ((local (and directory
                    (not (file-remote-p directory))
                    (expand-file-name directory))))
    (delq nil
          (append (list local)
                  (list (and local (md-link--project-root local)))
                  (mapcar (lambda (root)
                            (and (stringp root)
                                 (not (file-remote-p root))
                                 (expand-file-name root)))
                          md-link-roots)))))

(defun md-link--contained-p (file root)
  "Non-nil if FILE lies inside ROOT once both are canonical."
  (let ((file (file-truename file))
        (root (file-name-as-directory (file-truename root))))
    (and (string-prefix-p root file) file)))

(defun md-link--resolve-1 (path directory)
  "Resolve PATH against the roots of DIRECTORY.  Return a filename or nil."
  ;; Refuse remote names before touching the filesystem: even asking whether
  ;; /ssh:host:/x is readable opens a connection.
  (unless (file-remote-p path)
    (let ((found nil))
      (dolist (root (md-link--roots directory))
        (unless (or found (file-remote-p root))
          (let ((candidate (expand-file-name path root)))
            (unless (file-remote-p candidate)
              (when-let ((true (md-link--contained-p candidate root)))
                (when (and (file-regular-p true) (file-readable-p true))
                  (setq found true)))))))
      found)))

(defun md-link-resolve (path directory)
  "Resolve PATH against DIRECTORY, caching the answer.
A width-only re-render must not walk the filesystem again, so a
resolution -- including a failed one -- is remembered.  Following a link
re-checks the file, so a cached answer can never open something stale."
  (let* ((key (list path directory md-link-roots))
         (hit (gethash key md-link--cache 'missing)))
    (if (eq hit 'missing)
        (progn
          (when (> (hash-table-count md-link--cache) 2000)
            (clrhash md-link--cache))
          (puthash key (md-link--resolve-1 path directory) md-link--cache))
      hit)))

;;; Turning a resolved reference into a link

(defvar md-link-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (default-value 'shr-map))
    ;; shr binds no mouse-1: `follow-link' makes Emacs translate a click into
    ;; mouse-2, so mouse-2 is the binding that has to be overridden.
    (keymap-set map "RET" #'md-link-follow)
    (keymap-set map "<mouse-2>" #'md-link-follow)
    (keymap-set map "v" #'md-link-follow)
    map)
  "Keymap on links in a rendered document.
Inherits `shr-map', so TAB, link copying and image commands still work.")

(defvar-local md-link--count 0
  "References resolved during the current render.")

(defun md-link--linkify-span (start end)
  "Make the code span between START and END a link if it resolves."
  (when (< md-link--count md-link-max-references)
    (let* ((text (buffer-substring-no-properties start end))
           (reference (md-link-parse text)))
      (when reference
        (setq md-link--count (1+ md-link--count))
        (when-let ((file (md-link-resolve (car reference)
                                          (or md--base-directory
                                              default-directory))))
          (add-text-properties
           start end
           (list 'md-link-file file
                 'md-link-line (cdr reference)
                 'keymap md-link-map
                 'mouse-face 'highlight
                 'follow-link t
                 'help-echo (if (cdr reference)
                                (format "%s, line %d" file (cdr reference))
                              file)))
          (put-text-property start (1+ start) 'shr-tab-stop t)
          (add-face-text-property start end 'shr-link t))))))

;;; Following

(defun md-link--markdown-p (file)
  "Non-nil if FILE has a Markdown extension."
  (member (downcase (or (file-name-extension file) ""))
          md-link-markdown-extensions))

(defun md-link--visit-code (file line)
  "Open FILE at LINE in another window."
  (cond
   ((not (file-readable-p file))
    (message "md-mode: %s is no longer readable" file))
   (t
    (find-file-other-window file)
    (when line
      (goto-char (point-min))
      (forward-line (1- line))))))

(defun md-link--has-scheme-p (url)
  "Non-nil if URL names something outside the filesystem."
  (and (stringp url) (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url)))

(defun md-link-follow ()
  "Follow the link at point.
A file reference opens the file at its line.  A link to a local
Markdown document opens it rendered, other local files open normally,
and anything with a scheme goes to the browser."
  (interactive)
  (let ((file (get-text-property (point) 'md-link-file))
        (url (get-text-property (point) 'shr-url)))
    (cond
     (file (md-link--visit-code file (get-text-property (point) 'md-link-line)))
     ((null url) (message "md-mode: no link at point"))
     ((md-link--has-scheme-p url) (shr-browse-url))
     (t
      ;; A local target that does not exist is reported, not handed to the
      ;; browser, which would treat "./nope.md" as a URL.
      (let ((target (md-link--local-target url)))
        (cond
         ((null target) (message "md-mode: cannot read %s from here" url))
         ((and md-link-follow-markdown (md-link--markdown-p target))
          (md-link-visit-document target))
         (t (find-file-other-window target))))))))

(defun md-link--local-target (url)
  "Return the local file URL names, or nil if it is not a local file."
  (and (stringp url)
       (not (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" url))
       (not (string-prefix-p "#" url))
       (let* ((path (car (split-string url "#")))
              (directory (or md--base-directory default-directory)))
         (unless (or (string-empty-p path) (file-remote-p path)
                     (file-remote-p directory))
           (let ((candidate (expand-file-name path directory)))
             (and (not (file-remote-p candidate))
                  (file-readable-p candidate)
                  (file-regular-p candidate)
                  candidate))))))

(defun md-link--reset-count ()
  "Start counting references again for a fresh render."
  (setq md-link--count 0))

(defun md-link-setup ()
  "Turn on reference and document links in the current rendered buffer."
  (setq-local md-render-link-map md-link-map)
  (add-hook 'md-render-code-span-functions #'md-link--linkify-span nil t)
  (add-hook 'md-render-before-hook #'md-link--reset-count nil t))

(provide 'md-link)
;;; md-link.el ends here
