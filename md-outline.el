;;; md-outline.el --- Outline and imenu for rendered Markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea@byteset.com>
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

;; Navigation around a long rendered document: fold sections, jump between
;; headings with imenu, and pick one from a table of contents.
;;
;; A rendered buffer holds no `#' markup, so outline cannot find headings by
;; regexp.  It does not need to: shr already puts an `outline-level' text
;; property on every heading it renders, and Emacs 29 added
;; `outline-search-function' along with `outline-search-level', which searches
;; exactly that property.  All this file has to supply is a matching
;; `outline-level' function -- the default measures the length of the regexp
;; match, which with a property search is the heading's title, so a heading
;; called "Decision" would come out as level 8.
;;
;; Folds are overlays, and they evaporate when `md-render' erases the buffer.
;; Sections are therefore remembered by their heading text across a re-render
;; rather than by position.

;;; Code:

(require 'outline)
(require 'imenu)
(require 'md-render)

(defcustom md-outline-fold-on-render t
  "Whether folded sections stay folded when the document is re-rendered.
A re-render erases the buffer, so folds have to be recreated by heading
text.  Nil re-renders the document fully unfolded."
  :type 'boolean
  :group 'md)

;;; Finding headings

(defun md-outline--level-at (position)
  "Heading level at POSITION, or nil if there is no heading there."
  (get-text-property position 'outline-level))

(defun md-outline-level ()
  "Level of the heading at point.
The variable `outline-level' defaults to measuring the width of the
regexp match, which is meaningless when headings are found by text
property."
  (or (md-outline--level-at (point)) 1))

(defun md-outline-headings ()
  "Every heading in the buffer, as a list of (LEVEL TEXT POSITION)."
  (save-excursion
    (goto-char (point-min))
    (let ((headings '())
          (position (point-min)))
      (while position
        (when (md-outline--level-at position)
          (push (list (md-outline--level-at position)
                      (string-trim
                       (buffer-substring-no-properties
                        position (save-excursion (goto-char position)
                                                 (line-end-position))))
                      position)
                headings))
        (setq position (next-single-property-change position 'outline-level)))
      (nreverse headings))))

;;; Folding across a re-render

(defun md-outline-folded-headings ()
  "Text of every heading whose section is currently folded."
  (when (bound-and-true-p outline-minor-mode)
    (let ((folded '()))
      (pcase-dolist (`(,_level ,text ,position) (md-outline-headings))
        (when (save-excursion
                (goto-char position)
                (end-of-line)
                (and (not (eobp))
                     (eq (get-char-property (point) 'invisible) 'outline)))
          (push text folded)))
      (nreverse folded))))

(defun md-outline-refold (headings)
  "Fold the sections named by HEADINGS again after a re-render."
  (when (and md-outline-fold-on-render headings
             (bound-and-true-p outline-minor-mode))
    (save-excursion
      (pcase-dolist (`(,_level ,text ,position) (md-outline-headings))
        (when (member text headings)
          (goto-char position)
          (ignore-errors (outline-hide-subtree)))))))

;;; Imenu

(defun md-outline--nest (headings)
  "Turn the flat HEADINGS list into a nested imenu index."
  (let ((index '()))
    (while headings
      (pcase-let* ((`(,level ,text ,position) (car headings))
                   (rest (cdr headings))
                   (children '()))
        (while (and rest (> (nth 0 (car rest)) level))
          (push (car rest) children)
          (setq rest (cdr rest)))
        (setq children (nreverse children))
        (push (if children
                  (cons text (cons (cons (concat text " .") position)
                                   (md-outline--nest children)))
                (cons text position))
              index)
        (setq headings rest)))
    (nreverse index)))

(defun md-outline-imenu-index ()
  "Build an imenu index from the headings of the rendered document."
  (md-outline--nest (md-outline-headings)))

;;; Commands

(defun md-outline-toc ()
  "Jump to a heading chosen from the document's table of contents."
  (interactive)
  (let* ((headings (md-outline-headings))
         (candidates
          (mapcar (lambda (heading)
                    (pcase-let ((`(,level ,text ,position) heading))
                      (cons (concat (make-string (* 2 (1- level)) ?\s) text)
                            position)))
                  headings)))
    (if (null candidates)
        (message "md-mode: this document has no headings")
      (let ((choice (completing-read "Heading: " candidates nil t)))
        (when-let ((position (cdr (assoc choice candidates))))
          (goto-char position)
          (recenter 0))))))

(defun md-outline-tab ()
  "Fold or unfold the section at point, or move to the next link."
  (interactive)
  (if (and (bound-and-true-p outline-minor-mode)
           (md-outline--level-at (point)))
      (outline-cycle)
    (shr-next-link)))

(defun md-outline--search (&optional bound move backward looking-at)
  "Find the next heading, by text property rather than by regexp.
Stand in for `outline-search-level\' where that is not available.  BOUND,
MOVE, BACKWARD and LOOKING-AT are as `outline-search-function\'
documents them."
  (if looking-at
      (and (md-outline--level-at (point))
           (or (bobp) (not (md-outline--level-at (1- (point))))))
    (let ((found nil)
          (position (point)))
      (while (and (not found)
                  (setq position (if backward
                                     (previous-single-property-change
                                      position 'outline-level)
                                   (next-single-property-change
                                    position 'outline-level))))
        (when (and (md-outline--level-at position)
                   (or (null bound)
                       (if backward (>= position bound) (<= position bound))))
          (setq found position)))
      (cond
       (found
        (goto-char found)
        (set-match-data (list found (line-end-position)))
        t)
       (move
        (goto-char (or bound (if backward (point-min) (point-max))))
        nil)
       (t nil)))))

(defun md-outline-setup ()
  "Turn on outline and imenu for the current rendered document."
  ;; `outline-search-level' searches the very property shr writes, but it is
  ;; not in every Emacs this package supports.
  (setq-local outline-search-function
              (if (fboundp 'outline-search-level)
                  #'outline-search-level
                #'md-outline--search))
  (setq-local outline-level #'md-outline-level)
  (setq-local imenu-create-index-function #'md-outline-imenu-index)
  (outline-minor-mode 1))

(defun md-outline-invalidate ()
  "Drop caches that point into the buffer a re-render has just replaced."
  (setq imenu--index-alist nil))

(provide 'md-outline)
;;; md-outline.el ends here
