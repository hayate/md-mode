;;; md-mode.el --- Read Markdown files as rendered documents  -*- lexical-binding: t; -*-

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

;; `M-x md-mode' in a Markdown buffer replaces the window with the rendered
;; document: proportional text, scaled headings, drawn tables, quoted passages
;; with a bar down their left, syntax-highlighted code and inline images.
;; `M-x md-mode' again, or `q', returns to the source.
;;
;; The rendered view is a separate read-only buffer, so the file on disk is
;; never touched by rendering and cannot be saved in rendered form.  Point is
;; carried across the toggle in both directions.
;;
;; Note on naming: `md-mode' is a command rather than a major mode, because it
;; has to work from both sides of the toggle.  The major mode of the rendered
;; buffer is `md-view-mode'.

;;; Code:

(require 'md-parse)
(require 'md-render)

(defcustom md-auto-rerender-max-size 200000
  "Documents larger than this many characters are not re-rendered automatically.
They still re-render on demand with \\[revert-buffer]."
  :type 'integer
  :group 'md)

(defcustom md-rerender-delay 0.3
  "Seconds to wait after a window resize before re-rendering."
  :type 'number
  :group 'md)

(defcustom md-view-margin 2
  "Columns of blank left margin in a rendered document.
A page has a margin; a buffer flush against the frame edge reads like a
terminal.  The margin is outside the text area, so `md-render-width\'
accounts for it without any arithmetic here."
  :type 'integer
  :group 'md)

(defcustom md-view-hide-line-numbers t
  "Whether to hide line numbers in a rendered document.
A rendered buffer's line numbers count rendered lines, which correspond
to nothing the reader can use: they are not the source's line numbers,
and a document does not want a gutter.  Set this to nil to leave
whatever `display-line-numbers-mode' does elsewhere alone."
  :type 'boolean
  :group 'md)

(defvar-local md--view-buffer nil
  "In a source buffer, its rendered view.")

(defvar-local md--source-buffer nil
  "In a rendered buffer, the source it was rendered from.")

(defvar-local md--rendered-width nil
  "Width, in characters, this view was last rendered at.")

(defvar-local md--dom-cache nil
  "Cons of `buffer-chars-modified-tick' and the DOM parsed at that tick.")

(defvar-local md--rerender-timer nil
  "This view's pending re-render, if any.")

(defvar md--rendering nil
  "Bound while rendering, so that layout changes cannot re-enter.")

;; `revert-buffer' resets the major mode, which clears ordinary buffer-local
;; variables and buffer-local hooks.  Without these the source would forget its
;; view on every revert and render a second one beside the first.
(put 'md--view-buffer 'permanent-local t)
(put 'md--source-buffer 'permanent-local t)
(put 'md--rendered-width 'permanent-local t)
(put 'md--rerender-timer 'permanent-local t)
(put 'md--refresh 'permanent-local-hook t)
(put 'md--source-killed 'permanent-local-hook t)

;;; Rendering

(defun md--dom (source)
  "Parse SOURCE, reusing the cached DOM if it has not been edited."
  (with-current-buffer source
    (let ((tick (buffer-chars-modified-tick)))
      (if (eql (car md--dom-cache) tick)
          (cdr md--dom-cache)
        (cdr (setq md--dom-cache (cons tick (md-parse-buffer source))))))))

(defun md--view-name (source)
  "A buffer name for the view of SOURCE."
  (generate-new-buffer-name
   (format "*md: %s*" (buffer-name source))))

(defun md--tidy-display ()
  "Turn off editor furniture a rendered document should not carry.
This runs on every render rather than from `md-view-mode\', because a
global minor mode such as `global-display-line-numbers-mode\' turns
itself on from `after-change-major-mode-hook\' -- that is, after the
mode body has already had its say."
  (when md-view-hide-line-numbers
    (setq-local display-line-numbers nil))
  (setq-local truncate-lines nil)
  ;; The buffer-local width covers every later display of this buffer;
  ;; the windows showing it right now need telling directly.
  (setq-local left-margin-width md-view-margin)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-margins window md-view-margin)))

(defun md--render-into (view source &optional width)
  "Render SOURCE into VIEW at WIDTH."
  (let ((md--rendering t)
        (dom (md--dom source))
        (directory (with-current-buffer source
                     (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory))))
    (with-current-buffer view
      (let ((inhibit-read-only t)
            (line (and (bound-and-true-p md--rendered-width)
                       (md-render-source-line (point)))))
        ;; Before rendering, not after: the margin narrows the text area,
        ;; and the render measures that area to decide where to fill.
        (md--tidy-display)
        (md-render-dom dom directory width)
        (setq md--rendered-width (or width (md-render-width)))
        (set-buffer-modified-p nil)
        (when line
          (goto-char (md-render-position-for-line line)))))))

(defun md--refresh (&rest _)
  "Re-render the view of the current source buffer, if it has one."
  (when (buffer-live-p md--view-buffer)
    (md--render-into md--view-buffer (current-buffer))))

;;; The view

(defvar-keymap md-view-mode-map
  :doc "Keymap for `md-view-mode'."
  "q"       #'md-mode
  "TAB"     #'shr-next-link
  "<tab>"   #'shr-next-link
  "S-TAB"   #'shr-previous-link
  "<backtab>" #'shr-previous-link)

(define-derived-mode md-view-mode special-mode "MD-View"
  "Major mode for a rendered Markdown document.

\\{md-view-mode-map}"
  (setq-local revert-buffer-function #'md--revert)
  (setq-local cursor-type 'bar)
  (buffer-face-set 'variable-pitch)
  (add-hook 'kill-buffer-hook #'md--view-killed nil t))

(defun md--revert (&rest _)
  "Re-render this view from its source."
  (if (buffer-live-p md--source-buffer)
      (md--render-into (current-buffer) md--source-buffer)
    (message "md-mode: the source buffer is gone")))

(defun md--view-killed ()
  "Drop the source buffer's pointer to this view."
  (when md--rerender-timer
    (cancel-timer md--rerender-timer)
    (setq md--rerender-timer nil))
  (when (buffer-live-p md--source-buffer)
    (with-current-buffer md--source-buffer
      (setq md--view-buffer nil)
      (remove-hook 'after-save-hook #'md--refresh t)
      (remove-hook 'after-revert-hook #'md--refresh t)
      (remove-hook 'kill-buffer-hook #'md--source-killed t))))

(defun md--source-killed ()
  "Kill the view when its source goes away, rather than leave it stale."
  (when (buffer-live-p md--view-buffer)
    (let ((view md--view-buffer))
      (with-current-buffer view (setq md--source-buffer nil))
      (kill-buffer view))))

;;; Resizing
;;
;; A rendered buffer is laid out for one width, so it cannot serve two windows
;; of different widths at once.  When that happens the view is left alone and
;; the user can re-render with `g'.

(defun md--maybe-rerender (view window)
  "Re-render VIEW for WINDOW if its width changed and nothing forbids it."
  (let ((source (buffer-local-value 'md--source-buffer view)))
    (when (and (not md--rendering)
               (buffer-live-p source)
               (= 1 (length (get-buffer-window-list view nil t)))
               (< (buffer-size source) md-auto-rerender-max-size))
      (let ((width (with-selected-window window (md-render-width))))
        (unless (eql width (buffer-local-value 'md--rendered-width view))
          ;; The timer is per view: one shared timer would let a second view
          ;; cancel the first one's pending render and drop it.
          (with-current-buffer view
            (when md--rerender-timer (cancel-timer md--rerender-timer))
            (setq md--rerender-timer
                  (run-with-idle-timer
                   md-rerender-delay nil
                   (lambda ()
                     (when (buffer-live-p view)
                       (with-current-buffer view (setq md--rerender-timer nil)))
                     (when (and (buffer-live-p view) (buffer-live-p source)
                                (get-buffer-window view t))
                       ;; Recheck the width: the window may have moved again.
                       (let ((now (if (get-buffer-window view t)
                                      (with-selected-window (get-buffer-window view t)
                                        (md-render-width))
                                    width)))
                         (md--render-into view source now))))))))))))

(defun md--window-size-changed (frame)
  "Re-render any rendered Markdown views on FRAME whose width changed."
  (dolist (window (window-list frame 'no-minibuf))
    (let ((buffer (window-buffer window)))
      (when (and (buffer-live-p buffer)
                 (buffer-local-value 'md--source-buffer buffer))
        (md--maybe-rerender buffer window)))))

(defun md--watch-resizes ()
  "Start watching for window resizes.
Hooked on first use rather than at load, so that merely requiring the
package costs a global hook that fires on every resize in every frame."
  (add-hook 'window-size-change-functions #'md--window-size-changed))

;;; The command

(defun md--show-source ()
  "Switch from a rendered view back to its source, keeping place."
  (let ((source md--source-buffer)
        (line (md-render-source-line (point))))
    (if (not (buffer-live-p source))
        (message "md-mode: the source buffer is gone")
      (switch-to-buffer source)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun md--show-render ()
  "Render the current buffer and switch to the rendered view."
  (let* ((source (current-buffer))
         (line (line-number-at-pos))
         (view (if (buffer-live-p md--view-buffer)
                   md--view-buffer
                 (get-buffer-create (md--view-name source)))))
    (setq md--view-buffer view)
    (with-current-buffer view
      (unless (derived-mode-p 'md-view-mode) (md-view-mode))
      (setq md--source-buffer source))
    (md--watch-resizes)
    (add-hook 'after-save-hook #'md--refresh nil t)
    (add-hook 'after-revert-hook #'md--refresh nil t)
    (add-hook 'kill-buffer-hook #'md--source-killed nil t)
    (switch-to-buffer view)
    (md--render-into view source)
    (goto-char (md-render-position-for-line line))))

;;;###autoload
(defun md-mode ()
  "Render the Markdown in this buffer, or go back to the source.

In a Markdown buffer, replace the window with the rendered document.
In a rendered document, switch back to the Markdown it came from.
Point is carried across in both directions."
  (interactive)
  (if (derived-mode-p 'md-view-mode)
      (md--show-source)
    (md--show-render)))

(provide 'md-mode)
;;; md-mode.el ends here
