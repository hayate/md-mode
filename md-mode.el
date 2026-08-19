;;; md-mode.el --- Read Markdown files as rendered documents  -*- lexical-binding: t; -*-

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
(require 'md-outline)
(require 'md-link)

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

(defvar-local md--generation 0
  "Bumped by every render.
A render erases the buffer, so any position captured before one refers
to text that no longer exists.  Deferred work carries the generation it
was scheduled under and is dropped if the document has been rebuilt
since.")

(defvar md--rendering nil
  "Bound while rendering, so that layout changes cannot re-enter.")

(defvar-local md--sync-peer nil
  "The buffer this one is kept in step with.")

(defvar-local md--sync-anchor nil
  "Source line both sides were last agreed to be on.")

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

(defun md--capture-state ()
  "Capture what has to survive a re-render.
Everything here is anchored to a source line or a heading name rather
than to a buffer position, because the positions are about to be
destroyed."
  (list :line (and md--rendered-width (md-render-source-line (point)))
        :windows (mapcar (lambda (window)
                           (cons window (md-render-source-line
                                         (window-start window))))
                         (get-buffer-window-list (current-buffer) nil t))
        :folds (md-outline-folded-headings)))

(defun md--restore-state (state)
  "Put back what `md--capture-state\' recorded in STATE."
  (md-outline-invalidate)
  (md-outline-refold (plist-get state :folds))
  (when-let ((line (plist-get state :line)))
    (goto-char (md-render-position-for-line line))
    ;; Point may have landed inside a section that is folded again.
    (when (get-char-property (point) 'invisible)
      (ignore-errors (outline-back-to-heading))))
  (pcase-dolist (`(,window . ,line) (plist-get state :windows))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (set-window-start window (md-render-position-for-line line) t)
      (set-window-point window (point)))))

(defun md--render-into (view source &optional width)
  "Render SOURCE into VIEW at WIDTH.
A render erases and rebuilds the buffer, so this is a transaction:
state anchored to positions is captured first, the document is rebuilt,
and the state is resolved against the new text."
  (let ((md--rendering t)
        (dom (md--dom source))
        (directory (with-current-buffer source
                     (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory))))
    (with-current-buffer view
      (let ((inhibit-read-only t)
            (state (md--capture-state)))
        ;; Before rendering, not after: the margin narrows the text area,
        ;; and the render measures that area to decide where to fill.
        (md--tidy-display)
        (md-render-dom dom directory width)
        (setq md--generation (1+ md--generation))
        (setq md--rendered-width (or width (md-render-width)))
        (set-buffer-modified-p nil)
        (md--restore-state state)))))

(defun md--refresh (&rest _)
  "Re-render the view of the current source buffer, if it has one."
  (when (buffer-live-p md--view-buffer)
    (md--render-into md--view-buffer (current-buffer))))

;;; The view

(defvar-keymap md-view-mode-map
  :doc "Keymap for `md-view-mode'."
  "q"         #'md-mode
  "TAB"       #'md-outline-tab
  "<tab>"     #'md-outline-tab
  "S-TAB"     #'outline-cycle-buffer
  "<backtab>" #'outline-cycle-buffer
  "n"         #'shr-next-link
  "p"         #'shr-previous-link
  "i"         #'imenu
  "t"         #'md-outline-toc
  "l"         #'md-link-back
  "r"         #'md-link-forward
  "^"         #'md-link-up
  "s"         #'md-split)

(define-derived-mode md-view-mode special-mode "MD-View"
  "Major mode for a rendered Markdown document.

\\{md-view-mode-map}"
  (setq-local revert-buffer-function #'md--revert)
  (setq-local cursor-type 'bar)
  (buffer-face-set 'variable-pitch)
  (md-outline-setup)
  (md-link-setup)
  (add-hook 'kill-buffer-hook #'md--view-killed nil t))

(defun md--revert (&rest _)
  "Re-render this view from its source."
  (if (buffer-live-p md--source-buffer)
      (md--render-into (current-buffer) md--source-buffer)
    (message "md-mode: the source buffer is gone")))

(defun md--view-killed ()
  "Drop the source buffer's pointer to this view."
  (md--sync-disable md--sync-peer)
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


;;; Walking between documents
;;
;; Each document keeps its own view, which leaves the one-source-one-view
;; ownership in place: a view is never repointed at a different source, so
;; saving or killing one document cannot disturb another's view.  What moves
;; between them is the history.

(defvar-local md--history nil
  "Places visited before this one, as (FILE . SOURCE-LINE).")

(defvar-local md--forward nil
  "Places returned from, so that going back can be undone.")

(defun md--current-place ()
  "Where we are, as (FILE . SOURCE-LINE), or nil outside a file."
  (let ((source (if (derived-mode-p 'md-view-mode) md--source-buffer (current-buffer))))
    (when (and (buffer-live-p source) (buffer-file-name source))
      (cons (buffer-file-name source)
            (if (derived-mode-p 'md-view-mode)
                (md-render-source-line (point))
              (line-number-at-pos))))))

(defun md--open-document (file &optional line)
  "Open FILE rendered, at LINE.  Leaves the view current.
The switch must not happen inside `with-current-buffer\': that form
restores the previous buffer on exit, so the window would show the new
document while point, and any buffer-local state we then set, belonged
to the old one."
  (let ((source (find-file-noselect file)))
    (with-current-buffer source
      (when line
        (goto-char (point-min))
        (forward-line (1- line))))
    (switch-to-buffer source)
    (when md-link-follow-markdown
      (md--show-render))))

(defun md-link-visit-document (file &optional line)
  "Open FILE as a rendered document, remembering where we came from."
  (let ((origin (md--current-place)))
    (md--open-document file line)
    (when (and origin (derived-mode-p 'md-view-mode))
      (setq md--history (cons origin md--history))
      (setq md--forward nil))))

(defun md--step (from to)
  "Move to the head of FROM, pushing where we are onto TO.
FROM and TO name the buffer-local variables holding the two stacks."
  (let ((stack (symbol-value from)))
    (if (null stack)
        (message "md-mode: nothing to go %s to"
                 (if (eq from 'md--history) "back" "forward"))
      (let ((here (md--current-place))
            (target (car stack))
            (rest (cdr stack))
            (other (symbol-value to)))
        (md--open-document (car target) (cdr target))
        (when (derived-mode-p 'md-view-mode)
          (set from rest)
          (set to (if here (cons here other) other)))))))

(defun md-link-back ()
  "Return to the document visited before this one."
  (interactive)
  (md--step 'md--history 'md--forward))

(defun md-link-forward ()
  "Undo a `md-link-back'."
  (interactive)
  (md--step 'md--forward 'md--history))

(defun md-link-up ()
  "Open the directory the document lives in."
  (interactive)
  (let ((source (if (derived-mode-p 'md-view-mode) md--source-buffer (current-buffer))))
    (if (and (buffer-live-p source) (buffer-file-name source))
        (dired (file-name-directory (buffer-file-name source)))
      (message "md-mode: this document is not visiting a file"))))

;;; Side by side
;;
;; The two buffers are kept in step by source line, not by buffer position.
;; That matters: rendering is lossy -- a whole paragraph maps to its first
;; line -- so round-tripping a position would let point crawl backwards a
;; little on every command.  An anchor only moves when the user actually
;; moves it, and both sides are set to the same anchor, so there is nothing
;; to drift.

(defun md--sync-anchor-here ()
  "The source line point is on, whichever side of the split this is."
  (if (derived-mode-p 'md-view-mode)
      (md-render-source-line (point))
    (line-number-at-pos)))

(defun md--sync-position-for (line)
  "Where LINE is in this buffer."
  (if (derived-mode-p 'md-view-mode)
      (md-render-position-for-line line)
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (point))))

(defun md--sync-post-command ()
  "Move the other side of the split to wherever this side now is."
  (when (and md--sync-peer (buffer-live-p md--sync-peer) (not md--rendering))
    (let ((anchor (md--sync-anchor-here))
          (peer md--sync-peer))
      (unless (eql anchor md--sync-anchor)
        (setq md--sync-anchor anchor)
        (with-current-buffer peer
          (setq md--sync-anchor anchor)
          (let ((position (md--sync-position-for anchor)))
            ;; Point only.  Forcing `window-start' as well fights redisplay's
            ;; own scrolling and makes the peer window jitter.
            ;;
            ;; Both the buffer point and every window point are moved.  The
            ;; peer is never the selected window here, so its buffer point
            ;; would otherwise stay where it was, and a later re-render --
            ;; which reads `point' to decide where to land -- would undo the
            ;; scrolling the user just did.
            (goto-char position)
            (dolist (window (get-buffer-window-list peer nil t))
              (set-window-point window position))))))))

(defun md--sync-enable (buffer peer)
  "Keep BUFFER in step with PEER."
  (with-current-buffer buffer
    (setq md--sync-peer peer)
    (setq md--sync-anchor nil)
    (add-hook 'post-command-hook #'md--sync-post-command nil t)))

(defun md--sync-disable (buffer)
  "Stop keeping BUFFER in step with anything."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq md--sync-peer nil)
      (remove-hook 'post-command-hook #'md--sync-post-command t))))

;;;###autoload
(defun md-split ()
  "Show the Markdown source and the rendered document side by side.
Moving in one window moves the other.  Saving re-renders.  Call it
again to put the windows back."
  (interactive)
  (let ((source (if (derived-mode-p 'md-view-mode) md--source-buffer (current-buffer))))
    (unless (buffer-live-p source)
      (user-error "md-mode: no Markdown source here"))
    (if (buffer-local-value 'md--sync-peer source)
        (let ((view (buffer-local-value 'md--sync-peer source)))
          (md--sync-disable source)
          (md--sync-disable view)
          (switch-to-buffer source)
          (delete-other-windows)
          (message "md-mode: split closed"))
      ;; Not inside `with-current-buffer': it restores the old buffer on
      ;; exit, so `view' would end up bound to the source.
      (switch-to-buffer source)
      (md--show-render)
      (let ((view (current-buffer)))
        (delete-other-windows)
        (switch-to-buffer source)
        (set-window-buffer (split-window-right) view)
        (md--sync-enable source view)
        (md--sync-enable view source)
        (message "md-mode: source and document in step; s to close")))))

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
