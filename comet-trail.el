;;; comet-trail.el --- Cursor comet trail effect  -*- lexical-binding: t -*-

;; Author: Andros Fenollosa <hi@andros.dev>
;; Maintainer: Andros Fenollosa <hi@andros.dev>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces, convenience
;; URL: https://git.andros.dev/andros/comet-trail.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a comet trail effect for the cursor.  When
;; the cursor moves, a short animated comet travels along the
;; geometric line between the old and new positions.
;;
;; It highlights the background of characters that fall along the
;; path, correctly handling visual line wrapping
;; (`visual-line-mode', word-wrap, `toggle-truncate-lines').
;;
;; The core idea: given two buffer positions, compute their visual
;; (column, row) coordinates on screen, trace a line between them
;; using Bresenham's algorithm, then map each cell back to a buffer
;; position and animate a sliding comet with ease-out brightness.
;;
;; Usage:
;;
;;   (require 'comet-trail)
;;   (add-hook 'prog-mode-hook #'comet-trail-mode)

;;; Code:

(defgroup comet-trail nil
  "Highlight characters along a geometric line."
  :group 'convenience
  :prefix "comet-trail-")

(defface comet-trail-highlight
  '((((background dark))  :background "#f0c674")
    (((background light)) :background "#c678dd"))
  "Face used to highlight characters on the line path."
  :group 'comet-trail)

(defcustom comet-trail-face 'comet-trail-highlight
  "Face used for highlighting characters on the line.
Set to nil to inherit the cursor color automatically."
  :type '(choice face (const nil))
  :group 'comet-trail)

(defcustom comet-trail-length 4
  "Maximum number of characters visible in the comet trail."
  :type 'integer
  :group 'comet-trail)

(defcustom comet-trail-speed 0.3
  "Duration in seconds for the comet to traverse the full path."
  :type 'number
  :group 'comet-trail)

(defcustom comet-trail-tick-interval 0.016
  "Interval in seconds between animation frames.
Default is approximately 60fps."
  :type 'number
  :group 'comet-trail)

(defcustom comet-trail-fade-exponent 2.0
  "Exponent controlling the brightness falloff along the comet tail.
1.0 is linear, 2.0 is quadratic ease-out (recommended),
3.0 is cubic (sharper tail).  Higher values concentrate
brightness near the head."
  :type 'number
  :group 'comet-trail)

(defcustom comet-trail-minimum-distance 2
  "Minimum path length in cells to trigger a comet animation.
Movements shorter than this are ignored to reduce visual noise."
  :type 'integer
  :group 'comet-trail)

(defvar-local comet-trail--overlays nil
  "List of active overlays used by the current animation frame.")

(defvar-local comet-trail--overlay-pool nil
  "Pool of reusable overlays to avoid allocation per frame.")

(defvar-local comet-trail--animations nil
  "List of active comet animations, each a vector [PATH START-TIME PALETTE].")

(defvar-local comet-trail--anim-timer nil
  "Timer for comet animations.")

;;; --- Visual coordinate helpers ---

(defun comet-trail--visual-coords (pos &optional window)
  "Return (VCOL . VROW) visual coordinates for buffer position POS.
VCOL is the visual column, VROW is the visual row number in WINDOW.
Returns nil if POS is not visible."
  (setq window (or window (selected-window)))
  (let ((info (pos-visible-in-window-p pos window t)))
    (when info
      (let* ((x (nth 0 info))
             (y (nth 1 info))
             (fw (window-font-width window))
             (fh (window-font-height window))
             (vcol (if (> fw 0) (round (/ (float x) fw)) 0))
             (vrow (if (> fh 0) (round (/ (float y) fh)) 0)))
        (cons vcol vrow)))))

(defun comet-trail--pos-at-visual-coords (vcol vrow &optional window)
  "Return buffer position at visual column VCOL and row VROW in WINDOW.
Returns nil if the position does not match the requested cell,
for example when VCOL is past the end of a visual line."
  (setq window (or window (selected-window)))
  (let* ((fw (window-font-width window))
         (fh (window-font-height window))
         (px (+ (* vcol fw) (/ fw 2)))
         (py (+ (* vrow fh) (/ fh 2)))
         (posn (posn-at-x-y px py window))
         (pos (when posn (posn-point posn))))
    (when pos
      ;; Verify the position maps back to the expected visual cell.
      ;; posn-at-x-y clamps to the nearest valid character, so if we
      ;; asked for a column past the line end, it returns the newline
      ;; or last char.  Reject those by round-tripping.
      (let ((actual (comet-trail--visual-coords pos window)))
        (when (and actual
                   (= (car actual) vcol)
                   (= (cdr actual) vrow))
          pos)))))

;;; --- Bresenham line algorithm ---

(defun comet-trail--bresenham (x0 y0 x1 y1)
  "Compute list of (X . Y) cells along a line from (X0,Y0) to (X1,Y1).
Uses Bresenham's line algorithm."
  (let ((points nil)
        (dx (abs (- x1 x0)))
        (dy (- (abs (- y1 y0))))
        (sx (if (< x0 x1) 1 -1))
        (sy (if (< y0 y1) 1 -1))
        (err 0)
        (e2 0))
    (setq err (+ dx dy))
    (let ((x x0) (y y0))
      (catch 'done
        (while t
          (push (cons x y) points)
          (when (and (= x x1) (= y y1))
            (throw 'done nil))
          (setq e2 (* 2 err))
          (when (>= e2 dy)
            (setq err (+ err dy))
            (setq x (+ x sx)))
          (when (<= e2 dx)
            (setq err (+ err dx))
            (setq y (+ y sy))))))
    (nreverse points)))

;;; --- Core API ---

;;;###autoload
(defun comet-trail-clear ()
  "Remove all comet overlays from the current buffer."
  (interactive)
  (comet-trail--pool-clear))

(defun comet-trail--compute-path (pos1 pos2 &optional window)
  "Compute a vector of buffer positions along a line from POS1 to POS2.
WINDOW defaults to the selected window.
Returns nil if either position is not visible."
  (setq window (or window (selected-window)))
  (let ((coords1 (comet-trail--visual-coords pos1 window))
        (coords2 (comet-trail--visual-coords pos2 window)))
    (when (and coords1 coords2)
      (let* ((cells (comet-trail--bresenham
                     (car coords1) (cdr coords1)
                     (car coords2) (cdr coords2)))
             (positions nil))
        (dolist (cell cells)
          (let ((buf-pos (comet-trail--pos-at-visual-coords
                          (car cell) (cdr cell) window)))
            (when (and buf-pos
                       (>= buf-pos (point-min))
                       (<= buf-pos (point-max)))
              (push buf-pos positions))))
        (vconcat (nreverse positions))))))

;;;###autoload
(defun comet-trail-draw (pos1 pos2 &optional face window)
  "Draw a static highlighted line between buffer positions POS1 and POS2.
FACE defaults to `comet-trail-face'.  WINDOW defaults to selected window.
Returns the list of overlays created, or nil if positions are not visible."
  (interactive "r")
  (setq face (or face comet-trail-face))
  (let ((path (comet-trail--compute-path pos1 pos2 (or window (selected-window)))))
    (unless path
      (user-error "Both positions must be visible in the window"))
    (let ((new-overlays nil))
      (dotimes (i (length path))
        (let ((ov (make-overlay (aref path i) (1+ (aref path i)))))
          (overlay-put ov 'face face)
          (overlay-put ov 'comet-trail t)
          (overlay-put ov 'priority 100)
          (push ov new-overlays)))
      (setq comet-trail--overlays
            (append (nreverse new-overlays) comet-trail--overlays))
      new-overlays)))

;;; --- Color helpers ---

(defun comet-trail--color-components (color)
  "Return (R G B) as floats 0.0-1.0 for COLOR name or hex string."
  (let ((rgb (color-values color)))
    (when rgb
      (list (/ (float (nth 0 rgb)) 65535.0)
            (/ (float (nth 1 rgb)) 65535.0)
            (/ (float (nth 2 rgb)) 65535.0)))))

(defun comet-trail--lerp-color (from to progress)
  "Interpolate between colors FROM and TO by PROGRESS (0.0 to 1.0).
Returns a hex color string."
  (let ((fc (comet-trail--color-components from))
        (tc (comet-trail--color-components to)))
    (when (and fc tc)
      (let ((r (+ (nth 0 fc) (* progress (- (nth 0 tc) (nth 0 fc)))))
            (g (+ (nth 1 fc) (* progress (- (nth 1 tc) (nth 1 fc)))))
            (b (+ (nth 2 fc) (* progress (- (nth 2 tc) (nth 2 fc))))))
        (format "#%02x%02x%02x"
                (round (* 255 r))
                (round (* 255 g))
                (round (* 255 b)))))))

(defun comet-trail--bg-color ()
  "Return the current buffer background color as a string."
  (or (face-background 'default nil t) "#000000"))

(defun comet-trail--highlight-color ()
  "Return the highlight color from `comet-trail-face' or the cursor."
  (or (and comet-trail-face (face-background comet-trail-face nil t))
      (face-background 'cursor nil t)
      "#f0c674"))

;;; --- Overlay pool ---

(defun comet-trail--pool-get (pos)
  "Get an overlay at POS, reusing from pool or creating a new one."
  (let ((ov (pop comet-trail--overlay-pool)))
    (if ov
        (progn (move-overlay ov pos (1+ pos)) ov)
      (let ((new-ov (make-overlay pos (1+ pos))))
        (overlay-put new-ov 'comet-trail t)
        (overlay-put new-ov 'priority 100)
        new-ov))))

(defun comet-trail--pool-release (ov)
  "Return OV to the overlay pool for reuse."
  (overlay-put ov 'face nil)
  (move-overlay ov 1 1)
  (push ov comet-trail--overlay-pool))

(defun comet-trail--pool-clear ()
  "Delete all overlays in both active list and pool."
  (dolist (ov comet-trail--overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq comet-trail--overlays nil)
  (dolist (ov comet-trail--overlay-pool)
    (when (overlayp ov) (delete-overlay ov)))
  (setq comet-trail--overlay-pool nil))

;;; --- Pre-computed color palette ---

(defun comet-trail--compute-palette ()
  "Pre-compute a vector of colors for the comet gradient.
Index 0 is the head (brightest), last index is the tail (dimmest)."
  (let* ((bg (comet-trail--bg-color))
         (hi (comet-trail--highlight-color))
         (len comet-trail-length)
         (colors (make-vector len nil)))
    (dotimes (i len)
      (let* ((linear (if (> len 1)
                         (- 1.0 (/ (float i) (1- len)))
                       1.0))
             (brightness (expt linear comet-trail-fade-exponent)))
        (aset colors i (comet-trail--lerp-color bg hi brightness))))
    colors))

;;; --- Comet animation engine ---

(defun comet-trail--tick ()
  "Advance all active comet animations by one frame."
  (let ((inhibit-redisplay t))
    ;; Return current overlays to pool.
    (dolist (ov comet-trail--overlays)
      (comet-trail--pool-release ov))
    (setq comet-trail--overlays nil)
    (let ((now (float-time))
          (comet-len comet-trail-length)
          (alive nil))
      (dolist (anim comet-trail--animations)
        (let* ((path (aref anim 0))
               (start-time (aref anim 1))
               (palette (aref anim 2))
               (path-len (length path))
               (elapsed (- now start-time))
               (speed (/ (float path-len) comet-trail-speed))
               (head (floor (* elapsed speed)))
               (tail (- head comet-len)))
          (if (>= tail path-len)
              nil
            (push anim alive)
            (let ((vis-start (max 0 tail))
                  (vis-end (min head path-len)))
              (when (> vis-end vis-start)
                (dotimes (i (- vis-end vis-start))
                  (let* ((idx (+ vis-start i))
                         (buf-pos (aref path idx))
                         (dist (- head idx 1))
                         (color-idx (min dist (1- comet-len)))
                         (col (aref palette color-idx))
                         (ov (comet-trail--pool-get buf-pos)))
                    (overlay-put ov 'face `(:background ,col))
                    (push ov comet-trail--overlays))))))))
      (setq comet-trail--animations (nreverse alive))
      (when (null comet-trail--animations)
        (comet-trail--anim-stop)))))

(defun comet-trail--anim-start ()
  "Start the animation timer if not already running."
  (unless comet-trail--anim-timer
    (let ((buf (current-buffer)))
      (setq comet-trail--anim-timer
            (run-at-time nil comet-trail-tick-interval
                         (lambda ()
                           (if (buffer-live-p buf)
                               (with-current-buffer buf
                                 (comet-trail--tick))
                             (comet-trail--anim-stop))))))))

(defun comet-trail--anim-stop ()
  "Stop the animation timer."
  (when comet-trail--anim-timer
    (cancel-timer comet-trail--anim-timer)
    (setq comet-trail--anim-timer nil)))

;;; --- Trail minor mode ---

(defvar-local comet-trail--last-pos nil
  "Last cursor position tracked by `comet-trail-mode'.")

(defvar comet-trail--ignored-commands
  '(self-insert-command
    delete-char delete-backward-char
    backward-delete-char-untabify
    newline newline-and-indent open-line
    yank yank-pop
    kill-region kill-line kill-word backward-kill-word
    undo undo-redo)
  "Commands that should not trigger a comet animation.")

(defun comet-trail--hook ()
  "Launch a comet from previous to current cursor position."
  (let ((pos (point)))
    (when (and comet-trail--last-pos
               (/= pos comet-trail--last-pos)
               (not (memq this-command comet-trail--ignored-commands))
               (pos-visible-in-window-p comet-trail--last-pos)
               (pos-visible-in-window-p pos))
      (let ((path (comet-trail--compute-path comet-trail--last-pos pos)))
        (when (and path (>= (length path) comet-trail-minimum-distance))
          (push (vector path (float-time) (comet-trail--compute-palette))
                comet-trail--animations)
          (comet-trail--anim-start))))
    (setq comet-trail--last-pos pos)))

;;;###autoload
(define-minor-mode comet-trail-mode
  "Toggle cursor trail mode.
When enabled, moving the cursor launches a comet animation along
the line between the previous and new positions.
Works with both keyboard and mouse."
  :lighter " Trail"
  :group 'comet-trail
  (if comet-trail-mode
      (progn
        (setq comet-trail--last-pos (point))
        (add-hook 'post-command-hook #'comet-trail--hook nil t))
    (remove-hook 'post-command-hook #'comet-trail--hook t)
    (comet-trail--anim-stop)
    (comet-trail--pool-clear)
    (setq comet-trail--animations nil)
    (setq comet-trail--last-pos nil)))

(provide 'comet-trail)

;;; comet-trail.el ends here
