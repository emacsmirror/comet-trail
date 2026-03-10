;;; comet.el --- Cursor comet trail effect  -*- lexical-binding: t -*-

;; Author: Andros
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, convenience
;; URL: https://git.andros.dev/andros/comet.el

;;; Commentary:

;; This package highlights the background of characters that fall along
;; a geometric line drawn between two buffer positions.  It correctly
;; handles visual line wrapping (visual-line-mode, word-wrap,
;; toggle-truncate-lines).
;;
;; The core idea: given two buffer positions, compute their visual
;; (column, row) coordinates on screen, trace a line between them
;; using Bresenham's algorithm, then map each cell back to a buffer
;; position and apply an overlay.

;;; Code:

(defgroup comet nil
  "Highlight characters along a geometric line."
  :group 'convenience
  :prefix "comet-")

(defface comet-highlight
  '((((background dark))  :background "#f0c674")
    (((background light)) :background "#c678dd"))
  "Face used to highlight characters on the line path."
  :group 'comet)

(defcustom comet-face 'comet-highlight
  "Face used for highlighting characters on the line.
Set to nil to inherit the cursor color automatically."
  :type '(choice face (const nil))
  :group 'comet)

(defcustom comet-comet-length 4
  "Maximum number of characters visible in the comet trail."
  :type 'integer
  :group 'comet)

(defcustom comet-comet-speed 0.3
  "Duration in seconds for the comet to traverse the full path."
  :type 'number
  :group 'comet)

(defcustom comet-tick-interval 0.016
  "Interval in seconds between animation frames.
Default is approximately 60fps."
  :type 'number
  :group 'comet)

(defcustom comet-fade-exponent 2.0
  "Exponent controlling the brightness falloff along the comet tail.
1.0 is linear, 2.0 is quadratic ease-out (recommended),
3.0 is cubic (sharper tail).  Higher values concentrate
brightness near the head."
  :type 'number
  :group 'comet)

(defcustom comet-minimum-distance 2
  "Minimum path length in cells to trigger a comet animation.
Movements shorter than this are ignored to reduce visual noise."
  :type 'integer
  :group 'comet)

(defvar-local comet--overlays nil
  "List of overlays created by comet for the current buffer.")

(defvar-local comet--animations nil
  "List of active comet animations, each a vector [PATH START-TIME].")

(defvar-local comet--anim-timer nil
  "Timer for comet animations.")

;;; --- Visual coordinate helpers ---

(defun comet--visual-coords (pos &optional window)
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

(defun comet--pos-at-visual-coords (vcol vrow &optional window)
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
      (let ((actual (comet--visual-coords pos window)))
        (when (and actual
                   (= (car actual) vcol)
                   (= (cdr actual) vrow))
          pos)))))

;;; --- Bresenham line algorithm ---

(defun comet--bresenham (x0 y0 x1 y1)
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

(defun comet-clear ()
  "Remove all comet overlays from the current buffer."
  (interactive)
  (dolist (ov comet--overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq comet--overlays nil))

(defun comet--compute-path (pos1 pos2 &optional window)
  "Compute a vector of buffer positions along a line from POS1 to POS2.
WINDOW defaults to the selected window.
Returns nil if either position is not visible."
  (setq window (or window (selected-window)))
  (let ((coords1 (comet--visual-coords pos1 window))
        (coords2 (comet--visual-coords pos2 window)))
    (when (and coords1 coords2)
      (let* ((cells (comet--bresenham
                     (car coords1) (cdr coords1)
                     (car coords2) (cdr coords2)))
             (positions nil))
        (dolist (cell cells)
          (let ((buf-pos (comet--pos-at-visual-coords
                          (car cell) (cdr cell) window)))
            (when (and buf-pos
                       (>= buf-pos (point-min))
                       (<= buf-pos (point-max)))
              (push buf-pos positions))))
        (vconcat (nreverse positions))))))

(defun comet-draw (pos1 pos2 &optional face window)
  "Draw a static highlighted line between buffer positions POS1 and POS2.
FACE defaults to `comet-face'.  WINDOW defaults to selected window.
Returns the list of overlays created, or nil if positions are not visible."
  (interactive "r")
  (setq face (or face comet-face))
  (let ((path (comet--compute-path pos1 pos2 (or window (selected-window)))))
    (unless path
      (user-error "Both positions must be visible in the window"))
    (let ((new-overlays nil))
      (dotimes (i (length path))
        (let ((ov (make-overlay (aref path i) (1+ (aref path i)))))
          (overlay-put ov 'face face)
          (overlay-put ov 'comet t)
          (overlay-put ov 'priority 100)
          (push ov new-overlays)))
      (setq comet--overlays
            (append (nreverse new-overlays) comet--overlays))
      new-overlays)))

;;; --- Color helpers ---

(defun comet--color-components (color)
  "Return (R G B) as floats 0.0-1.0 for COLOR name or hex string."
  (let ((rgb (color-values color)))
    (when rgb
      (list (/ (float (nth 0 rgb)) 65535.0)
            (/ (float (nth 1 rgb)) 65535.0)
            (/ (float (nth 2 rgb)) 65535.0)))))

(defun comet--lerp-color (from to progress)
  "Interpolate between colors FROM and TO by PROGRESS (0.0 to 1.0).
Returns a hex color string."
  (let ((fc (comet--color-components from))
        (tc (comet--color-components to)))
    (when (and fc tc)
      (format "#%02x%02x%02x"
              (round (* 255 (+ (nth 0 fc) (* progress (- (nth 0 tc) (nth 0 fc))))))
              (round (* 255 (+ (nth 1 fc) (* progress (- (nth 1 tc) (nth 1 fc))))))
              (round (* 255 (+ (nth 2 fc) (* progress (- (nth 2 tc) (nth 2 fc))))))))))

(defun comet--bg-color ()
  "Return the current buffer background color as a string."
  (or (face-background 'default nil t) "#000000"))

(defun comet--highlight-color ()
  "Return the highlight color from `comet-face' or the cursor."
  (or (and comet-face (face-background comet-face nil t))
      (face-background 'cursor nil t)
      "#f0c674"))

;;; --- Comet animation engine ---

(defun comet--comet-tick ()
  "Advance all active comet animations by one frame."
  (dolist (ov comet--overlays)
    (when (overlayp ov) (delete-overlay ov)))
  (setq comet--overlays nil)
  (let ((now (float-time))
        (hi (comet--highlight-color))
        (bg (comet--bg-color))
        (comet-len comet-comet-length)
        (alive nil))
    (dolist (anim comet--animations)
      (let* ((path (aref anim 0))
             (start-time (aref anim 1))
             (path-len (length path))
             (elapsed (- now start-time))
             (speed (/ (float path-len) comet-comet-speed))
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
                       (linear (max 0.0 (- 1.0 (/ (float dist)
                                                   (max 1 (1- comet-len))))))
                       (brightness (expt linear comet-fade-exponent))
                       (col (comet--lerp-color bg hi brightness)))
                  (when col
                    (let ((ov (make-overlay buf-pos (1+ buf-pos))))
                      (overlay-put ov 'face `(:background ,col))
                      (overlay-put ov 'comet t)
                      (overlay-put ov 'priority 100)
                      (push ov comet--overlays))))))))))
    (setq comet--animations (nreverse alive))
    (when (null comet--animations)
      (comet--anim-stop))))

(defun comet--anim-start ()
  "Start the animation timer if not already running."
  (unless comet--anim-timer
    (let ((buf (current-buffer)))
      (setq comet--anim-timer
            (run-at-time nil comet-tick-interval
                         (lambda ()
                           (if (buffer-live-p buf)
                               (with-current-buffer buf
                                 (comet--comet-tick))
                             (comet--anim-stop))))))))

(defun comet--anim-stop ()
  "Stop the animation timer."
  (when comet--anim-timer
    (cancel-timer comet--anim-timer)
    (setq comet--anim-timer nil)))

;;; --- Trail minor mode ---

(defvar-local comet--last-pos nil
  "Last cursor position tracked by `comet-trail-mode'.")

(defun comet--trail-hook ()
  "Hook function that launches a comet from previous to current cursor position."
  (let ((pos (point)))
    (when (and comet--last-pos
               (/= pos comet--last-pos)
               (pos-visible-in-window-p comet--last-pos)
               (pos-visible-in-window-p pos))
      (let ((path (comet--compute-path comet--last-pos pos)))
        (when (and path (>= (length path) comet-minimum-distance))
          (push (vector path (float-time)) comet--animations)
          (comet--anim-start))))
    (setq comet--last-pos pos)))

;;;###autoload
(define-minor-mode comet-trail-mode
  "Toggle cursor trail mode.
When enabled, moving the cursor launches a comet animation along
the line between the previous and new positions.
Works with both keyboard and mouse."
  :lighter " Trail"
  :group 'comet
  (if comet-trail-mode
      (progn
        (setq comet--last-pos (point))
        (add-hook 'post-command-hook #'comet--trail-hook nil t))
    (remove-hook 'post-command-hook #'comet--trail-hook t)
    (comet--anim-stop)
    (comet-clear)
    (setq comet--animations nil)
    (setq comet--last-pos nil)))

(provide 'comet)

;;; comet.el ends here
