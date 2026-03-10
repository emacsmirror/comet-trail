comet.el
========

Cursor comet trail effect for Emacs. When you move the cursor, a comet
of highlighted characters travels along the geometric line between the
old and new positions, then fades away.

Works with both keyboard and mouse movement. Correctly handles visual
line wrapping (`visual-line-mode`, `word-wrap`, `toggle-truncate-lines`).

Requires Emacs 27.1 or later.

<video src="https://git.andros.dev/andros/comet.el/media/branch/main/demo.mp4" autoplay muted loop controls></video>

How it works
------------

Given two buffer positions, the package computes their visual (column,
row) coordinates on screen, traces a line between them using
Bresenham's algorithm, maps each cell back to a buffer position, and
animates a comet sliding along that path with an ease-out brightness
gradient.

Installation
------------

### use-package with :vc (Emacs 29+)

Recommended method for Emacs 29 and later:

    (use-package comet
      :vc (:url "https://git.andros.dev/andros/comet.el"
           :rev :newest)
      :config
      (add-hook 'prog-mode-hook #'comet-trail-mode)
      (add-hook 'text-mode-hook #'comet-trail-mode))

### use-package with :load-path

For manual installation or Emacs < 29:

    (use-package comet
      :load-path "/path/to/comet.el"
      :config
      (add-hook 'prog-mode-hook #'comet-trail-mode)
      (add-hook 'text-mode-hook #'comet-trail-mode))

### Manual

Clone the repository and place the file in a directory on your `load-path`:

    git clone https://git.andros.dev/andros/comet.el.git

Then add to your `user-init-file`:

    (add-to-list 'load-path "/path/to/comet.el")
    (autoload 'comet-trail-mode "comet" nil t)

Or, to load it immediately at startup:

    (add-to-list 'load-path "/path/to/comet.el")
    (require 'comet)
    (add-hook 'prog-mode-hook #'comet-trail-mode)

Usage
-----

Run `M-x comet-trail-mode RET` to toggle the effect in the current
buffer. The mode indicator `Trail` appears in the mode line when
active.

Additional commands:

- `comet-draw`: Draw a static highlighted line between two positions
  (works on the active region with `M-x comet-draw`).
- `comet-clear`: Remove all comet overlays from the current buffer.

Customization
-------------

Run `M-x customize-group RET comet RET` to list all available options.

Key options:

- `comet-comet-length` (default `4`): Number of characters visible in
  the comet trail.
- `comet-comet-speed` (default `0.3`): Duration in seconds for the
  comet to traverse the full path.
- `comet-fade-exponent` (default `2.0`): Brightness falloff curve.
  `1.0` is linear, `2.0` is quadratic ease-out (recommended), `3.0`
  is cubic (sharper tail).
- `comet-minimum-distance` (default `2`): Minimum movement in cells to
  trigger an animation. Avoids visual noise on small cursor jumps.
- `comet-tick-interval` (default `0.016`): Seconds between animation
  frames (approximately 60fps).
- `comet-face` (default `comet-highlight`): Face used for the comet
  color. Set to `nil` to inherit the cursor color automatically.

The default highlight color is `#f0c674` (warm yellow) on dark themes
and `#c678dd` (purple) on light themes.

Contributing
------------

Contributions are welcome! Please see the [contribution guidelines](https://git.andros.dev/andros/contribute) for instructions on how to submit issues or pull requests.

License
-------

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.
