;;; ascii-art-to-unicode.el

;; This file is part of ttn's personal elisp library, released under
;; the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.  There is NO WARRANTY.  See file COPYING for details.

;;; Description: A small Artist mode adjunct.
;;; Version: 1.1

;;; Commentary:

;; The command `aa2u' converts simple ASCII art line drawings in
;; the {active,accessible} region of the current buffer to Unicode.
;;
;; Example use case:
;; - M-x artist-mode RET
;; - C-c C-a r               ; artist-select-op-rectangle
;; - (draw two rectangles)
;;
;;   +---------------+
;;   |               |
;;   |       +-------+--+
;;   |       |       |  |
;;   |       |       |  |
;;   |       |       |  |
;;   +-------+-------+  |
;;           |          |
;;           |          |
;;           |          |
;;           +----------+
;;
;; - C-c C-c                 ; artist-mode-off (optional)
;; - C-x n n                 ; narrow-to-region
;; - M-x aa2u RET
;;
;;   ┌───────────────┐
;;   │               │
;;   │       ┌───────┼──┐
;;   │       │       │  │
;;   │       │       │  │
;;   │       │       │  │
;;   └───────┼───────┘  │
;;           │          │
;;           │          │
;;           │          │
;;           └──────────┘
;;
;; TODO:
;; - Add phase 0, to grok and lock label (as opposed to line) text.
;; - Add interactive mode, to choose per-line light vs heavy.
;; - Improve neighbor-determining heuristic.
;; - Choose plus-replacement by composing "VERTICAL", "LEFT", etc.

;;; Code:

(require 'cl)
(require 'pcase)

(defun aa2u-phase-1 ()
  (goto-char (point-min))
  (while (search-forward "|" nil t)
    (replace-match "│" t t))
  (goto-char (point-min))
  (while (search-forward "-" nil t)
    (replace-match "─" t t)))

(defun aa2u-replacement (pos)
  (let ((cc (- pos (line-beginning-position))))
    (flet ((ns (name pos) (unless (= ?\s (char-syntax (char-after pos)))
                            name))
           (v (name dir) (let ((bol (line-beginning-position dir))
                               (eol (line-end-position dir)))
                           (when (< cc (- eol bol))
                             (ns name (+ bol cc)))))
           (h (name dir) (let ((bol (line-beginning-position))
                               (eol (line-end-position))
                               (pos (+ pos dir)))
                           (unless (or (> bol pos)
                                       (<= eol pos))
                             (ns name pos)))))
      (let* ((n (v 'n 0))
             (s (v 's 2))
             (w (h 'w -1))
             (e (h 'e  1)))
        (pcase (delq nil (list n s w e))
          (`(n s w e) #x253C) ;;; BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
          (`(s e)     #x250C) ;;; BOX DRAWINGS LIGHT DOWN AND RIGHT
          (`(s w)     #x2510) ;;; BOX DRAWINGS LIGHT DOWN AND LEFT
          (`(n e)     #x2514) ;;; BOX DRAWINGS LIGHT UP AND RIGHT
          (`(n w)     #x2518) ;;; BOX DRAWINGS LIGHT UP AND LEFT
          (`(n s e)   #x251C) ;;; BOX DRAWINGS LIGHT VERTICAL AND RIGHT
          (`(n s w)   #x2524) ;;; BOX DRAWINGS LIGHT VERTICAL AND LEFT
          (`(n w e)   #x2534) ;;; BOX DRAWINGS LIGHT UP AND HORIZONTAL
          (`(s w e)   #x252C) ;;; BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
          (_ #x003f))))))     ;;; QUESTION MARK

(defun aa2u-phase-2 ()
  (goto-char (point-min))
  (while (search-forward "+" nil t)
    (replace-match (string (aa2u-replacement (1- (point))))
                   nil t)))

;;;###autoload
(defun aa2u ()
  "Convert simple ASCII art line drawings to Unicode.
Specifically, perform the following replacements:

  - (hyphen)          U+2500   BOX DRAWINGS LIGHT HORIZONTAL
  | (vertical bar)    U+2502   BOX DRAWINGS LIGHT VERTICAL
  + (plus)            (one of)
                      U+253C   BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
                      U+250C   BOX DRAWINGS LIGHT DOWN AND RIGHT
                      U+2510   BOX DRAWINGS LIGHT DOWN AND LEFT
                      U+2514   BOX DRAWINGS LIGHT UP AND RIGHT
                      U+2518   BOX DRAWINGS LIGHT UP AND LEFT
                      U+251C   BOX DRAWINGS LIGHT VERTICAL AND RIGHT
                      U+2524   BOX DRAWINGS LIGHT VERTICAL AND LEFT
                      U+2534   BOX DRAWINGS LIGHT UP AND HORIZONTAL
                      U+252C   BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
                      U+003f   QUESTION MARK

More precisely, hyphen and vertical bar are substituted unconditionally,
first, and plus is substituted with a character depending on its north,
south, east and west neighbors.

This command operates on either the active region, as per
`use-region-p', or the accessible portion otherwise."
  (interactive)
  (save-excursion
    (flet ((do-it! () (aa2u-phase-1) (aa2u-phase-2)))
      (if (use-region-p)
          (let ((beg (region-beginning))
                (end (region-end)))
            (save-restriction
              (widen)
              (narrow-to-region beg end)
              (do-it!)))
        (do-it!)))))

(provide 'ascii-art-to-unicode)

;; ─    2500    BOX DRAWINGS LIGHT HORIZONTAL
;; ━    2501    BOX DRAWINGS HEAVY HORIZONTAL
;; │    2502    BOX DRAWINGS LIGHT VERTICAL
;; ┃    2503    BOX DRAWINGS HEAVY VERTICAL
;; ┄    2504    BOX DRAWINGS LIGHT TRIPLE DASH HORIZONTAL
;; ┅    2505    BOX DRAWINGS HEAVY TRIPLE DASH HORIZONTAL
;; ┆    2506    BOX DRAWINGS LIGHT TRIPLE DASH VERTICAL
;; ┇    2507    BOX DRAWINGS HEAVY TRIPLE DASH VERTICAL
;; ┈    2508    BOX DRAWINGS LIGHT QUADRUPLE DASH HORIZONTAL
;; ┉    2509    BOX DRAWINGS HEAVY QUADRUPLE DASH HORIZONTAL
;; ┊    250A    BOX DRAWINGS LIGHT QUADRUPLE DASH VERTICAL
;; ┋    250B    BOX DRAWINGS HEAVY QUADRUPLE DASH VERTICAL
;; ┌    250C    BOX DRAWINGS LIGHT DOWN AND RIGHT
;; ┍    250D    BOX DRAWINGS DOWN LIGHT AND RIGHT HEAVY
;; ┎    250E    BOX DRAWINGS DOWN HEAVY AND RIGHT LIGHT
;; ┏    250F    BOX DRAWINGS HEAVY DOWN AND RIGHT
;; ┐    2510    BOX DRAWINGS LIGHT DOWN AND LEFT
;; ┑    2511    BOX DRAWINGS DOWN LIGHT AND LEFT HEAVY
;; ┒    2512    BOX DRAWINGS DOWN HEAVY AND LEFT LIGHT
;; ┓    2513    BOX DRAWINGS HEAVY DOWN AND LEFT
;; └    2514    BOX DRAWINGS LIGHT UP AND RIGHT
;; ┕    2515    BOX DRAWINGS UP LIGHT AND RIGHT HEAVY
;; ┖    2516    BOX DRAWINGS UP HEAVY AND RIGHT LIGHT
;; ┗    2517    BOX DRAWINGS HEAVY UP AND RIGHT
;; ┘    2518    BOX DRAWINGS LIGHT UP AND LEFT
;; ┙    2519    BOX DRAWINGS UP LIGHT AND LEFT HEAVY
;; ┚    251A    BOX DRAWINGS UP HEAVY AND LEFT LIGHT
;; ┛    251B    BOX DRAWINGS HEAVY UP AND LEFT
;; ├    251C    BOX DRAWINGS LIGHT VERTICAL AND RIGHT
;; ┝    251D    BOX DRAWINGS VERTICAL LIGHT AND RIGHT HEAVY
;; ┞    251E    BOX DRAWINGS UP HEAVY AND RIGHT DOWN LIGHT
;; ┟    251F    BOX DRAWINGS DOWN HEAVY AND RIGHT UP LIGHT
;; ┠    2520    BOX DRAWINGS VERTICAL HEAVY AND RIGHT LIGHT
;; ┡    2521    BOX DRAWINGS DOWN LIGHT AND RIGHT UP HEAVY
;; ┢    2522    BOX DRAWINGS UP LIGHT AND RIGHT DOWN HEAVY
;; ┣    2523    BOX DRAWINGS HEAVY VERTICAL AND RIGHT
;; ┤    2524    BOX DRAWINGS LIGHT VERTICAL AND LEFT
;; ┥    2525    BOX DRAWINGS VERTICAL LIGHT AND LEFT HEAVY
;; ┦    2526    BOX DRAWINGS UP HEAVY AND LEFT DOWN LIGHT
;; ┧    2527    BOX DRAWINGS DOWN HEAVY AND LEFT UP LIGHT
;; ┨    2528    BOX DRAWINGS VERTICAL HEAVY AND LEFT LIGHT
;; ┩    2529    BOX DRAWINGS DOWN LIGHT AND LEFT UP HEAVY
;; ┪    252A    BOX DRAWINGS UP LIGHT AND LEFT DOWN HEAVY
;; ┫    252B    BOX DRAWINGS HEAVY VERTICAL AND LEFT
;; ┬    252C    BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
;; ┭    252D    BOX DRAWINGS LEFT HEAVY AND RIGHT DOWN LIGHT
;; ┮    252E    BOX DRAWINGS RIGHT HEAVY AND LEFT DOWN LIGHT
;; ┯    252F    BOX DRAWINGS DOWN LIGHT AND HORIZONTAL HEAVY
;; ┰    2530    BOX DRAWINGS DOWN HEAVY AND HORIZONTAL LIGHT
;; ┱    2531    BOX DRAWINGS RIGHT LIGHT AND LEFT DOWN HEAVY
;; ┲    2532    BOX DRAWINGS LEFT LIGHT AND RIGHT DOWN HEAVY
;; ┳    2533    BOX DRAWINGS HEAVY DOWN AND HORIZONTAL
;; ┴    2534    BOX DRAWINGS LIGHT UP AND HORIZONTAL
;; ┵    2535    BOX DRAWINGS LEFT HEAVY AND RIGHT UP LIGHT
;; ┶    2536    BOX DRAWINGS RIGHT HEAVY AND LEFT UP LIGHT
;; ┷    2537    BOX DRAWINGS UP LIGHT AND HORIZONTAL HEAVY
;; ┸    2538    BOX DRAWINGS UP HEAVY AND HORIZONTAL LIGHT
;; ┹    2539    BOX DRAWINGS RIGHT LIGHT AND LEFT UP HEAVY
;; ┺    253A    BOX DRAWINGS LEFT LIGHT AND RIGHT UP HEAVY
;; ┻    253B    BOX DRAWINGS HEAVY UP AND HORIZONTAL
;; ┼    253C    BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
;; ┽    253D    BOX DRAWINGS LEFT HEAVY AND RIGHT VERTICAL LIGHT
;; ┾    253E    BOX DRAWINGS RIGHT HEAVY AND LEFT VERTICAL LIGHT
;; ┿    253F    BOX DRAWINGS VERTICAL LIGHT AND HORIZONTAL HEAVY
;; ╀    2540    BOX DRAWINGS UP HEAVY AND DOWN HORIZONTAL LIGHT
;; ╁    2541    BOX DRAWINGS DOWN HEAVY AND UP HORIZONTAL LIGHT
;; ╂    2542    BOX DRAWINGS VERTICAL HEAVY AND HORIZONTAL LIGHT
;; ╃    2543    BOX DRAWINGS LEFT UP HEAVY AND RIGHT DOWN LIGHT
;; ╄    2544    BOX DRAWINGS RIGHT UP HEAVY AND LEFT DOWN LIGHT
;; ╅    2545    BOX DRAWINGS LEFT DOWN HEAVY AND RIGHT UP LIGHT
;; ╆    2546    BOX DRAWINGS RIGHT DOWN HEAVY AND LEFT UP LIGHT
;; ╇    2547    BOX DRAWINGS DOWN LIGHT AND UP HORIZONTAL HEAVY
;; ╈    2548    BOX DRAWINGS UP LIGHT AND DOWN HORIZONTAL HEAVY
;; ╉    2549    BOX DRAWINGS RIGHT LIGHT AND LEFT VERTICAL HEAVY
;; ╊    254A    BOX DRAWINGS LEFT LIGHT AND RIGHT VERTICAL HEAVY
;; ╋    254B    BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
;; ╌    254C    BOX DRAWINGS LIGHT DOUBLE DASH HORIZONTAL
;; ╍    254D    BOX DRAWINGS HEAVY DOUBLE DASH HORIZONTAL
;; ╎    254E    BOX DRAWINGS LIGHT DOUBLE DASH VERTICAL
;; ╏    254F    BOX DRAWINGS HEAVY DOUBLE DASH VERTICAL
;; ═    2550    BOX DRAWINGS DOUBLE HORIZONTAL
;; ║    2551    BOX DRAWINGS DOUBLE VERTICAL
;; ╒    2552    BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
;; ╓    2553    BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
;; ╔    2554    BOX DRAWINGS DOUBLE DOWN AND RIGHT
;; ╕    2555    BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
;; ╖    2556    BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
;; ╗    2557    BOX DRAWINGS DOUBLE DOWN AND LEFT
;; ╘    2558    BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
;; ╙    2559    BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
;; ╚    255A    BOX DRAWINGS DOUBLE UP AND RIGHT
;; ╛    255B    BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
;; ╜    255C    BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
;; ╝    255D    BOX DRAWINGS DOUBLE UP AND LEFT
;; ╞    255E    BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
;; ╟    255F    BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
;; ╠    2560    BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
;; ╡    2561    BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
;; ╢    2562    BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
;; ╣    2563    BOX DRAWINGS DOUBLE VERTICAL AND LEFT
;; ╤    2564    BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
;; ╥    2565    BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
;; ╦    2566    BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
;; ╧    2567    BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
;; ╨    2568    BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
;; ╩    2569    BOX DRAWINGS DOUBLE UP AND HORIZONTAL
;; ╪    256A    BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
;; ╫    256B    BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
;; ╬    256C    BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
;; ╭    256D    BOX DRAWINGS LIGHT ARC DOWN AND RIGHT
;; ╮    256E    BOX DRAWINGS LIGHT ARC DOWN AND LEFT
;; ╯    256F    BOX DRAWINGS LIGHT ARC UP AND LEFT
;; ╰    2570    BOX DRAWINGS LIGHT ARC UP AND RIGHT
;; ╱    2571    BOX DRAWINGS LIGHT DIAGONAL UPPER RIGHT TO LOWER LEFT
;; ╲    2572    BOX DRAWINGS LIGHT DIAGONAL UPPER LEFT TO LOWER RIGHT
;; ╳    2573    BOX DRAWINGS LIGHT DIAGONAL CROSS
;; ╴    2574    BOX DRAWINGS LIGHT LEFT
;; ╵    2575    BOX DRAWINGS LIGHT UP
;; ╶    2576    BOX DRAWINGS LIGHT RIGHT
;; ╷    2577    BOX DRAWINGS LIGHT DOWN
;; ╸    2578    BOX DRAWINGS HEAVY LEFT
;; ╹    2579    BOX DRAWINGS HEAVY UP
;; ╺    257A    BOX DRAWINGS HEAVY RIGHT
;; ╻    257B    BOX DRAWINGS HEAVY DOWN
;; ╼    257C    BOX DRAWINGS LIGHT LEFT AND HEAVY RIGHT
;; ╽    257D    BOX DRAWINGS LIGHT UP AND HEAVY DOWN
;; ╾    257E    BOX DRAWINGS HEAVY LEFT AND LIGHT RIGHT
;; ╿    257F    BOX DRAWINGS HEAVY UP AND LIGHT DOWN
;; ▀    2580    UPPER HALF BLOCK
;; ▁    2581    LOWER ONE EIGHTH BLOCK
;; ▂    2582    LOWER ONE QUARTER BLOCK
;; ▃    2583    LOWER THREE EIGHTHS BLOCK
;; ▄    2584    LOWER HALF BLOCK
;; ▅    2585    LOWER FIVE EIGHTHS BLOCK
;; ▆    2586    LOWER THREE QUARTERS BLOCK
;; ▇    2587    LOWER SEVEN EIGHTHS BLOCK
;; █    2588    FULL BLOCK
;; ▉    2589    LEFT SEVEN EIGHTHS BLOCK
;; ▊    258A    LEFT THREE QUARTERS BLOCK
;; ▋    258B    LEFT FIVE EIGHTHS BLOCK
;; ▌    258C    LEFT HALF BLOCK
;; ▍    258D    LEFT THREE EIGHTHS BLOCK
;; ▎    258E    LEFT ONE QUARTER BLOCK
;; ▏    258F    LEFT ONE EIGHTH BLOCK
;; ▐    2590    RIGHT HALF BLOCK
;; ░    2591    LIGHT SHADE
;; ▒    2592    MEDIUM SHADE
;; ▓    2593    DARK SHADE
;; ▔    2594    UPPER ONE EIGHTH BLOCK
;; ▕    2595    RIGHT ONE EIGHTH BLOCK
;; ▖    2596    QUADRANT LOWER LEFT
;; ▗    2597    QUADRANT LOWER RIGHT
;; ▘    2598    QUADRANT UPPER LEFT
;; ▙    2599    QUADRANT UPPER LEFT AND LOWER LEFT AND LOWER RIGHT
;; ▚    259A    QUADRANT UPPER LEFT AND LOWER RIGHT
;; ▛    259B    QUADRANT UPPER LEFT AND UPPER RIGHT AND LOWER LEFT
;; ▜    259C    QUADRANT UPPER LEFT AND UPPER RIGHT AND LOWER RIGHT
;; ▝    259D    QUADRANT UPPER RIGHT
;; ▞    259E    QUADRANT UPPER RIGHT AND LOWER LEFT
;; ▟    259F    QUADRANT UPPER RIGHT AND LOWER LEFT AND LOWER RIGHT

;;; ascii-art-to-unicode.el ends here
