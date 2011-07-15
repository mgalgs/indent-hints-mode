;;; indent-hints.el --- Get some hints about whether your buffer is
;;; space- or tab-loving

;; Copyright (C) 2011, Mitchel Humpherys

;; Author: Mitchel Humpherys <mitch.special@gmail.com>
;; Keywords: convenience
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; As the Eternal Holy War of tabs-versus-spaces rages on, even within
;; individual projects, an emacs minor mode arises from the depths of
;; github with the goal of easing the burden placed on the programmer
;; of trying to maintain consistency in text files.
;;
;; If you jump into a file that uses tabs for indentation, you shall
;; continue using tabs for indentation. If you jump into a file that
;; uses spaces for indentation, you shall continue using spaces for
;; indentation. That's the idea.
;;
;;; Installation:
;;
;; o Put indent-hints.el in your load path
;; o add the following line to your .emacs file:
;;
;;`     (require 'indent-hints)
;;
;; o You can then activate indent-hints mode in a buffer the usual
;;   way:
;;
;;`     (indent-hints-mode)
;;
;; o You can also use the globalized minor mode to enable
;;   indent-hints-mode in all buffers:
;;
;;`     (indent-hints-global-mode)
;;
;;; Use:
;;
;; Just check out your mode-line to see whether the buffer you're
;; visiting is space-loving or tab-loving. It also shows the ratio of
;; space-to-tab (or tab-to-space, whichever your buffer loves)
;; loving-ness that your current buffer exudes. Here's a "screenshot":
;;
;;`     test.el Top -- (Emacs-Lisp Tab-loving:0.53 yas pair)--etc. etc.--
;;
;; The file being visited in the "screenshot" has more tabs than
;; spaces (53% of the lines that start with some sort of indentation
;; start with tabs, to be exact).
;;
;;; TODO:
;;
;; o Add some helper functions to switch between tabs/spaces profiles.
;;
;;; Code

(define-minor-mode indent-hints-mode
  "Give user hints about whether this buffer is space- or tab-loving."
  nil "" nil
  (if indent-hints-mode
      (let* ((vals (count-line-beginnings))
             (begin-with-tab (nth 0 vals))
             (begin-with-space (nth 1 vals))
             (begin-with-something-else (nth 2 vals)))
        (message "%s: %d lines, %d start with tabs, %d start with spaces"
                 (buffer-name) begin-with-something-else
                 begin-with-tab begin-with-space)
        (if (> begin-with-space begin-with-tab)
            (let ((space-ratio
                   (/ (float begin-with-space) (+ begin-with-space begin-with-tab))))
              (setq space-loving t)
              (setq tab-loving nil)
              (update-space-loving-ratio space-ratio))
          ;; else
          (let ((tab-ratio
                 (/ (float begin-with-tab) (+ begin-with-space begin-with-tab))))
            (setq tab-loving t)
            (setq space-loving nil)
            (update-tab-loving-ratio tab-ratio)))) ; eo let,if,let*
    ;; else, the mode was disabled:
    (progn
      (setq space-loving nil tab-loving nil)
      (message "indent-hints-mode disabled!"))))


;;; Helper functions
;;

(defun count-line-beginnings ()
  "The real meat. Examine the first character of each line in the
buffer. This can be used to determine if a buffer is space-loving
or tab-loving. Returns a list of the
form: (num-beginning-with-tab num-beginning-with-space
num-beginning-with-something-else)"
  (interactive)
  (save-excursion
    (let ((begin-with-tab 0)
          (begin-with-space 0)
          (begin-with-something-else 0)
          (current-line-number 1))
      (goto-char 1)
      (while (char-after (point))
        (cond ((= 32 (char-after (point)))
               (setq begin-with-space (1+ begin-with-space)))
              ((= 9 (char-after (point)))
               (setq begin-with-tab (1+ begin-with-tab)))
              (t (setq begin-with-something-else (1+ begin-with-something-else))))
        (setq current-line-number (1+ current-line-number))
        (forward-line 1)
        ) ;; eo while
      (list begin-with-tab begin-with-space begin-with-something-else)
      ) ;; eo let
    ) ;; eo save-excursion
  ) ;; eo defun

;; global variable to keep track of whether or not we've done global
;; activation of indent-hints-mode:
(setq indent-hints-did-global-activation nil)

(defun indent-hints-global-activate ()
  "Sets up the minor-mode-alist and buffer-local variable for indentation hints"
  (message "doing global-activate globact: %S" indent-hints-did-global-activation)
  (setq minor-mode-alist (cons '(space-loving " Space-loving")
                               (cons '(tab-loving " Tab-loving")
                                     minor-mode-alist)))
  (setq space-loving nil tab-loving nil)
  (make-variable-buffer-local 'space-loving)
  (make-variable-buffer-local 'tab-loving)
  (setq indent-hints-did-global-activation t))

(setq love-sep ":")

(defun update-space-loving-ratio (ratio)
  "Update the of space-loving-ness shown in the mode line"
  (interactive)
  (let ((newval (concat " Space-loving" love-sep (format "%.2f" ratio))))
       (setq minor-mode-alist
             (cons (list 'space-loving newval)
                   (assq-delete-all 'space-loving minor-mode-alist)))))

(defun update-tab-loving-ratio (ratio)
  "Update the of tab-loving-ness shown in the mode line"
  (interactive)
  (let ((newval (concat " Tab-loving" love-sep (format "%.2f" ratio))))
       (setq minor-mode-alist
             (cons (list 'tab-loving newval)
                   (assq-delete-all 'tab-loving minor-mode-alist)))))

(defun indent-hints-mode-on ()
  "Turns on indent-hints-mode, if appropriate.
This function is intended to be used with define-globalized-minor-mode"
  (unless indent-hints-did-global-activation (indent-hints-global-activate))
  (unless (or indent-hints-mode (minibufferp) (is-temp-buffer (buffer-name)))
    (indent-hints-mode 1)))

(defun is-temp-buffer (the-buffer-name)
  "Returns true if given buffer name is a temp buffer (starts with \" *\")"
  (string= (substring (car (split-string the-buffer-name)) 0 1) "*"))

(define-globalized-minor-mode indent-hints-global-mode indent-hints-mode indent-hints-mode-on
  :group 'indent-hints
  :require 'indent-hints)

(provide 'indent-hints)
;;; indent-hints.el ends here
;;
