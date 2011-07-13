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
;;     (require 'indent-hints)
;; 
;; You can then activate indent-hints mode in a buffer the usual way:
;;     (indent-hints-mode)
;; 
;; You can also use the globalized minor mode to enable
;; indent-hints-mode in all buffers:
;;     (indent-hints-global-mode)
;; 
;;; Use:
;; 
;; Look at your mode-line to see whether the buffer you're visiting is
;; space-loving or tab-loving.
;; 
;; TODO:
;; 
;; o Add some helper functions to switch between tabs/spaces profiles.
;; o Actually disable the mode (clean up the minor-mode-alist) on disable.
;; 
;;; Code

(define-minor-mode indent-hints-mode
  "Give user hints about whether this buffer is space- or tab-loving."
  nil "" nil
  (cond (indent-hints-mode
         (message "processing indentation hints for buffer %s" (buffer-name))
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
             (message "%d start with tabs, %d start with spaces, %d with something else" begin-with-tab begin-with-space begin-with-something-else)
             (if (> begin-with-space begin-with-tab)
                 (setq space-loving t tab-loving nil)
               (setq tab-loving t space-loving nil))
             ) ;; eo let
           ) ;; eo save-excursion
         ) ;; eo cond indent-hints-mode
        (t
         (setq space-loving nil tab-loving nil)
         (message "indent-hints-mode disabled!")))) ;; eo cond, define-minor-mode



;;; Helper functions
;;

;; global variable to keep track of whether or not we've done global
;; activation of indent-hints-mode:
(setq indent-hints-did-global-activation nil)

(defun indent-hints-global-activate ()
  "Sets up the minor-mode-alist and buffer-local variable for indentation hints"
  (message "doing global-activate globact: %S" indent-hints-did-global-activation)
  (setq minor-mode-alist (cons '(space-loving " [Space-loving]")
                               (cons '(tab-loving " [Tab-loving]")
                                     minor-mode-alist)))
  (setq string-loving nil tab-loving nil)
  (make-variable-buffer-local 'space-loving)
  (make-variable-buffer-local 'tab-loving)
  (setq indent-hints-did-global-activation t))


(defun indent-hints-mode-on ()
  "Turns on indent-hints-mode, if appropriate.
This function is intended to be used with define-globalized-minor-mode"
  (unless indent-hints-did-global-activation (indent-hints-global-activate))
  (unless (or indent-hints-mode (minibufferp))
    (indent-hints-mode 1)))

(define-globalized-minor-mode indent-hints-global-mode indent-hints-mode indent-hints-mode-on
  :group 'indent-hints
  :require 'indent-hints)

(provide 'indent-hints)
;;; indent-hints.el ends here
;;
