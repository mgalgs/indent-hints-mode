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
;; TODO
;; 
;;; Use:
;; 
;; TODO
;; 
;;; Code

(define-minor-mode indent-hints-mode
  "Give user hints about whether this buffer is space- or tab-loving."
  nil " indent-hints" nil
  (cond (indent-hints-mode
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
               (next-line)
               ) ;; eo while
             ; (message "%d start with tabs, %d start with spaces, %d with something else" begin-with-tab begin-with-space begin-with-something-else)
             (if (> begin-with-space begin-with-tab)
                 (message "Looks like this buffer is space-loving")
               (message "Looks like this buffer is tab-loving"))
             ) ;; eo let
           ) ;; eo save-excursion
         ) ;; eo indent-hints-mode
        (t
         (message "indent-hints-mode disabled!")))) ;; eo cond, define-minor-mode

(provide 'indent-hints-mode)
;;; indent-hints.el ends here
;;
