;;; haskell-mode-expansions.el --- haskell-specific expansions for expand-region

;; Copyright (C) 2011 Magnar Sveen

;; Author: Markus Hauck
;; Keywords: marking region

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


;; Expansions:
;;
;;   er/haskell-mark-where-clause-inner
;;   er/haskell-mark-where-clause
;;   er/haskell-mark-declaration-after-do-before-where
;;   er/haskell-mark-declaration-after-do
;;   er/haskell-mark-declaration-right-before-where
;;   er/haskell-mark-declaration-right
;;   er/haskell-mark-declaration
;;   er/haskell-mark-declaration-with-type
;;

;;; Code:

(require 'expand-region-core)

(require 'haskell-decl-scan)

(defun er/haskell-mark-declaration-right ()
  "Mark the body after the equals sign of the declaration."
  (interactive)
  (er/haskell-goto-declaration-equals)
  (set-mark-command nil)
  (er/haskell-goto-declaration-end)
  (exchange-point-and-mark))

(defun er/haskell-mark-declaration-after-do ()
  "Mark the body of a do block."
  (interactive)
  (er/haskell-goto-declaration-end)
  (set-mark-command nil)
  (re-search-backward "\\bdo\\b"))

(defun er/haskell-mark-declaration-after-do-before-where ()
  "Mark the body of a do but do block not including where clauses."
  (interactive)
  (er/haskell-mark-declaration-after-do)
  (exchange-point-and-mark)
  (re-search-backward "where")
  (beginning-of-line)
  (forward-char -1)
  (exchange-point-and-mark))

(defun er/haskell-mark-declaration-right-before-where ()
  "Mark the body after the equals sing of the declaration and before a where clause."
  (interactive)
  (er/haskell-mark-declaration-right)
  (exchange-point-and-mark)
  (re-search-backward "where")
  (beginning-of-line)
  (forward-char -1)
  (exchange-point-and-mark))

(defun er/haskell-mark-declaration ()
  "Mark the complete declaration omitting the type annotation."
  (interactive)
  (haskell-ds-backward-decl)
  (er/haskell-goto-declaration-equals)
  (beginning-of-line)
  (set-mark-command nil)
  (er/haskell-goto-declaration-end)
  (exchange-point-and-mark))

(defun er/haskell-mark-declaration-with-type ()
  "Mark the complete declaration with the type annotation."
  (interactive)
  (er/haskell-mark-declaration)
  (forward-line -1))

(defun er/haskell-mark-where-clause-inner ()
  "Mark the body of a where clause."
  (interactive)
  (er/haskell-mark-where-clause)
  (forward-sexp))

(defun er/haskell-mark-where-clause ()
  "Mark a complete where clause."
  (interactive)
  (let ((start (haskell-ds-backward-decl))
        (end (haskell-ds-forward-decl)))
    (goto-char start)
    (search-forward-regexp "\\bwhere\\b" end)
    (back-to-indentation)
    (set-mark (point))
    (er/haskell-goto-end-of-indentation (current-column))
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun er/haskell-goto-end-of-indentation (goal-column)
  "Go down line until the indentation is less than (as GOAL-COLUMN)."
  (interactive)
  (while (and (not (progn
                     (end-of-line)
                     (eq (point) (point-max))))
              (or (>= (current-column) goal-column)
                  (looking-at "\n")))
    (forward-line 1)
    (back-to-indentation))
  (re-search-backward ".")
  (end-of-line))

(defun er/haskell-goto-declaration-equals ()
  "Go to the equals sign of the function declaration."
  (interactive)
  (haskell-ds-backward-decl)
  (re-search-forward "=")
  (forward-sexp)
  (backward-sexp)
  (point))

(defun er/haskell-goto-declaration-end ()
  "Go to the end of the current declaration."
  (interactive)
  (haskell-ds-backward-decl)
  (forward-line 1)
  (when (not (eq (point) (point-max)))
    (forward-char 1))
  (er/haskell-goto-end-of-indentation 1)
  (point))

(defun er/add-haskell-mode-expansions ()
  "Add Haskell-specific expansions for buffers in haskell-mode."
  (set (make-local-variable 'er/try-expand-list)
       (append (remove 'er/mark-defun er/try-expand-list)
               '(;;er/haskell-mark-where-clause-inner
                   er/haskell-mark-where-clause
                   er/haskell-mark-declaration-right
                   er/haskell-mark-declaration
;;                 er/haskell-mark-declaration-after-do-before-where
;;                 er/haskell-mark-declaration-after-do
;;                 er/haskell-mark-declaration-right-before-where


                 er/haskell-mark-declaration-with-type
                 mark-paragraph))))

(er/enable-mode-expansions 'haskell-mode 'er/add-haskell-mode-expansions)

(provide 'haskell-mode-expansions)
;;; haskell-mode-expansions.el ends here
