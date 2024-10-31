;;; literate-config.el --- Literate Emacs Config Using Org -*- lexical-binding:t -*-

;; Copyright (C) 2024 Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; Keywords: org-mode
;; Version: 0.0.1

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

;; Literate Emacs config using Org.

;;; Code:

(defvar literate-config-before-section-hook nil)

;;;###autoload
(defun literate-config-compile-config ()
  "This function will write all source blocks from =config.org= into =config.el= that:
- Are not marked as `tangle: no'
- Don't have the TODO state `DISABLED'
- Have a source-code of `emacs-lisp'"
  (require 'org)
  (let* ((body-list ())
         (output-file (concat user-emacs-directory "config.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (let ((inhibit-message t)) (message "Generating %s" output-file))
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat user-emacs-directory "config.org")
          (let* ((org_block_info (org-babel-get-src-block-info 'light))
                 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
                 (todo-keyword-match))
            (save-excursion
              (catch 'exit
                (org-back-to-heading t)
                (when (looking-at org-outline-regexp)
                  (goto-char (1- (match-end 0))))
                (when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
                  (setq todo-keyword-match (match-string 1)))))
            (unless (or (string= "no" tfile)
                        (string= "DISABLED" todo-keyword-match)
                        (not (string= "emacs-lisp" lang)))
              (add-to-list 'body-list (concat "\n\n;; #############################################################\n"
                                              ";; " (org-get-heading) "\n"
                                              ";; #############################################################\n\n"
                                              "(run-hook-with-args 'literate-config-before-section-hook \"" (org-get-heading) "\")\n"))
              (add-to-list 'body-list body)))))
      (with-temp-file output-file
        (insert ";;; config.el -*- lexical-binding: t; byte-compile-warnings: (not free-vars make-local noruntime); -*-\n")
        (insert ";; ============================================================\n")
        (insert ";; Don't edit this file, edit config.org instead.\n")
        (insert ";; Generated at " (format-time-string "%a %b %d %Y-%m-%dT%H:%M:%S " (current-time)) "on host " system-name "\n")
        (insert ";; ============================================================\n")
        (insert (apply 'concat (reverse body-list)))
        (insert "\n"))
      (let ((inhibit-message t)) (message "Wrote %s" output-file)))))

(provide 'literate-config)
;;; literate-config.el ends here
