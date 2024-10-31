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

(defvar literate-config-org-file-name (expand-file-name (concat user-emacs-directory "config.org")))
(defvar literate-config-el-file-name (expand-file-name (concat user-emacs-directory "config.el")))
(defvar literate-config-before-section-hook nil)

;;;###autoload
(defun literate-config-compile ()
  "Write all source blocks from =config.org= into =config.el= that:
- Are not marked as `tangle: no'
- Don't have the TODO state `DISABLED'
- Have a source-code of `emacs-lisp'"
  (require 'org)
  (let* ((body-list ())
         (output-file literate-config-el-file-name)
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (let ((inhibit-message t)) (message "Generating %s..." output-file))
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks literate-config-org-file-name
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
                                              "(run-hook-with-args 'literate-config-before-section-hook \"" (org-get-heading) "\")\n\n"))
              (add-to-list 'body-list body)))))
      (with-temp-file output-file
        (insert ";;; config.el -*- lexical-binding: t; byte-compile-warnings: (not free-vars make-local noruntime); -*-\n")
        (insert ";; ============================================================\n")
        (insert ";; Don't edit this file, edit org config instead.\n")
        (insert ";; Generated at " (format-time-string "%a %b %d %Y-%m-%dT%H:%M:%S " (current-time)) "on host " system-name "\n")
        (insert ";; ============================================================\n")
        (insert (apply 'concat (reverse body-list)))
        (insert "\n"))
      (let ((inhibit-message t)) (message "Wrote %s" output-file)))))

;;;###autoload
(defun literate-config-ensure-compiled ()
  "Compile the config if it does not exist or the source config has changed."
  (when (or (not (file-exists-p literate-config-el-file-name))
            (file-newer-than-file-p literate-config-org-file-name literate-config-el-file-name))
    (literate-config-compile)))

;;;###autoload
(defun literate-config-load ()
  "Load the config."
  (load literate-config-el-file-name nil 'nomessage 'nosuffix))

(defun literate-config-after-save-hook ()
  "Compile the config when saved."
  (when (string= literate-config-org-file-name buffer-file-name)
    (literate-config-compile)))

(defun literate-config-add-after-save-hook ()
  "Install hook that compiles the config on save."
  (add-hook 'after-save-hook 'literate-config-after-save-hook))

;;;###autoload
(defun literate-config-start ()
  "Compile, install hooks, and load the config."
  (literate-config-ensure-compiled)
  (literate-config-add-after-save-hook)
  (literate-config-load))

(provide 'literate-config)
;;; literate-config.el ends here
