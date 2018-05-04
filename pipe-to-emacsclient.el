;;; pipe-to-emacsclient.el --- pipe stdin to emacsclient

;; Copyright (C) 2018 by Koichi Osanai

;; Author: Koichi Osanai <osanai3@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; set PAGER environment variable
;; emacs --batch -l /path/to/pipe-to-emacsclient.el --eval='(pipe-to-emacsclient-batch)'

;; init.el
;; (require 'pipe-to-emacsclient)
;; (add-hook 'find-file-hook 'pipe-to-emacsclient-format)

;;; Code:

(require 'ansi-color)
(require 'man)

(defun pipe-to-emacsclient-batch ()
  (let ((tmpfilename (make-temp-file "pipe"))
        (mode-var "mode: pipe-to-emacsclient")
        (default-directory-var (format "default-directory: \"%s\"" default-directory))
        (buffer-name-var (format "pipe-to-emacsclient-buffer-name: \"*%s*\"" (or (getenv "PIPE_TO_EMACSCLIENT_COMMAND") "pipe"))))
    (append-to-file
     (format "-*- %s; %s; %s -*-\n" mode-var default-directory-var buffer-name-var)
     nil tmpfilename)
    (condition-case nil
        (let ((line ""))
          (while (setq line (read-string ""))
            (append-to-file (format "%s%s" line "\n") nil tmpfilename)))
      (error nil))
    (call-process "emacsclient" nil nil nil "-n" tmpfilename)))

(define-derived-mode pipe-to-emacsclient-mode fundamental-mode "Pipe")

(defvar pipe-to-emacsclient-buffer-name)

(defun pipe-to-emacsclient-format ()
  (when (eq major-mode 'pipe-to-emacsclient-mode)
    (ansi-color-apply-on-region (point-min) (point-max))
    (Man-fontify-manpage)
    (goto-char (point-min))
    (narrow-to-region (+ 1 (length (thing-at-point 'line t))) (point-max))
    (read-only-mode t)
    (set-buffer-modified-p nil)
    (rename-buffer pipe-to-emacsclient-buffer-name t)
    (set-visited-file-name nil)
    ))

(put 'pipe-to-emacsclient-buffer-name 'safe-local-variable 'stringp)

(provide 'pipe-to-emacsclient)

;;; pipe-to-emacsclient.el ends here
