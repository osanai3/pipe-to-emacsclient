;;; pipe-to-emacsclient.el --- pipe stdin to emacsclient

;; Copyright (C) 2018 by Koichi Osanai

;; Author: Koichi Osanai <osanai3@gmail.com>
;; Version: 0.2

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
;; "emacs --batch -l /path/to/pipe-to-emacsclient.el --eval='(pipe-to-emacsclient-batch)'"

;; init.el
;; (require 'pipe-to-emacsclient)

;;; Code:

(defun pipe-to-emacsclient-batch ()
  "Call emacsclient to show stdin."
  (let ((tmpfilename (make-temp-file "pipe"))
        (buffer-name (or (getenv "PIPE_TO_EMACSCLIENT_COMMAND") "pipe")))
    (with-temp-file tmpfilename
      (pipe-to-emacsclient-insert-stdin))
    (call-process "emacsclient" nil nil nil "--eval"
                  (format "%S" (list 'pipe-to-emacsclient-find-file tmpfilename buffer-name default-directory)))))

(defun pipe-to-emacsclient-insert-stdin ()
  "Insert stdin to current buffer."
  (condition-case nil
      (let ((line ""))
        (while (setq line (read-string ""))
          (insert (format "%s\n" line))))
    (error nil)))

(require 'ansi-color)
(require 'man)

;;;###autoload
(defun pipe-to-emacsclient-find-file (filename name directory)
  "Open FILENAME with buffer name NAME with 'default-directory' DIRECTORY and format buffer."
  (with-current-buffer (generate-new-buffer name)
    (setq default-directory directory)
    (insert-file-contents-literally filename)
    (ansi-color-apply-on-region (point-min) (point-max))
    (Man-fontify-manpage)
    (goto-char (point-min))
    (read-only-mode t)
    (set-buffer-modified-p nil)
    (select-window (display-buffer (current-buffer)))))

(provide 'pipe-to-emacsclient)

;;; pipe-to-emacsclient.el ends here
