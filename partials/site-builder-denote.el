;;; site-builder-denote.el --- exporting denote notes with site-builder  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  thibaut

;; Author: thibaut <thibaut@thibaut-Precision-5550>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun site-builder-denote-import ()
  (message "denote dir: %s" denote-directory)
  (let*
      ((default-directory denote-directory)
       (files (directory-files default-directory))
       (tocfile (make-temp-file "ToC" nil ".org"))
       (toc (save-excursion
              (find-file tocfile)
              (let ((default-directory denote-directory))
                (message "buffer-file-name :%s" (buffer-file-name))
                (dolist (f files)
                  (message "file: %s" (file-truename f))
                  (when (file-regular-p (file-truename f))
                    (message "file regular")
                    (insert "- ")
                    (denote-link (file-truename f))
                    (insert "\n"))))
              (buffer-substring-no-properties (point-min) (point-max)))))
    (delete-file tocfile)
    toc))

(provide 'site-builder-denote)
;;; site-builder-denote.el ends here
